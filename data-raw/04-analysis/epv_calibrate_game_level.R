#!/usr/bin/env Rscript
# Game-level EPV calibration: points of ACTUAL margin per unit of each channel
# ===========================================================================
# The sibling of data-raw/04-analysis/epv3_calibrate_4ch.R, and deliberately the
# same procedure -- fit per channel, apply, refit, require 1.000 -- but pointed
# at a different layer and a different target.
#
#   epv3_calibrate_4ch.R : EPR difference   -> margin   ("what will happen")
#   this script          : per-game team EPV -> margin   ("what happened")
#
# WHY BOTH EXIST. The two answer different questions and cannot share one
# constant. EPR is per-game EPV after decay, Bayesian shrinkage and position
# centring -- a different aggregation, not a rescaling -- so a coefficient of
# 1.000 at the EPR layer does not imply 1.000 per match. Measured 2026-09-05:
# per-match team dEPV runs about 2x margin points while the EPR layer reads
# 1.000, and that gap is structural, not drift (per-season slope 0.398-0.427
# across six seasons).
#
# WHICH COLUMNS. The raw `epv_*` columns -- the ones published in
# player_game_ratings and displayed by the blog. Note these are NOT the columns
# EPV3_POINTS_SCALE touches: centre_epv_by_position() applies that scale to the
# `_adj` variants only (verified 2026-09-05: `_adj` scales at 3.6e-15, raw
# columns unchanged at exactly 0). So the display column currently carries no
# points calibration at all, and this script supplies it. EPR consumes
# `_oadj`/`_adj` and is therefore untouched by anything here -- by construction,
# not by luck.
#
# WHAT THIS CANNOT DELIVER. "Team dEPV equals the margin" is not purchasable:
# R^2 is ~0.71, so ~30% of margin variance is not in EPV at all, and there is a
# ~4.9-point home-ground term EPV does not carry. Expect the anchored quantity
# to be right ON AVERAGE with a ~16-point median gap in any single game.

suppressMessages({
  library(data.table)
})
devtools::load_all(quiet = TRUE)

OUT_DIR <- Sys.getenv("EPV_CAL_OUT", unset = tempdir())
say <- function(...) cat(..., "\n", sep = "")

CH  <- c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout")
LBL <- c(epv_recv = "recv", epv_disp = "disp",
         epv_spoil = "cont_aerial", epv_hitout = "cont_stop")
# Slot meanings under v3: the COLUMN names are v2 leftovers. `spoil` holds
# aerial contest value and `hitout` holds ruck/stoppage.
PRETTY <- c(epv_recv = "winning it", epv_disp = "using it",
            epv_spoil = "aerial contests", epv_hitout = "ruck")

# ---- data -----------------------------------------------------------------
pg  <- as.data.table(load_player_game_ratings(seasons = TRUE))
res <- as.data.table(load_results(TRUE))[!is.na(home_score) & !is.na(away_score)]
res[, margin := home_score - away_score]
xg  <- tryCatch(as.data.table(load_xg(TRUE)), error = function(e) NULL)

tgt <- res[, .(match_id = as.character(match_id), season, margin)]
if (!is.null(xg) && "xscore_diff" %in% names(xg)) {
  tgt <- merge(tgt, xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
               by = "match_id", all.x = TRUE)
} else {
  tgt[, xmargin := NA_real_]
}

#' Team-level channel differences for one match, home minus away
build_diffs <- function(pgd, results) {
  ts <- pgd[, lapply(.SD, function(v) sum(v, na.rm = TRUE)),
            by = .(match_id, team), .SDcols = CH]
  mm <- merge(ts, results[, .(match_id = as.character(match_id),
                              home_team = home_team_name,
                              away_team = away_team_name)], by = "match_id")
  h <- mm[team == home_team]; a <- mm[team == away_team]
  d <- merge(h[, c("match_id", CH), with = FALSE],
             a[, c("match_id", CH), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH) d[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  d[, c("match_id", paste0("d_", CH)), with = FALSE]
}

fit_channels <- function(d, target) {
  m <- merge(d, tgt, by = "match_id")
  m <- m[is.finite(get(target))]
  # Through the origin, as epv3_calibrate_4ch.R does: a per-player value metric
  # has nowhere to put an intercept, so the home-ground term must stay OUT of
  # the coefficients rather than be silently smeared across the channels.
  f <- lm(as.formula(paste(target, "~ 0 +", paste0("d_", CH, collapse = " + "))), data = m)
  co <- summary(f)$coefficients
  list(coef = setNames(co[, 1], sub("^d_", "", rownames(co))),
       t    = setNames(co[, 3], sub("^d_", "", rownames(co))),
       sd   = vapply(CH, function(v) sd(m[[paste0("d_", v)]]), numeric(1)),
       r2   = summary(f)$r.squared, n = nrow(m), data = m)
}

report <- function(fc, label) {
  say("")
  say("=== ", label, " ===")
  print(data.table(
    channel   = unname(PRETTY[names(fc$coef)]),
    slot      = unname(LBL[names(fc$coef)]),
    coef      = round(fc$coef, 5),
    t         = round(fc$t, 2),
    sd_units  = round(fc$sd, 3),
    var_share = round(100 * fc$sd^2 / sum(fc$sd^2), 1)), row.names = FALSE)
  say("n = ", fc$n, "   R^2 = ", round(fc$r2, 4))
}

say("=== GAME-LEVEL EPV CALIBRATION ===")
say("run at ", format(Sys.time()))
say("EPV_ENGINE = ", EPV_ENGINE, " | EPV3_CHANNELS = ", EPV3_CHANNELS)

d0 <- build_diffs(pg, res)
say("\nCOVERAGE: ", nrow(d0), " matches with both teams; seasons ",
    paste(range(tgt[match_id %in% d0$match_id]$season), collapse = "-"))

# ---- A. unscaled fit ------------------------------------------------------
fa_m <- fit_channels(d0, "margin")
report(fa_m, "A. UNSCALED: points of ACTUAL margin per unit of each channel")
if (any(is.finite(tgt$xmargin))) {
  fa_x <- fit_channels(d0, "xmargin")
  report(fa_x, "A2. same, against xmargin (the quieter target)")
}

# ---- B. apply and verify --------------------------------------------------
# Section B is the test, exactly as in the EPR-layer script: after applying the
# fitted coefficients every identifiable channel must read 1.000. dMAE cannot
# see a units error -- both scales predict equally well -- so this is the only
# check that can.
k <- fa_m$coef
say("\nProposed EPV_GAME_POINTS_SCALE:")
print(round(setNames(k, unname(LBL[names(k)])), 6))

d1 <- copy(d0)
for (v in CH) d1[, (paste0("d_", v)) := get(paste0("d_", v)) * k[[v]]]
fb <- fit_channels(d1, "margin")
report(fb, "B. APPLIED: every identifiable channel must read 1.000")

ok <- abs(fb$coef - 1) < 1e-6
say("\nSection B verdict: ", if (all(ok)) "PASS -- all channels read 1.000"
    else paste0("FAIL -- ", sum(!ok), " channel(s) off 1.000"))

# ---- C. per season, because pooled hides drift ----------------------------
say("\n=== C. per-season check on the APPLIED scale ===")
m1 <- merge(d1, tgt, by = "match_id")
m1[, dv := rowSums(.SD), .SDcols = paste0("d_", CH)]
for (s in sort(unique(m1$season))) {
  x <- m1[season == s]
  if (nrow(x) < 30) next
  f <- lm(margin ~ 0 + dv, data = x)
  say(sprintf("  %s: slope %.4f  sd(dv) %6.2f  sd(margin) %6.2f  ratio %.4f  n %d",
              s, coef(f)[["dv"]], sd(x$dv), sd(x$margin),
              sd(x$margin) / sd(x$dv), nrow(x)))
}

# ---- D. what it does NOT buy ---------------------------------------------
say("\n=== D. residual gap after calibration (state this, do not bury it) ===")
m1[, fitted := dv]
say(sprintf("  median |dEPV - margin| = %.1f points", median(abs(m1$fitted - m1$margin))))
say(sprintf("  R^2 = %.4f, so %.0f%% of margin variance is not in EPV at all",
            fb$r2, 100 * (1 - fb$r2)))
f_int <- lm(margin ~ dv, data = m1)
say(sprintf("  home-ground intercept the channels cannot carry: %.2f points",
            coef(f_int)[["(Intercept)"]]))

saveRDS(list(fit_unscaled = fa_m, fit_applied = fb, scale = k),
        file.path(OUT_DIR, "epv_game_calibration.rds"))
say("\nSaved to ", file.path(OUT_DIR, "epv_game_calibration.rds"))
say("=== DONE ===")
