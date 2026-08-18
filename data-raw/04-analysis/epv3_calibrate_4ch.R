# v3 points calibration on the FOUR-channel structure (EPV3_CHANNELS = 4L,
# set 2026-08-18).
#
# Why a separate script from epv3_calibrate_final.R: that one calibrates the
# THREE-channel structure, and its stages B and C hard-set EPV3_CHANNELS = 3L.
# Its whole job is to fit a sub-scale that merges the two contest components into
# one slot. Under four channels there is no merge, so there is no sub-scale and
# no stage B -- each channel is fitted, applied and verified directly.
#
# The merge is what this replaces, and why:
#   - aerial contests and ruck are ORTHOGONAL (cor 0.004), so merging destroys
#     information rather than removing duplication;
#   - ruck is the most predictive channel per sd in the rating, while aerial is
#     redundant (p = 0.954) with roughly five times the spread. Summed over 22
#     players, aerial noise buries the ruck signal from one or two;
#   - measured 2026-08-18: ruck reads t = 4.39 standalone, t = 0.13 merged. A
#     channel at t = 0.13 has no measurable points-per-unit, so the calibration
#     literally cannot pass while the merge is in place.
#
# Stages:
#   A  fit   -- one coefficient per channel: points of margin per unit of EPR.
#   B  apply -- EPV3_POINTS_SCALE = A, every EPR prior rate carrying the same
#               factor, then REBUILD and refit. Each channel must return 1.000.
#
# Only channels with identifiable signal (|t| >= 2) are judged. The rest are
# reported, never silently exempted, and the tolerance is never widened to make
# one pass.
suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_calibrate_4ch.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(...) {
  vals <- list(...)
  for (nm in names(vals)) assignInNamespace(nm, vals[[nm]], ns = "torp")
}

say("=== v3 points calibration, FOUR channels ===")
say("run at ", format(Sys.time()))
say("EPV3_CHANNELS = ", EPV3_CHANNELS, " | EPV_RECV_NEG_MULT = ", EPV_RECV_NEG_MULT)
stopifnot(identical(EPV3_CHANNELS, 4L))

pbp    <- load_pbp(TRUE)
stats_ <- load_player_stats(TRUE)
teams  <- load_teams(TRUE)
chains <- load_chains(TRUE)
shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res <- as.data.table(load_results(TRUE))
xg  <- as.data.table(load_xg(TRUE))
tgt <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]
say("target matches: ", nrow(tgt))

build_pgd <- function(tag) {
  f <- file.path(OUT_DIR, paste0("epv3_cal_pgd_", tag, ".parquet"))
  if (file.exists(f)) {
    cli::cli_alert_info("Reusing cached pgd {tag}")
    d <- as.data.table(read_parquet(f))
  } else {
    d <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_engine = "v3"))
    write_parquet(d, f)
  }
  setattr(d, "epv_engine", "v3")
  d
}

build_ratings <- function(pgd, tag) {
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v3")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    out <- calculate_torp(out, psr_df)
  }
  out
}

CH4 <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
# Slot -> meaning. The COLUMN names are v2 leftovers: under v3 the `spoil` slot
# holds aerial contests and the `hitout` slot holds ruck/stoppage. Reported with
# the v3 meanings so nobody reads a v2 number into them.
PRETTY <- c(epr_recv = "winning it", epr_disp = "using it",
            epr_spoil = "aerial contests", epr_hitout = "ruck")
LBL <- c(epr_recv = "recv", epr_disp = "disp",
         epr_spoil = "cont_aerial", epr_hitout = "cont_stop")

fit_channels <- function(rt, chans, target = "xmargin") {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", chans), with = FALSE],
             a[, c("match_id", chans), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in chans) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")
  f <- lm(as.formula(paste(target, "~ 0 +", paste0("d_", chans, collapse = " + "))), data = m)
  co <- summary(f)$coefficients
  list(coef = setNames(co[, 1], sub("^d_", "", rownames(co))),
       t = setNames(co[, 3], sub("^d_", "", rownames(co))),
       sd = vapply(chans, function(v) sd(m[[paste0("d_", v)]]), numeric(1)),
       n = nrow(m))
}

report <- function(fc, label) {
  say("")
  say("=== ", label, " ===")
  say_dt(data.table(channel = unname(PRETTY[names(fc$coef)]),
                    slot = names(fc$coef),
                    coef = round(fc$coef, 5), t = round(fc$t, 2),
                    sd_points = round(fc$sd, 3),
                    var_share_pct = round(100 * fc$sd^2 / sum(fc$sd^2), 1)), 6)
}

# ---- A. fit ---------------------------------------------------------------
set_const(EPV3_CHANNELS = 4L,
          EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
          EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
          EPR_PRIOR_RATE_SPOIL = -0.3, EPR_PRIOR_RATE_HITOUT = -0.3)
pgd4 <- build_pgd("4ch_cal")
rt4  <- build_ratings(pgd4, "4ch")
fa   <- fit_channels(rt4, CH4)
report(fa, "A. unscaled: points of margin per unit of each channel")

pts <- setNames(as.numeric(fa$coef[CH4]), unname(LBL[CH4]))
say("")
say("EPV3_POINTS_SCALE <- c(", paste(sprintf("%s = %.6f", names(pts), pts), collapse = ", "), ")")

# ---- B. apply and verify --------------------------------------------------
set_const(EPV3_POINTS_SCALE = pts,
          EPR_PRIOR_RATE_RECV = -0.7 * pts[["recv"]],
          EPR_PRIOR_RATE_DISP = -0.7 * pts[["disp"]],
          EPR_PRIOR_RATE_SPOIL = -0.3 * pts[["cont_aerial"]],
          EPR_PRIOR_RATE_HITOUT = -0.3 * pts[["cont_stop"]])
rt4c <- build_ratings(pgd4, "4ch_final")
fc   <- fit_channels(rt4c, CH4)
report(fc, "B. applied: every identifiable channel must read 1.000")

.ident   <- abs(fc$t) >= 2
.judged  <- names(fc$coef)[.ident]
.skipped <- names(fc$coef)[!.ident]
say("")
say("identifiable (|t| >= 2): ",
    if (length(.judged)) paste(unname(PRETTY[.judged]), collapse = ", ") else "NONE")
if (length(.skipped)) {
  say("NOT JUDGED, no identifiable signal: ",
      paste(sprintf("%s (t = %.2f, %.1f%% of variance)", unname(PRETTY[.skipped]),
                    fc$t[.skipped], 100 * fc$sd[.skipped]^2 / sum(fc$sd^2)),
            collapse = "; "))
}
say("VERDICT: ", if (!length(.judged)) "NOT MET - nothing identifiable"
    else if (max(abs(fc$coef[.judged] - 1)) < 5e-3) "MET" else "NOT MET",
    "  (on ", length(.judged), " identifiable channel", if (length(.judged) == 1) "" else "s", ")")

say("")
say("--- cross-check on ACTUAL margin (xmargin is the quieter target) ---")
fm <- fit_channels(rt4c, CH4, target = "margin")
say_dt(data.table(channel = unname(PRETTY[names(fm$coef)]),
                  coef = round(fm$coef, 4), t = round(fm$t, 2)), 6)

say("")
say("--- THE POINT OF THE EXERCISE: did ruck survive? ---")
say("merged (3 channels, 2026-08-18): contest coef 0.0102, t 0.13")
say(sprintf("four channels:                   ruck    coef %.4f, t %.2f",
            fc$coef[["epr_hitout"]], fc$t[["epr_hitout"]]))

saveRDS(list(points_scale = pts, fit_unscaled = fa, fit_applied = fc,
             fit_margin = fm, rt4 = rt4, rt4c = rt4c),
        file.path(OUT_DIR, "epv3_calibrate_4ch.rds"))
say("")
say("done ", format(Sys.time()))
close(con)
cat("\nDone\n")
