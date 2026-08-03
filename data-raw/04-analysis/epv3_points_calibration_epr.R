# Points calibration at the EPR layer, the way production actually does it.
#
# My first attempt calibrated at the EPV layer -- team-summed epv_*_adj for the
# game against that game's actual margin. That is a DESCRIPTIVE statement ("a
# player who accrued one unit in this game contributed one point to this
# margin") and it is not what production means by EPV_POINTS_SCALE. That
# constant is documented as "regressing actual margin on the team rating
# difference: EPR converted at 0.919" -- the EPR layer, i.e. PREDICTIVE ("a team
# rated one point higher wins by one more point").
#
# The two give very different numbers because they are different quantities:
#   EPV layer   recv 0.599  disp 0.612  cont 0.260
#   EPR layer   recv 2.19   disp 1.40   cont -0.02
# Same trap as the retracted contest-coefficient finding earlier tonight. Always
# say which layer.
#
# THIS SCRIPT DOES IT PROPERLY:
#   * EPR of the NAMED LINEUP (emergencies excluded), TOG-weighted by
#     POSITION_AVG_TOG -- via .build_team_ratings_df(), the production builder,
#     so there is no second implementation to drift
#   * target xmargin (xscore_diff), which strips shot-conversion luck that no
#     rating should be asked to predict, at a cost of only ~20% power
#   * actual margin reported alongside as the cross-check
#
# The resulting constants are directly comparable to the 0.919 that is live.
#
# PERFORMANCE: two team-feature builds and a handful of linear fits. ~2 minutes.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_points_calibration_epr.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Points calibration at the EPR layer (production's convention) ===")

teams  <- load_teams(TRUE)
res    <- as.data.table(load_results(TRUE))
xg     <- as.data.table(load_xg(TRUE))
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)

tgt <- merge(
  res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
  xg[,  .(match_id = as.character(match_id), xmargin = xscore_diff)],
  by = "match_id", all.x = TRUE)
tgt <- tgt[is.finite(margin)]
say("matches with a margin: ", nrow(tgt),
    " | with xmargin: ", sum(is.finite(tgt$xmargin)))

CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")

build_diffs <- function(rt_file, label) {
  rt <- as.data.frame(arrow::read_parquet(file.path(OUT_DIR, rt_file)))
  # Production builder: named lineup, emergencies dropped, each player's rating
  # multiplied by POSITION_AVG_TOG[lineup_position] before summing, missing
  # ratings imputed with the per-channel prior.
  tr <- as.data.table(.build_team_ratings_df(teams, rt, psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", CH), with = FALSE],
             a[, c("match_id", CH), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")
  m[, arm := label]
  say("  ", label, ": ", nrow(m), " matches, mean players/team ",
      round(mean(tr$count), 2))
  m
}

say("")
say("--- building team features via .build_team_ratings_df() ---")
m2 <- build_diffs("epv3_ratings_v2.parquet", "v2")
m3 <- build_diffs("epv3_ratings_v3.parquet", "v3 (4ch)")

fit_report <- function(m, label, target, chans) {
  d <- m[is.finite(get(target))]
  # Drop channels with no variance (v3 3-channel zeroes the hitout slot).
  chans <- chans[vapply(chans, function(c) sd(d[[paste0("d_", c)]], na.rm = TRUE) > 1e-9, logical(1))]
  frm <- as.formula(paste(target, "~ 0 +", paste0("d_", chans, collapse = " + ")))
  f <- lm(frm, data = d)
  co <- summary(f)$coefficients
  data.table(arm = label, target = target, n = nrow(d),
             channel = sub("^d_", "", rownames(co)),
             coef = round(co[, 1], 4), se = round(co[, 2], 4), t = round(co[, 3], 2))
}

say("")
say("=== Per-channel coefficients: points of margin per unit of EPR ===")
tab <- rbindlist(list(
  fit_report(m2, "v2", "margin",  CH),
  fit_report(m2, "v2", "xmargin", CH),
  fit_report(m3, "v3", "margin",  CH),
  fit_report(m3, "v3", "xmargin", CH)
))
say_dt(dcast(tab, arm + channel ~ target, value.var = "coef"), 20)
say("")
say("with t statistics:")
say_dt(dcast(tab, arm + channel ~ target, value.var = "t"), 20)

say("")
say("=== The single global constant, for comparison with the live 0.919 ===")
for (nm in c("v2", "v3")) {
  m <- if (nm == "v2") m2 else m3
  m[, d_epr_tot := d_epr_recv + d_epr_disp + d_epr_spoil + d_epr_hitout]
  for (tg in c("margin", "xmargin")) {
    d <- m[is.finite(get(tg))]
    f <- lm(as.formula(paste(tg, "~ 0 + d_epr_tot")), data = d)
    ci <- confint(f)
    say(sprintf("  %-4s %-8s  slope %.4f  [%.4f, %.4f]  (live EPV_POINTS_SCALE = %.3f)",
                nm, tg, coef(f)[[1]], ci[1], ci[2], EPV_POINTS_SCALE))
  }
}

say("")
say("=== PROPOSED per-channel constants (v3, fitted on xmargin) ===")
k <- tab[arm == "v3" & target == "xmargin"]
say(paste0("EPV3_POINTS_SCALE <- c(",
           paste(sprintf("%s = %.4f", sub("^epr_", "", k$channel), k$coef),
                 collapse = ", "), ")"))
say("")
say("cross-check on ACTUAL margin (should be close, xmargin is just quieter):")
k2 <- tab[arm == "v3" & target == "margin"]
say(paste0("  on margin: ",
           paste(sprintf("%s = %.4f", sub("^epr_", "", k2$channel), k2$coef),
                 collapse = ", ")))

say("")
say("=== verification: scaling by these must return coefficients of 1.0 ===")
mc <- copy(m3)
for (i in seq_len(nrow(k))) {
  cc <- paste0("d_", k$channel[i])
  mc[, (cc) := get(cc) * k$coef[i]]
}
vf <- fit_report(mc, "v3 scaled", "xmargin", CH)
say_dt(vf[, .(channel, coef, t)], 6)
say("VERDICT: ", if (max(abs(vf$coef - 1)) < 1e-4) "MET" else "NOT MET")

say("")
say("=== WHICH LAYER, AND WHY IT MATTERS ===")
say("EPV layer (value accrued IN a game -> that game's margin):")
say("  recv 0.599  disp 0.612  cont 0.260   -- descriptive")
say("EPR layer (historical rating -> margin), above -- predictive, and what")
say("production's 0.919 means. These are different quantities, not competing")
say("estimates of one thing. Quote the layer whenever you quote a number.")

arrow::write_parquet(tab, file.path(OUT_DIR, "epv3_points_calibration_epr.parquet"))
close(con)
cat("\nDone\n")
