# Can raw EPV be calibrated per channel AND still conserve?
#
# THE TENSION THIS TESTS. Pete's second criterion is "1 point ~ 1 point". Under
# the difficulty split the TOTAL is there -- raw epv converts to margin at
# 0.9936 -- but the channels are not:
#
#     recv 1.399   disp 1.103   contest 0.368
#
# A point of reception credit buys 1.4 points of margin and a point of contest
# credit buys 0.37. So the total is right by cancellation, which is not the same
# as being right. Anyone reading a player's contest number is reading something
# that is not in points.
#
# EPV3_POINTS_SCALE already fixes this, but at the EPR layer -- the RATING input.
# The raw per-game frame, which is the natural descriptive product ("what did
# this player do in this game, in points"), has never been calibrated.
#
# THE CATCH, and it is the whole question. Rescaling each channel by 1/coef makes
# every channel read 1.000 by construction. It does NOT follow that the SUM still
# converts at 1: the multivariate fit is over three correlated regressors, and
# summing rescaled channels is a different quantity from the fitted value. If
# per-channel calibration costs total conservation, the two criteria are in
# conflict and Pete has a choice to make rather than a fix to apply.
#
# Reported both ways, plus what the rescale does to each channel's spread --
# a channel scaled up 2.7x is 2.7x more prominent in any leaderboard built from
# it, which is a product decision, not just an arithmetic one.
#
# ~2 min, cached frames only.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_raw_layer_calibration.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Raw-layer per-channel calibration ==="); say("run at ", format(Sys.time()))

res <- as.data.table(load_results(TRUE))
tgt <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score)][is.finite(margin)]

CH <- c("epv_recv", "epv_disp", "epv_spoil")
team_diffs <- function(d) {
  ts <- d[, lapply(.SD, function(v) sum(v, na.rm = TRUE)), .SDcols = CH,
          by = .(match_id, team)]
  h <- merge(tgt, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tgt, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, c("match_id", "margin", CH), with = FALSE],
             a[, c("match_id", CH), with = FALSE], by = "match_id",
             suffixes = c("_h", "_a"))
  for (v in CH) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m
}
fit3 <- function(m) {
  co <- summary(stats::lm(as.formula(paste("margin ~ 0 +", paste0("d_", CH, collapse = " + "))),
                          data = m))$coefficients
  setNames(co[, 1], CH)
}

ARMS <- list(ship = "epv3_fin_pgd_ship.parquet",
             difficulty = "epv3_difficulty_wired_pgd.parquet")

for (nm in names(ARMS)) {
  f <- file.path(OUT_DIR, ARMS[[nm]])
  if (!file.exists(f)) { say(""); say("MISSING: ", ARMS[[nm]]); next }
  d <- as.data.table(read_parquet(f))
  m <- team_diffs(d)
  co <- fit3(m)
  say(""); say("=== ", nm, " (", nrow(m), " matches) ===")
  say_dt(data.table(channel = CH, conversion = round(co, 4),
                    sd_raw = round(vapply(CH, function(v) sd(m[[paste0("d_", v)]]), 0), 2)), 4)

  # Total, uncalibrated.
  m[, tot_raw := Reduce(`+`, lapply(CH, function(v) m[[paste0("d_", v)]]))]
  c_raw <- coef(stats::lm(margin ~ 0 + tot_raw, data = m))[[1]]

  # Total, with each channel rescaled to read 1.000.
  s <- 1 / co
  m[, tot_cal := Reduce(`+`, lapply(CH, function(v) s[[v]] * m[[paste0("d_", v)]]))]
  c_cal <- coef(stats::lm(margin ~ 0 + tot_cal, data = m))[[1]]

  say("")
  say(sprintf("  scale to apply:  recv %.4f  disp %.4f  contest %.4f",
              s[["epv_recv"]], s[["epv_disp"]], s[["epv_spoil"]]))
  say(sprintf("  TOTAL converts:  uncalibrated %.4f  ->  per-channel calibrated %.4f",
              c_raw, c_cal))
  say(sprintf("  |1 - conversion| goes %.4f -> %.4f  (%s)",
              abs(1 - c_raw), abs(1 - c_cal),
              ifelse(abs(1 - c_cal) <= abs(1 - c_raw), "no conflict", "CONFLICT")))

  # What the rescale does to each channel's prominence.
  say("")
  say("  effect on spread -- a channel scaled up dominates any leaderboard")
  say("  built from the sum, which is a product decision not just arithmetic:")
  say_dt(data.table(
    channel = CH,
    sd_before = round(vapply(CH, function(v) sd(d[[v]], na.rm = TRUE), 0), 3),
    sd_after = round(vapply(CH, function(v) s[[v]] * sd(d[[v]], na.rm = TRUE), 0), 3),
    share_before = round(100 * vapply(CH, function(v) sd(d[[v]], na.rm = TRUE), 0)^2 /
      sum(vapply(CH, function(v) sd(d[[v]], na.rm = TRUE), 0)^2), 1),
    share_after = round(100 * vapply(CH, function(v) s[[v]] * sd(d[[v]], na.rm = TRUE), 0)^2 /
      sum(vapply(CH, function(v) s[[v]] * sd(d[[v]], na.rm = TRUE), 0)^2), 1)), 4)

  # Does calibrating change WHO is on top? If the leaderboard is unchanged the
  # calibration is cosmetic; if it reorders heavily it is a real product change.
  cur <- d[season == max(season, na.rm = TRUE)]
  agg <- cur[, .(g = .N,
                 raw = sum(epv_recv + epv_disp + epv_spoil, na.rm = TRUE),
                 cal = sum(s[["epv_recv"]] * epv_recv + s[["epv_disp"]] * epv_disp +
                           s[["epv_spoil"]] * epv_spoil, na.rm = TRUE)),
             by = .(player_name, position_group)][g >= 8]
  agg[, `:=`(rk_raw = frank(-raw), rk_cal = frank(-cal))]
  say("")
  say(sprintf("  leaderboard reorder: Spearman %.4f | mean |rank change| %.1f of %d players",
              cor(agg$rk_raw, agg$rk_cal, method = "spearman"),
              mean(abs(agg$rk_raw - agg$rk_cal)), nrow(agg)))
  setorder(agg, rk_cal)
  say("  top 10 calibrated:")
  say_dt(agg[1:10, .(player_name, position_group, cal = round(cal, 1),
                     was = as.integer(rk_raw), now = as.integer(rk_cal))], 10)
  say("  positions in the calibrated top 40:")
  say_dt(agg[1:40, .N, by = position_group][order(-N)], 7)
}

say("")
say("=== HOW TO DECIDE ===")
say("If the calibrated total still converts near 1, per-channel calibration is")
say("free and should be applied to the descriptive product -- a contest number")
say("that is not in points is a number nobody can interpret.")
say("If it does not, 'every channel reads 1' and 'the total reads 1' are")
say("genuinely different products and the choice is Pete's, not a bug to fix.")

close(con); cat("\nDone\n")
