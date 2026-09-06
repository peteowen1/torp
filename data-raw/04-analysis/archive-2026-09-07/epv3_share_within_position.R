# The one test that decides the surprise share: does the extra separation REPEAT?
#
# WHERE THIS SITS. epv3_share_separation.txt left the measured share ambiguous:
#
#   repeatability (pooled)   epv 0.6546 -> 0.6569        a wash
#   count-dependence         better on all three          for
#   conservation             0.9936 both                  unchanged, as predicted
#   position balance         one MEDIUM_DEFENDER enters   marginally for
#   within-position spread   higher in ALL SIX groups     for, IF it is real
#
# The last line is the whole question and the pooled repeatability cannot answer
# it. Pooled year-over-year is dominated by BETWEEN-position variance -- key
# forwards score differently from key defenders every year, and that alone
# produces a high correlation regardless of whether the metric separates players
# who play the same role. A share that spreads players out more within a
# position is an improvement only if the spread persists.
#
# So: year-over-year repeatability computed WITHIN each position group, after
# centring on that group's mean each season. What survives is the part that
# distinguishes a good key defender from an average one, which is the thing
# Pete's "sensible ranking with players in each position" actually needs.
#
# If the measured share raises within-position repeatability, its extra spread
# is signal and it should ship despite costing ~0.10 MAE. If it does not, the
# spread is noise and the flat 0.5 stays -- four constants that buy dispersion
# without persistence are worse than one that does not.
#
# ~2 min, cached frames only.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_share_within_position.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Does the measured share's extra spread repeat? ===")
say("run at ", format(Sys.time()))

ARMS <- c(flat = "epv3_difficulty_wired_pgd.parquet",
          measured = "epv3_diffgate_pgd_diff_type.parquet")
res <- as.data.table(load_results(TRUE))
d <- lapply(ARMS, function(f) {
  x <- as.data.table(read_parquet(file.path(OUT_DIR, f)))
  calibrate_epv_channels(x, results = res)
})

# Season rate per player, then centre within (position, season) so only the
# within-position part survives. Correlate consecutive seasons.
wyoy <- function(x, col, min_games = 8) {
  v <- data.table(player_id = x$player_id, season = x$season,
                  pos = x$position_group, v = x[[col]])
  v <- v[is.finite(v) & !is.na(pos)]
  s <- v[, .(r = sum(v), g = .N, pos = pos[1]), by = .(player_id, season)][g >= min_games]
  s[, rate := r / g]
  s[, rate_c := rate - mean(rate), by = .(pos, season)]
  b <- copy(s)[, season := season - 1]
  setnames(b, c("rate", "rate_c"), c("rate_next", "rate_c_next"))
  m <- merge(s[, .(player_id, season, pos, rate, rate_c)],
             b[, .(player_id, season, rate_next, rate_c_next)],
             by = c("player_id", "season"))
  m[is.finite(rate_c) & is.finite(rate_c_next)]
}

say(""); say("=== 1. WITHIN-POSITION REPEATABILITY, ALL POSITIONS POOLED ===")
say("(centred within position-season, so between-position variance is gone)")
rows <- rbindlist(lapply(c("epv_recv_cal", "epv_disp_cal", "epv_cal"), function(cc) {
  a <- wyoy(d$flat, cc); b <- wyoy(d$measured, cc)
  data.table(channel = cc, n = nrow(b),
             pooled_flat = round(cor(a$rate, a$rate_next), 4),
             pooled_meas = round(cor(b$rate, b$rate_next), 4),
             within_flat = round(cor(a$rate_c, a$rate_c_next), 4),
             within_meas = round(cor(b$rate_c, b$rate_c_next), 4))
}))
rows[, within_delta := round(within_meas - within_flat, 4)]
say_dt(rows, 5)

say(""); say("=== 2. BY POSITION GROUP (epv_cal) ===")
a <- wyoy(d$flat, "epv_cal"); b <- wyoy(d$measured, "epv_cal")
per <- merge(
  a[, .(n = .N, flat = round(cor(rate_c, rate_c_next), 4)), by = pos],
  b[, .(measured = round(cor(rate_c, rate_c_next), 4)), by = pos], by = "pos")
per[, delta := round(measured - flat, 4)]
say_dt(per[order(-n)], 8)
say("")
say("Groups with small n carry wide error bars -- a delta there is not evidence.")

say(""); say("=== VERDICT INPUT ===")
wd <- rows[channel == "epv_cal", within_delta]
say(sprintf("  within-position repeatability, total epv_cal: %+.4f", wd))
say("")
if (wd > 0.01) {
  say("  The extra spread REPEATS. The measured share is separating players who")
  say("  play the same role, which is what criterion 3 asks for, and the ~0.10")
  say("  MAE cost is the kind Pete said he would pay.")
} else if (wd < -0.01) {
  say("  The extra spread does NOT repeat -- it is noise. Keep the flat 0.5.")
} else {
  say("  Flat. The measured share buys dispersion that neither persists nor")
  say("  decays, so the four extra constants are not paying for themselves.")
  say("  Keep EPV_DIFFICULTY_SURPRISE_BY_TYPE = FALSE, and treat the measured")
  say("  table as a documented alternative rather than a pending improvement.")
}
say("")
say("EITHER WAY the within-branch MEASUREMENT stands (retained 0.66-0.70,")
say("turnover 0.79-0.82, handball ~0.5, kick ~0.7-0.8). What is undecided is")
say("whether acting on it improves the metric, which is a different question")
say("from whether the number is right.")

close(con); cat("\nDone\n")
