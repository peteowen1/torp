# Does the contest channel earn its place at all?
#
# THE CASE FOR ASKING. Three independent measurements now point the same way:
#
#   raw variance share        42.9%  against 6.1% of the margin-explaining share
#   raw margin conversion     0.368  -- a point of contest credit buys a third
#                                       of a point
#   fitted EPR scale          0.2634 on a fresh build, against 0.5226 on the
#                                       stale one. Removing the i50 contamination
#                                       HALVED what the channel is worth.
#
# A channel carrying 43% of the dispersion and 6% of the signal is a candidate
# for deletion, not just for rescaling. And the honest version of that question
# has to be asked, because the alternative is shipping a metric whose largest
# moving part is mostly noise.
#
# THE COUNTER-ARGUMENT, which is why this is a measurement and not a proposal.
# From the two-products frame: for the DESCRIPTIVE product a noisy channel
# should be KEPT -- it happened, a contest was won, and conservation needs it.
# It is only the PREDICTIVE product that wants it shrunk. So "drop it" can be
# right for EPR and wrong for EPV at the same time, and the two must be answered
# separately rather than with one verdict.
#
# Contest is also the only channel carrying rucks at all (cont_stop), and the
# aerial part is the only thing crediting a contested mark. Deleting it does not
# just remove noise; it removes the only representation of two real acts.
#
# WHAT THIS MEASURES (player side only -- the MAE half needs a rolling eval):
#   1. conservation with and without contest, on the calibrated frame
#   2. within-position repeatability with and without
#   3. position balance in the top 40 with and without
#   4. which players actually depend on it
#
# ~3 min, cached frames only.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_contest_worth_keeping.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Does the contest channel earn its place? ==="); say("run at ", format(Sys.time()))

res <- as.data.table(load_results(TRUE))
tgt <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score)][is.finite(margin)]
x <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_difficulty_wired_pgd.parquet")))
x <- calibrate_epv_channels(x, results = res)
x[, epv_nocontest_cal := epv_recv_cal + epv_disp_cal]

say("")
say("=== 1. CONSERVATION ===")
conv <- function(col) {
  ts <- x[, .(v = sum(get(col), na.rm = TRUE)),
          by = .(match_id = as.character(match_id), team)]
  h <- merge(tgt, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tgt, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, .(match_id, margin, vh = v)], a[, .(match_id, va = v)], by = "match_id")
  m[, dv := vh - va]
  f <- summary(lm(margin ~ 0 + dv, data = m))
  c(conv = round(f$coefficients[1, 1], 4), r2 = round(f$r.squared, 4),
    sd = round(sd(m$dv), 2))
}
say_dt(rbindlist(list(
  data.table(build = "with contest", t(conv("epv_cal"))),
  data.table(build = "without contest", t(conv("epv_nocontest_cal")))
)), 3)
say("")
say("Conservation is the DESCRIPTIVE product's property. If dropping contest")
say("breaks it, the channel is load-bearing there no matter what it does for")
say("prediction -- a description that does not add up is not a description.")

wyoy <- function(col) {
  v <- data.table(player_id = x$player_id, season = x$season,
                  pos = x$position_group, v = x[[col]])[is.finite(v) & !is.na(pos)]
  s <- v[, .(r = sum(v), g = .N, pos = pos[1]), by = .(player_id, season)][g >= 8]
  s[, rate := r / g][, rate_c := rate - mean(rate), by = .(pos, season)]
  b <- copy(s)[, season := season - 1]; setnames(b, "rate_c", "rate_c_next")
  m <- merge(s[, .(player_id, season, pos, rate_c)], b[, .(player_id, season, rate_c_next)],
             by = c("player_id", "season"))
  m <- m[is.finite(rate_c) & is.finite(rate_c_next)]
  list(all = round(cor(m$rate_c, m$rate_c_next), 4),
       per = m[, .(n = .N, r = round(cor(rate_c, rate_c_next), 4)), by = pos])
}
say(""); say("=== 2. WITHIN-POSITION REPEATABILITY ===")
a <- wyoy("epv_cal"); b <- wyoy("epv_nocontest_cal")
say(sprintf("  with contest %.4f | without %.4f | delta %+.4f",
            a$all, b$all, b$all - a$all))
per <- merge(a$per, b$per, by = "pos", suffixes = c("_with", "_without"))
per[, delta := round(r_without - r_with, 4)]
say_dt(per[order(-n_with), .(pos, n = n_with, with = r_with, without = r_without, delta)], 8)

say(""); say("=== 3. POSITION BALANCE IN THE TOP 40 ===")
cur <- x[season == max(season, na.rm = TRUE)]
for (cc in c("epv_cal", "epv_nocontest_cal")) {
  agg <- cur[, .(g = .N, v = sum(get(cc), na.rm = TRUE)),
             by = .(player_name, position_group)][g >= 8]
  setorder(agg, -v)
  say(""); say("  --- ", cc, " ---")
  say("  top 8: ", paste(agg$player_name[1:8], collapse = ", "))
  say_dt(agg[1:40, .N, by = position_group][order(-N)], 7)
}

say(""); say("=== 4. WHO DEPENDS ON THE CHANNEL ===")
agg <- cur[, .(g = .N, with = sum(epv_cal, na.rm = TRUE),
               without = sum(epv_nocontest_cal, na.rm = TRUE)),
           by = .(player_name, position_group)][g >= 8]
agg[, `:=`(drop = round(with - without, 1),
           rk_with = frank(-with), rk_without = frank(-without))]
agg[, rank_move := as.integer(rk_without - rk_with)]
setorder(agg, -drop)
say("  most contest-dependent (biggest loss if the channel goes):")
say_dt(agg[1:10, .(player_name, position_group, drop, rank_move)], 10)
say("")
say(sprintf("  Spearman between the two leaderboards: %.4f",
            cor(agg$rk_with, agg$rk_without, method = "spearman")))
say(sprintf("  mean |rank change|: %.1f of %d players",
            mean(abs(agg$rank_move)), nrow(agg)))

say(""); say("=== HOW TO READ IT ===")
say("Two different verdicts are allowed and likely. For EPR (prediction), a")
say("channel with 6% of the signal and 43% of the dispersion is a shrink-or-drop")
say("candidate. For EPV (description), it is the only representation of a won")
say("contested mark and the only channel carrying rucks at all, so dropping it")
say("means the description stops describing two real acts.")
say("")
say("This measures the player side only. The MAE half needs a rolling eval and")
say("is not answered here -- do not read a favourable table above as a licence")
say("to drop the channel from the rating.")

close(con); cat("\nDone\n")
