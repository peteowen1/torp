# Settling the surprise share on PLAYER SEPARATION, which is the criterion that
# should decide it.
#
# WHY NOT MAE. The match gate returned dMAE +0.1043 (CI [-0.1369, +0.3455]) for
# the measured share against the flat 0.5 -- slightly worse, not significantly,
# and worse on bits while better on tips. That is not a weak signal to squint
# at; it is the wrong instrument. The share is a pure TRANSFER between
# `epv_recv` and `epv_disp` within a row, and the match model consumes both
# channel diffs separately across all five GAMs plus `epr_diff`, reweighting
# them itself. Moving value between two of its own inputs is close to invisible
# to it -- the same reason five earlier changes came back neutral there.
#
# What the share actually changes is WHICH PLAYER gets the value. So the tests
# are the ones that can see that:
#
#   repeatability     year over year, per channel. A share that puts value on
#                     the player who earned it should persist better than one
#                     that puts it on the wrong player.
#   count-dependence  cor with disposals, marks, contested possessions. A share
#                     bought by making a channel track an event count is the
#                     degenerate optimum, not an improvement.
#   position balance  who ends up in the top 40, and whether every position is
#                     represented -- Pete's third criterion.
#   dispersion        does either share separate players more within a position?
#                     Separation that is real is what repeatability confirms.
#
# Both frames were built minutes apart by the same code and differ ONLY in
# EPV_DIFFICULTY_SURPRISE_BY_TYPE (verified: different md5, and the gate's arms
# guard read mean|diff| 0.17275 on EPR).
#
# ~3 min, cached frames only.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_share_separation.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Surprise share, judged on player separation ==="); say("run at ", format(Sys.time()))

ARMS <- c(flat = "epv3_difficulty_wired_pgd.parquet",
          measured = "epv3_diffgate_pgd_diff_type.parquet")
d <- lapply(ARMS, function(f) as.data.table(read_parquet(file.path(OUT_DIR, f))))

# Guard: if these two are the same frame every number below is noise.
chk <- merge(d$flat[, .(match_id, player_id, a = epv_recv)],
             d$measured[, .(match_id, player_id, b = epv_recv)],
             by = c("match_id", "player_id"))
say(sprintf("arms differ: mean|d epv_recv| %.5f over %s player-games",
            mean(abs(chk$a - chk$b), na.rm = TRUE), format(nrow(chk), big.mark = ",")))
if (mean(abs(chk$a - chk$b), na.rm = TRUE) < 1e-9) {
  say("!! IDENTICAL FRAMES -- nothing below means anything."); close(con); quit(status = 1)
}

res <- as.data.table(load_results(TRUE))
tgt <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score)][is.finite(margin)]

yoy <- function(x, col) {
  v <- data.table(player_id = x$player_id, season = x$season, v = x[[col]])
  v <- v[is.finite(v)]
  s <- v[, .(r = sum(v), g = .N), by = .(player_id, season)][g >= 8]
  s[, rate := r / g]
  b <- copy(s)[, season := season - 1]; setnames(b, "rate", "rate_next")
  m <- merge(s[, .(player_id, season, rate)], b[, .(player_id, season, rate_next)],
             by = c("player_id", "season"))
  m <- m[is.finite(rate) & is.finite(rate_next)]
  round(cor(m$rate, m$rate_next), 4)
}

say(""); say("=== 1. YEAR-OVER-YEAR REPEATABILITY ===")
say_dt(rbindlist(lapply(c("epv_recv", "epv_disp", "epv"), function(cc) {
  data.table(channel = cc, flat = yoy(d$flat, cc), measured = yoy(d$measured, cc),
             delta = round(yoy(d$measured, cc) - yoy(d$flat, cc), 4))
})), 5)

say(""); say("=== 2. COUNT-DEPENDENCE (lower is better) ===")
say_dt(rbindlist(lapply(names(d), function(nm) {
  x <- d[[nm]]
  data.table(arm = nm,
             disp_vs_disposals = round(cor(x$epv_disp, x$disposals, use = "complete.obs"), 3),
             recv_vs_marks = round(cor(x$epv_recv, x$marks, use = "complete.obs"), 3),
             recv_vs_cposs = round(cor(x$epv_recv, x$contested_possessions, use = "complete.obs"), 3))
})), 3)

say(""); say("=== 3. CONSERVATION (must not move -- the share is a transfer) ===")
CH <- c("epv_recv", "epv_disp", "epv_spoil")
for (nm in names(d)) {
  x <- d[[nm]]
  ts <- x[, lapply(.SD, function(v) sum(v, na.rm = TRUE)), .SDcols = CH,
          by = .(match_id = as.character(match_id), team)]
  h <- merge(tgt, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tgt, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, c("match_id", "margin", CH), with = FALSE],
             a[, c("match_id", CH), with = FALSE], by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m[, tot := d_epv_recv + d_epv_disp + d_epv_spoil]
  co <- coef(lm(margin ~ 0 + d_epv_recv + d_epv_disp + d_epv_spoil, data = m))
  say(sprintf("  %-9s total %.4f | recv %.4f  disp %.4f  contest %.4f", nm,
              coef(lm(margin ~ 0 + tot, data = m))[[1]], co[[1]], co[[2]], co[[3]]))
}

say(""); say("=== 4. POSITION BALANCE IN THE TOP 40 (Pete's criterion 3) ===")
say("On CALIBRATED raw epv, since the uncalibrated leaderboard is dominated by")
say("the contest channel and would only re-measure that.")
for (nm in names(d)) {
  x <- calibrate_epv_channels(d[[nm]], results = res)
  cur <- x[season == max(season, na.rm = TRUE)]
  agg <- cur[, .(g = .N, v = sum(epv_cal, na.rm = TRUE)),
             by = .(player_name, position_group)][g >= 8]
  setorder(agg, -v)
  say(""); say("  --- ", nm, " ---")
  say("  top 8: ", paste(agg$player_name[1:8], collapse = ", "))
  say_dt(agg[1:40, .N, by = position_group][order(-N)], 7)
}

say(""); say("=== 5. WITHIN-POSITION SEPARATION ===")
say("sd of season epv_cal within each position group. More spread is only")
say("better if it repeats -- read this next to section 1, not on its own.")
sep <- rbindlist(lapply(names(d), function(nm) {
  x <- calibrate_epv_channels(d[[nm]], results = res)
  cur <- x[season == max(season, na.rm = TRUE)]
  agg <- cur[, .(g = .N, v = sum(epv_cal, na.rm = TRUE)),
             by = .(player_name, position_group)][g >= 8]
  agg[!is.na(position_group), .(arm = nm, sd = round(sd(v), 2), n = .N), by = position_group]
}))
say_dt(dcast(sep, position_group ~ arm, value.var = "sd"), 8)

say("")
say("=== HOW TO READ IT ===")
say("The measured share is worth adopting only if it raises repeatability")
say("WITHOUT raising count-dependence, or improves position balance. If it does")
say("neither, the flat 0.5 stays -- not because 0.5 is right, but because the")
say("measured table has not earned the extra four constants.")

close(con); cat("\nDone\n")
