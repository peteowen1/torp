# Sean Darcy vs Max Gawn, every step from box score to EPR.
#
# Darcy came out 5th in the competition under the calibrated v2 and Gawn did not
# make the top 20, which is the opposite of what every external list says. This
# walks the arithmetic rather than reasoning about it, because the earlier
# explanation for a ruck anomaly ("Cox is an attendance freeloader") was wrong
# and only the numbers caught it.
#
# The chain: box stats -> per-game EPV channels -> per-80 -> position centring
# and standardising -> points scale -> Bayesian shrinkage -> EPR.
#
# Also prints what the channel WOULD be under EPV3_STOP_ZERO_SUM, the win/loss
# ledger. That flag is v3-only, so the v2 arm being gated still pays a ruck for
# every contest he ATTENDS including the ones he loses -- worth quantifying on
# these two specifically.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "walkthrough_darcy_gawn.txt"), split = TRUE)
cat("=== Sean Darcy vs Max Gawn, step by step ===\nrun at", format(Sys.time()), "\n")

WHO <- c("Sean Darcy", "Max Gawn")
pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v2.parquet")))
S <- max(pgd$season, na.rm = TRUE)
cur <- pgd[season == S & player_name %chin% WHO]

cat("\n########## 1. GAMES AND MINUTES ##########\n")
g <- cur[, .(games = .N,
             tog = round(mean(time_on_ground_percentage, na.rm = TRUE), 1),
             rounds = paste(range(round), collapse = "-")), by = player_name]
print(g)
cat("\nGames played is the first thing to check: EPR shrinks toward a prior with\n")
cat("EPR_PRIOR_GAMES = 3, so a small sample is pulled toward it -- but a high\n")
cat("per-game rate on few games can still finish high.\n")

cat("\n########## 2. RAW BOX INPUTS TO THE HITOUT CHANNEL ##########\n")
b <- cur[, .(ruck_contests = round(mean(ruck_contests, na.rm = TRUE), 1),
             hitouts = round(mean(hitouts, na.rm = TRUE), 1),
             to_advantage = round(mean(hitouts_to_advantage, na.rm = TRUE), 1)),
         by = player_name]
b[, `:=`(lost = round(ruck_contests - hitouts, 1),
         win_pct = round(100 * hitouts / ruck_contests, 1))]
print(b)

cat("\n########## 3. THE HITOUT CHANNEL, TERM BY TERM (per game) ##########\n")
cat(sprintf("weights: hitout %.4f | to advantage %.4f | contest ATTENDED %.4f\n\n",
            EPV_HITOUT_WT, EPV_HITOUT_ADV_WT, EPV_RUCK_CONTEST_WT))
t3 <- merge(b, cur[, .(epv_hitout = round(mean(epv_hitout, na.rm = TRUE), 3)), by = player_name],
            by = "player_name")
t3[, `:=`(from_win = round(hitouts * EPV_HITOUT_WT, 3),
          from_adv = round(to_advantage * EPV_HITOUT_ADV_WT, 3),
          from_attend = round(ruck_contests * EPV_RUCK_CONTEST_WT, 3))]
print(t3[, .(player_name, from_win, from_adv, from_attend, total = epv_hitout)])

cat("\n-- and what it would be as a WIN/LOSS LEDGER (EPV3_STOP_ZERO_SUM, v3-only) --\n")
t3[, ledger := round(hitouts * EPV_HITOUT_WT + to_advantage * EPV_HITOUT_ADV_WT +
                       hitouts * EPV_RUCK_CONTEST_WT - lost * EPV_RUCK_LOSS_WT, 3)]
print(t3[, .(player_name, pay_to_attend = epv_hitout, win_loss_ledger = ledger,
             difference = round(ledger - epv_hitout, 3))])

cat("\n########## 4. ALL FOUR CHANNELS, RAW PER GAME ##########\n")
ch <- c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout")
print(cur[, c(lapply(.SD, function(v) round(mean(v, na.rm = TRUE), 3)),
              .(epv = round(mean(epv, na.rm = TRUE), 3))),
          .SDcols = ch, by = player_name])

cat("\n########## 5. AFTER PER-80, CENTRING AND STANDARDISING (_adj) ##########\n")
adj <- paste0(ch, "_adj")
have <- intersect(adj, names(cur))
if (length(have)) {
  print(cur[, lapply(.SD, function(v) round(mean(v, na.rm = TRUE), 3)),
            .SDcols = have, by = player_name])
  cat("\nThese are per-80-minute, centred within position and standardised.\n")
  cat("A ruck is compared with other RUCKS here, so a big raw hitout number is\n")
  cat("only worth something if it beats the other rucks.\n")
} else cat("(no _adj columns on this frame)\n")

cat("\n########## 6. THE FINAL EPR, BOTH ARMS ##########\n")
CHR <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
latest <- function(f, lab) {
  d <- as.data.table(read_parquet(file.path(OUT_DIR, f)))
  d <- d[season == max(season, na.rm = TRUE) & player_name %chin% WHO]
  d <- d[, .SD[which.max(round)], by = player_name]
  d[, arm := lab]
  d[, c("arm", "player_name", "round", intersect(c("epr", CHR), names(d))), with = FALSE]
}
both <- rbind(latest("v2cal_rt_global.parquet", "production"),
              latest("v2cal_rt_percal.parquet", "calibrated"), fill = TRUE)
num <- setdiff(names(both), c("arm", "player_name"))
both[, (num) := lapply(.SD, function(v) round(v, 3)), .SDcols = num]
print(both)

cat("\n########## 7. WHERE THE GAP ACTUALLY COMES FROM ##########\n")
w <- dcast(both, player_name ~ arm, value.var = c("epr", CHR))
print(w)
cat("\nRead the calibrated columns. The scale is recv 0.870, disp 0.502,\n")
cat("spoil 2.892, hitout 4.033 -- so the hitout row is multiplied ~4x and the\n")
cat("disposal row roughly halved. A ruck whose value is almost all hitout gains\n")
cat("from both directions at once.\n")

saveRDS(list(box = b, terms = t3, epr = both), file.path(OUT_DIR, "walkthrough_darcy_gawn.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
