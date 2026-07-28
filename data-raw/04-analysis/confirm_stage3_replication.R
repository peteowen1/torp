# Replicate Stage 3 exactly, and see whether the published collapse reappears
# ===========================================================================
# Where we are. Published v2 epr SD collapses through 2022 (2.43 -> 1.09) and
# 2025, rises normally elsewhere. Ruled out so far:
#   - position amplification (uniform across all 7 groups)
#   - genuine competitive variation (match-margin SD flat 38-42)
#   - per-season standardisation constants (pooling barely moves anything)
#   - the per-game data itself: the PUBLISHED player_game_{season} files have
#     consistent epv_adj scale across seasons (8.22-8.79, gap 1.069), zero NAs,
#     zero zeros, and match a clean local rebuild to 3dp
#   - PSR: it rises normally (x1.17) in the same season EPR collapses (x0.45),
#     so this is specific to the EPR aggregation
#
# A clean calculate_epr() run over 2022 from those same inputs gives a HEALTHY
# trajectory (2.59 -> 2.86). So the published file disagrees with what the code
# produces from the published inputs.
#
# The untested surface is what Stage 3 does that the clean run did not:
#   all_pgd <- load_player_game_data(TRUE)      # pooled, all seasons
#   all_pgd <- adjust_epv_for_opponents(all_pgd) # adds _oadj, used by EPR
#   ... calculate_epr(..., skills = <stat ratings>)  # TOG adjustment
#
# This script runs that exact chain for 2022 and compares the trajectory with
# the published one. Also reports the diagnostic the aggregate SD hides: the
# NA/'0' rate of the _oadj columns per season, since a game whose _oadj is
# missing contributes nothing to the EPR numerator while still counting toward
# wt_gms -- which is precisely the sd-proportional-to-1/wt_gms signature the
# published 2022 and 2025 show (cor(sd, 1/wt) = +0.99).
#
# Run: powershell.exe -Command 'Rscript "<this file>"'

suppressMessages({
  library(dplyr); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

SEASONS   <- 2021:2022
TEST_SEAS <- 2022
OUT <- "C:/dev/torpverse/torpmodels/data-raw/04-match-model/experiments/results/stage3_replication.rds"

cli::cli_h1("Loading PUBLISHED player game data for {SEASONS}")
pgd <- as.data.table(load_player_game_data(SEASONS))
cli::cli_inform("{nrow(pgd)} rows, {ncol(pgd)} cols")
cat("\nper-game epv_adj SD by season (published inputs):\n")
print(pgd[!is.na(epv_adj), .(n = .N, sd = round(sd(epv_adj), 3)), by = season][order(season)],
      row.names = FALSE)

cli::cli_h2("Applying the opponent adjustment (Stage 3 does this; the clean run did not)")
pgd_o <- adjust_epv_for_opponents(pgd)

oadj <- grep("_oadj$", names(pgd_o), value = TRUE)
cat("\n_oadj columns:", paste(oadj, collapse = ", "), "\n")
cat("\n=== _oadj health by season -- an NA here contributes nothing to the EPR\n")
cat("    numerator while the game still counts toward wt_gms ===\n")
chk <- pgd_o[, c(list(n = .N),
                 lapply(.SD, function(x) sum(is.na(x)))),
             by = season, .SDcols = oadj][order(season)]
setnames(chk, oadj, paste0("na_", oadj))
print(chk, row.names = FALSE)

cat("\n=== _oadj scale by season (vs _adj) ===\n")
sc <- pgd_o[!is.na(epv_adj), .(
  sd_adj  = round(sd(epv_adj, na.rm = TRUE), 3),
  sd_oadj = round(sd(epv_oadj, na.rm = TRUE), 3),
  mean_adj  = round(mean(epv_adj, na.rm = TRUE), 3),
  mean_oadj = round(mean(epv_oadj, na.rm = TRUE), 3)
), by = season][order(season)]
print(sc, row.names = FALSE)

cli::cli_h2("Running calculate_epr over {TEST_SEAS} with the Stage 3 inputs")
skills <- tryCatch(get_player_stat_ratings(current = FALSE), error = function(e) {
  cli::cli_alert_danger("stat ratings unavailable: {conditionMessage(e)}"); NULL })
if (!is.null(skills)) cli::cli_inform("stat ratings: {nrow(skills)} rows")

rounds <- sort(unique(pgd_o$round_number[pgd_o$season == TEST_SEAS]))
rounds <- rounds[!is.na(rounds) & rounds >= 1]

traj <- rbindlist(lapply(rounds, function(r) {
  e <- tryCatch(
    calculate_epr(season_val = TEST_SEAS, round_val = r,
                  player_game_data = pgd_o,
                  skills = if (is.null(skills)) FALSE else skills),
    error = function(err) { cli::cli_warn("R{r}: {conditionMessage(err)}"); NULL })
  if (is.null(e)) return(NULL)
  e <- as.data.table(e)[!is.na(epr)]
  data.table(round = r, n = nrow(e), sd = sd(e$epr),
             wt = mean(as.numeric(wt_gms), na.rm = TRUE))
}), fill = TRUE)

cli::cli_h1("Trajectory comparison for {TEST_SEAS}")
cat("replicated Stage 3 :", paste(sprintf("%.2f", traj$sd), collapse = " "), "\n")
cat("published v2       : 2.43 2.28 2.14 2.02 1.92 1.83 1.75 1.68 1.64 ... 1.09\n")
cat("clean run (arm 3)  : 2.59 2.61 2.65 2.68 2.67 2.74 2.78 2.83 2.87 ... 2.86\n")
if (nrow(traj) > 1) {
  cat(sprintf("\nreplicated end/start ratio = %.3f  (published 0.45, clean 1.10)\n",
              traj$sd[nrow(traj)] / traj$sd[1]))
  cat(sprintf("cor(sd, 1/wt) = %+.3f  (published 2022: +0.993)\n",
              cor(traj$sd, 1 / traj$wt, use = "complete.obs")))
}
saveRDS(list(traj = traj, oadj_na = chk, scale = sc), OUT)
cli::cli_alert_success("Saved {OUT}")
