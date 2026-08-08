# Where exactly does Gawn > Darcy become Darcy > Gawn?
#
# Raw, Gawn is better on everything: epv 20.686 against 8.747, and hitout 4.981
# against 2.585. After per-80 + centring + standardising, Gawn's hitout_adj is
# 0.471 and Darcy's is 2.500. The flip is entirely in that step, and the
# calibration only multiplied the result by 4.
#
# Two candidates:
#   1. TIME ON GROUND. .position_adjust divides by tog to get a per-80 rate, then
#      multiplies BACK by tog at the end. Gawn plays 83.2% and Darcy 48.3%, so a
#      part-time ruck with a similar per-80 rate is treated very differently.
#   2. THE CENTRING CELL. The key is `lineup_position` (21 slots) by default, not
#      position_group. If Darcy's slot differs from Gawn's -- plausible on 48%
#      game time -- he is being compared with a group that barely rucks, and a
#      normal ruck output looks enormous against it.
#
# This prints both so the answer is read rather than guessed.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "walkthrough_centring_cell.txt"), split = TRUE)
cat("=== Why the flip? TOG or the centring cell ===\nrun at", format(Sys.time()), "\n")

pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v2.parquet")))
S <- max(pgd$season, na.rm = TRUE)
cur <- pgd[season == S]
WHO <- c("Sean Darcy", "Max Gawn")

cat("\n########## THE CENTRING KEY EACH PLAYER FALLS IN ##########\n")
cat("ROLE_USE_LINEUP_GROUP =", ROLE_USE_LINEUP_GROUP, "\n")
kcol <- if ("lineup_position" %in% names(cur)) "lineup_position" else "position_group"
cat("key column:", kcol, "\n\n")
k <- cur[player_name %chin% WHO, .N, by = .(player_name, key = get(kcol))]
setorder(k, player_name, -N)
print(k)

cat("\n########## PER-80 RATES, THE THING ACTUALLY CENTRED ##########\n")
cur[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
cur[, hitout_p80 := epv_hitout / tog_safe]
p <- cur[player_name %chin% WHO, .(games = .N,
        tog = round(mean(tog_safe), 3),
        raw_hitout = round(mean(epv_hitout), 3),
        per80 = round(mean(hitout_p80), 3)), by = player_name]
print(p)
cat("\nIf the per-80 rates are close, TOG is not what separates them and the\n")
cat("centring cell is. If they are far apart, TOG is doing the work.\n")

cat("\n########## THE CELL EACH IS MEASURED AGAINST ##########\n")
cells <- cur[, .(n_playergames = .N,
                 mean_per80 = round(mean(hitout_p80, na.rm = TRUE), 3),
                 sd_per80 = round(sd(hitout_p80, na.rm = TRUE), 3)),
             by = .(key = get(kcol))][order(-mean_per80)]
print(cells[1:10])
cat("\nand the cells our two sit in:\n")
mine <- unique(cur[player_name %chin% WHO, .(player_name, key = get(kcol))])
print(merge(mine, cells, by = "key"))

cat("\n########## THE STANDARDISED VALUE, RECONSTRUCTED ##########\n")
cat("centred / sd(cell) * pooled_sd * tog -- the .position_adjust formula\n\n")
pooled <- sd(cur$hitout_p80, na.rm = TRUE)
cat("pooled sd across all players:", round(pooled, 3), "\n\n")
rec <- merge(cur[player_name %chin% WHO,
                 .(tog = mean(tog_safe), per80 = mean(hitout_p80),
                   adj_actual = mean(epv_hitout_adj, na.rm = TRUE)),
                 by = .(player_name, key = get(kcol))],
             cells, by = "key")
rec[, reconstructed := round((per80 - mean_per80) / sd_per80 * pooled * tog, 3)]
print(rec[, .(player_name, key, tog = round(tog, 2), per80 = round(per80, 2),
              cell_mean = mean_per80, cell_sd = sd_per80,
              reconstructed, adj_actual = round(adj_actual, 3))])

cat("\n########## VERDICT ##########\n")
cat("If the two sit in DIFFERENT cells with different means, Darcy is being\n")
cat("compared with a group that does not ruck, and a normal ruck output looks\n")
cat("enormous against it -- a centring-key problem, not a calibration one.\n")
cat("If they share a cell, TOG is the lever: the same per-80 rate on half the\n")
cat("game time survives centring and then gets multiplied back by a small tog.\n")

saveRDS(list(cells = cells, rec = rec), file.path(OUT_DIR, "walkthrough_centring_cell.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
