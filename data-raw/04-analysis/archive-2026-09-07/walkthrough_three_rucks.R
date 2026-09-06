# Gawn vs Grundy vs Cox, after the bench remap. Where does Cox beat Grundy?
#
# After the remap the hitout channel reads Cox 1.920 and Grundy 1.310, while the
# RAW values are Cox 2.14 and Grundy 5.55. Grundy produces 2.6x as much and
# rates below him. The Darcy case was solved by finding the centring cell, so
# the first thing to print is the cell each of these three actually lands in --
# `.role_key` is kept on the frame precisely so it can be read rather than
# assumed.
#
# The standing hypothesis is per-80 scaling: a part-time ruck rucks for nearly
# all his minutes, a full-time ruck also rests and rotates forward, so dividing
# by TOG rewards the part-timer. This tests it by reconstructing the formula
# term by term. If the reconstruction matches, the arithmetic says which term is
# responsible; if it does not, the model of the pipeline is wrong and that is
# worth knowing too.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "walkthrough_three_rucks.txt"), split = TRUE)
cat("=== Gawn vs Grundy vs Cox, after the bench remap ===\nrun at", format(Sys.time()), "\n")

WHO <- c("Max Gawn", "Brodie Grundy", "Mason Cox")
d <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_benchremap_pgd.parquet")))
S <- max(d$season, na.rm = TRUE)
cur <- d[season == S]

cat("\n########## 1. THE CENTRING CELL EACH ONE LANDS IN ##########\n")
# `.role_key` is deliberately dropped before the frame is returned --
# player_game_data is a released artifact with a declared column schema. So
# recompute it exactly as the pipeline does, from the frame's own columns.
# Reading `lineup_position` instead shows the PRE-remap slot and answers the
# wrong question, which is what the first attempt did.
d[, .rk := {
  sl <- torp:::.remap_bench_role(as.character(lineup_position), player_id,
                                 season, position_group)
  if (isTRUE(ROLE_USE_LINEUP_GROUP)) torp:::.collapse_lineup_group(sl) else sl
}]
cur <- d[season == S]
kc <- ".rk"
cat("key recomputed exactly as the pipeline builds it, bench remap applied\n")
kk <- cur[player_name %chin% WHO, .N, by = .(player_name, key = get(kc))]
setorder(kk, player_name, -N); print(kk)
cat("\nIf these are not all the same cell, the remap moved the problem rather\n")
cat("than fixing it -- a ruck/forward swingman gets sent to a FORWARD cell,\n")
cat("where the mean hitout output is near zero.\n")

cat("\n########## 2. THE INPUTS ##########\n")
cur[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
cur[, p80 := epv_hitout / tog_safe]
b <- cur[player_name %chin% WHO, .(
  games = .N, tog = round(mean(tog_safe), 3),
  contests = round(mean(ruck_contests, na.rm = TRUE), 1),
  hitouts = round(mean(hitouts, na.rm = TRUE), 1),
  to_adv = round(mean(hitouts_to_advantage, na.rm = TRUE), 1),
  raw = round(mean(epv_hitout, na.rm = TRUE), 3),
  per80 = round(mean(p80, na.rm = TRUE), 3),
  adj = round(mean(epv_hitout_adj, na.rm = TRUE), 3)), by = player_name]
setorder(b, -raw); print(b)
cat("\nper80 = raw / TOG. This is the quantity that gets centred and\n")
cat("standardised; raw is not.\n")

cat("\n########## 3. THE CELLS, ACROSS ALL SEASONS (what centring actually uses) ##########\n")
all <- copy(d)
all[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
all[, p80 := epv_hitout / tog_safe]
cells <- all[, .(n = .N, mean_p80 = round(mean(p80, na.rm = TRUE), 3),
                 sd_p80 = round(sd(p80, na.rm = TRUE), 3)), by = .(key = get(kc))]
setorder(cells, -mean_p80)
print(cells[1:8])
mine <- unique(cur[player_name %chin% WHO, .(player_name, key = get(kc))])
cat("\nthe cells our three sit in:\n"); print(merge(mine, cells, by = "key"))

cat("\n########## 4. RECONSTRUCT THE FORMULA ##########\n")
cat("adj = (per80 - cell_mean) / cell_sd * pooled_sd * TOG\n\n")
pooled <- sd(all$p80, na.rm = TRUE)
cat("pooled sd (all players, all seasons):", round(pooled, 3), "\n\n")
rec <- merge(merge(b, mine, by = "player_name"), cells, by = "key")
rec[, z := round((per80 - mean_p80) / sd_p80, 3)]
rec[, reconstructed := round(z * pooled * tog, 3)]
print(rec[, .(player_name, key, tog, per80, cell_mean = mean_p80, cell_sd = sd_p80,
              z, reconstructed, actual = adj)])

cat("\n########## 5. WHICH TERM IS RESPONSIBLE ##########\n")
cat("Holding everything else at Grundy's values, one term at a time:\n\n")
gr <- rec[player_name == "Brodie Grundy"]; cx <- rec[player_name == "Mason Cox"]
if (nrow(gr) && nrow(cx)) {
  cat(sprintf("  Grundy actual                          %.3f\n", gr$reconstructed))
  cat(sprintf("  Cox actual                             %.3f\n", cx$reconstructed))
  cat(sprintf("  Cox, but in Grundy's CELL              %.3f\n",
              (cx$per80 - gr$mean_p80) / gr$sd_p80 * pooled * cx$tog))
  cat(sprintf("  Cox, but with Grundy's TOG             %.3f\n", cx$z * pooled * gr$tog))
  cat(sprintf("  Cox, in Grundy's cell AND TOG          %.3f\n",
              (cx$per80 - gr$mean_p80) / gr$sd_p80 * pooled * gr$tog))
  cat("\nWhichever substitution closes the gap is the term to fix. If the CELL\n")
  cat("line does it, the remap sent Cox to the wrong group. If the TOG line\n")
  cat("does it, per-80 is the problem and the cell is fine.\n")
}

saveRDS(list(inputs = b, cells = cells, rec = rec),
        file.path(OUT_DIR, "walkthrough_three_rucks.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
