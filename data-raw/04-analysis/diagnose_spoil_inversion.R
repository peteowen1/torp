# Why does the spoil channel's adjustment INVERT it?
#
# The adjustment-layer view reads cor(adj, raw) = -0.902 with a top-5 overlap of
# 0/5: the biggest raw producers rate lowest. That is larger than the hitout
# distortion and it is live.
#
# NOT diagnosed by analogy with hitout. Two things differ:
#   * spoil IS standardised (EPV_STANDARDISE_CHANNELS = recv, disp, spoil), so
#     .position_adjust divides by the cell sd -- hitout does not
#   * spoil is a wide box aggregate, not a single act:
#       spoils, tackles, pressure_acts, def_half_pressure_acts, intercepts,
#       one_percenters, rebound50s, frees_against
#     A negative weight in there (frees_against) can flip a player's sign before
#     any centring happens.
#
# So the first question is whether the inversion is in the ADJUSTMENT at all, or
# already present in the raw channel's construction.
#
# Method that worked twice today: take the extremes, print every step.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "spoil_inversion.txt"), split = TRUE)
cat("=== Why does the spoil adjustment invert the channel? ===\nrun at",
    format(Sys.time()), "\n")

d <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_schemec_pgd.parquet")))
S <- max(d$season, na.rm = TRUE)
d[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                             time_on_ground_percentage) / 100, 0.1)]
d[, p80 := epv_spoil / tog_safe]

cat("\n########## 0. IS IT STANDARDISED? ##########\n")
cat("EPV_STANDARDISE_CHANNELS =", paste(EPV_STANDARDISE_CHANNELS, collapse = ", "), "\n")
cat("so spoil uses  (per80 - cell_mean) / cell_sd * pooled_sd * TOG\n")
cat("where hitout used  (per80 - cell_mean) * TOG.\n")

cat("\n########## 1. THE EXTREMES ##########\n")
pl <- d[season == S, .(gm = .N, tog = round(mean(tog_safe), 2),
                       raw = round(mean(epv_spoil, na.rm = TRUE), 3),
                       p80 = round(mean(p80, na.rm = TRUE), 3),
                       adj = round(mean(epv_spoil_adj, na.rm = TRUE), 3),
                       pos = position_group[1]), by = player_name][gm >= 8]
setorder(pl, -raw); top_raw <- pl[1:6]
setorder(pl, -adj); top_adj <- pl[1:6]
cat("\nbiggest RAW spoil value:\n"); print(top_raw)
cat("\nhighest ADJUSTED:\n"); print(top_adj)
cat(sprintf("\ncor(raw, adj) across %d players: %.3f\n", nrow(pl), cor(pl$raw, pl$adj)))
cat(sprintf("cor(raw, per80): %.3f   cor(per80, adj): %.3f   cor(raw, tog): %.3f\n",
            cor(pl$raw, pl$p80), cor(pl$p80, pl$adj), cor(pl$raw, pl$tog)))
cat("\nIf cor(raw, per80) is already negative the inversion happens at the\n")
cat("per-80 step, before any cell is involved. If it is positive and\n")
cat("cor(per80, adj) is negative, the cell is doing it.\n")

cat("\n########## 2. WHAT IS IN THE RAW CHANNEL ##########\n")
comp <- c("spoils", "tackles", "pressure_acts", "def_half_pressure_acts",
          "intercepts", "one_percenters", "rebound50s", "frees_against")
have <- intersect(comp, names(d))
wts <- c(spoils = EPV_SPOIL_WT, tackles = EPV_TACKLE_WT,
         pressure_acts = EPV_PRESSURE_WT, def_half_pressure_acts = EPV_DEF_PRESSURE_WT,
         intercepts = EPV_INTERCEPTS_WT, one_percenters = EPV_ONE_PERCENTERS_WT,
         rebound50s = EPV_REBOUND50S_WT, frees_against = EPV_FREES_AGAINST_WT)
cat("weights:\n"); print(round(wts[have], 4))
cat("\nany NEGATIVE weight flips a component before centring:\n")
print(names(wts[have])[wts[have] < 0])
cur <- d[season == S]
contrib <- rbindlist(lapply(have, function(c) data.table(
  component = c, weight = round(unname(wts[[c]]), 4),
  mean_count = round(mean(cur[[c]], na.rm = TRUE), 2),
  mean_points = round(mean(cur[[c]], na.rm = TRUE) * unname(wts[[c]]), 4))))
setorder(contrib, -mean_points); print(contrib)

cat("\n########## 3. THE CELLS ##########\n")
mk <- function(x) {
  sl <- torp:::.remap_bench_role(as.character(x$lineup_position), x$player_id,
                                 x$season, x$position_group)
  if (isTRUE(ROLE_USE_LINEUP_GROUP)) torp:::.collapse_lineup_group(sl) else sl
}
d[, rk := mk(d)]
cells <- d[, .(n = .N, m = round(weighted.mean(p80, tog_safe, na.rm = TRUE), 3),
               s = round(sd(p80, na.rm = TRUE), 3)), by = .(cell = rk)]
setorder(cells, -m); print(cells[1:10])
cat("\nRatio of largest to smallest cell sd:",
    round(max(cells$s, na.rm = TRUE) / max(min(cells$s, na.rm = TRUE), 1e-9), 1), "\n")
cat("Dividing by the cell sd is what standardising does. A big ratio means a\n")
cat("player in a tight cell is inflated relative to one in a wide cell -- which\n")
cat("is the mechanism to check, since hitout (unstandardised) did not have it.\n")

cat("\n########## 4. RECONSTRUCT THE TOP TWO OF EACH ##########\n")
pooled <- sd(d$p80, na.rm = TRUE)
cat("pooled sd:", round(pooled, 3), "\n\n")
who <- unique(c(head(top_raw$player_name, 3), head(top_adj$player_name, 3)))
rec <- d[season == S & player_name %chin% who,
         .(cell = rk[1], tog = mean(tog_safe), p80 = mean(p80, na.rm = TRUE),
           raw = mean(epv_spoil, na.rm = TRUE),
           actual = mean(epv_spoil_adj, na.rm = TRUE)), by = player_name]
rec <- merge(rec, cells, by = "cell")
rec[, z := round((p80 - m) / s, 3)]
rec[, reconstructed := round(z * pooled * tog, 3)]
setorder(rec, -raw)
print(rec[, .(player_name, cell, tog = round(tog, 2), raw = round(raw, 2),
              p80 = round(p80, 2), cell_mean = m, cell_sd = s, z,
              reconstructed, actual = round(actual, 3))])

cat("\n########## READ ##########\n")
cat("Compare `raw` with `actual` down the table. Then look at which column\n")
cat("explains the reversal: a high raw with a high cell_mean (centred away), or\n")
cat("a low raw with a small cell_sd (inflated by standardising).\n")

saveRDS(list(players = pl, cells = cells, rec = rec),
        file.path(OUT_DIR, "spoil_inversion.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
