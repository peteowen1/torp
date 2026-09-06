# The rating-layer v2 calibration, on the fast causal gate.
#
# WHY THIS AND NOT THE BENCHMARK SUITE. `EPV_PER_CHANNEL_POINTS_SCALE` is applied
# inside centre_epv_by_position(), i.e. during the RATING build -- both arms
# share one player-game frame. The benchmark suite scores player-game frames, so
# it is structurally blind to this change: it would report two identical
# arms. The EPR gate reads ratings, so it is the right instrument and the only
# fast one.
#
# Not to be confused with calibrate_epv_channels(), which is the RAW-layer
# calibration: different fitted values (0.893/1.556/0.344 against this arm's
# 0.870/0.502/2.892/4.033), scores on the suite instead, is called by nothing,
# and never reaches a rating. That one fixes conservation 0.4778 -> 1.0000 and
# needs no match gate at all. Quoting its evidence for this change would be
# borrowing, which is what happened once already today.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "epr_gate_v2cal.txt"), split = TRUE)
cat("=== v2 global scale vs per-channel, at the rating layer ===\nrun at",
    format(Sys.time()), "\n")

res <- load_results(TRUE)
pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v2.parquet")))
cat("both arms share this player-game frame:", nrow(pgd), "rows\n")

g1 <- bm_epr_gate(pgd, as.data.table(read_parquet(file.path(OUT_DIR, "v2cal_rt_global.parquet"))),
                  res, "v2 global 0.919")
g2 <- bm_epr_gate(pgd, as.data.table(read_parquet(file.path(OUT_DIR, "v2cal_rt_percal.parquet"))),
                  res, "v2 per-channel")
print(g1); print(g2); compare_epr_gates(g1, g2)

cat("\nREAD. OOS mean MAE is the guardrail -- a rise means the rating got worse at\n")
cat("predicting team points. The within-team coefficient and its t are the causal\n")
cat("reads: does the rating track WHO IS PLAYING. The points-conceded row says\n")
cat("whether defence is measured better, which matters because the fitted scale\n")
cat("lifts the contest and hitout channels 2.9x and 4.0x -- so if defence is not\n")
cat("better captured after tripling the contest channel, that is worth knowing.\n")

saveRDS(list(global = g1, percal = g2), file.path(OUT_DIR, "epr_gate_v2cal.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
