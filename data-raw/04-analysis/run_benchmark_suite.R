# The benchmark suite, run on the two builds this session actually produced.
#
# First real use of the fixed protocol: does the difficulty split beat the ship
# build when both are judged the same way, every time, on metrics that carry
# their own failure mode?
#
# ~3 min, cached frames only -- against 100 minutes for a three-arm match gate
# that cannot resolve the effect anyway.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "benchmark_suite_run.txt"), split = TRUE)
cat("=== benchmark suite ===\nrun at", format(Sys.time()), "\n")

res <- load_results(TRUE)
ship <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_fin_pgd_ship.parquet")))
diff <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_difficulty_wired_pgd.parquet")))

cat("\nNOTE: the ship frame predates three contest-changing commits (handover §5).",
    "\nIt is the right comparison for the DIFFICULTY SPLIT specifically -- the",
    "\ndifficulty frame was built from it -- but do not read its absolute numbers",
    "\nas current v3.\n")

a <- benchmark_rating(ship, "ship", results = res)
b <- benchmark_rating(diff, "difficulty", results = res)
print(a); print(b)
compare_benchmarks(a, b)

saveRDS(list(ship = a, difficulty = b), file.path(OUT_DIR, "benchmark_suite_run.rds"))
cat("done", format(Sys.time()), "\n")
sink()
cat("\nDone\n")
