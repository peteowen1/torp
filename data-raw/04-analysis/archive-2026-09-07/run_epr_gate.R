# The fast EPR gate on the arms this session produced.
#
# Answers, in seconds, what the 20-minute-per-arm production gate cannot resolve:
# does the rating track a team's points, and does it survive team fixed effects.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "epr_gate_run.txt"), split = TRUE)
cat("=== fast EPR gate ===\nrun at", format(Sys.time()), "\n")

res <- load_results(TRUE)
ARMS <- list(
  ship = list(pgd = "epv3_fin_pgd_ship.parquet",       rt = "epv3_dgc_rt_dgc_ship_scaled.parquet"),
  difficulty = list(pgd = "epv3_difficulty_wired_pgd.parquet", rt = "epv3_dgc_rt_dgc_flat_scaled.parquet"))

g <- lapply(names(ARMS), function(nm) {
  pgd <- as.data.table(read_parquet(file.path(OUT_DIR, ARMS[[nm]]$pgd)))
  rt  <- as.data.table(read_parquet(file.path(OUT_DIR, ARMS[[nm]]$rt)))
  bm_epr_gate(pgd, rt, res, nm)
})
names(g) <- names(ARMS)
for (x in g) print(x)
compare_epr_gates(g$ship, g$difficulty)

saveRDS(g, file.path(OUT_DIR, "epr_gate_run.rds"))
cat("done", format(Sys.time()), "\n")
sink(); cat("\nDone\n")
