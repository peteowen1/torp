# The fast EPR gate on v2 production against v3 final.
#
# The production gate says v3 costs +0.3668 dMAE on 396 matches with a CI
# spanning zero. This asks the same question with 1,194 matches and the rating
# as the ONLY feature, so a genuine rating regression cannot be absorbed by the
# pipeline's other six features -- and an apparent one caused by that pipeline
# reweighting will not appear here at all.
#
# Both arms come from ws25, built under their own constants in one run.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "epr_gate_v2v3.txt"), split = TRUE)
cat("=== fast EPR gate: v2 production vs v3 final ===\nrun at", format(Sys.time()), "\n")

res <- load_results(TRUE)
g2 <- bm_epr_gate(as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v2.parquet"))),
                  as.data.table(read_parquet(file.path(OUT_DIR, "epv3_v2v3_rt_v2prod.parquet"))),
                  res, "v2 production")
g3 <- bm_epr_gate(as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v3diff.parquet"))),
                  as.data.table(read_parquet(file.path(OUT_DIR, "epv3_v2v3_rt_v3_stalepg.parquet"))),
                  res, "v3 final")
print(g2); print(g3); compare_epr_gates(g2, g3)

cat("\nHOW TO READ THIS AGAINST THE PRODUCTION GATE.\n")
cat("If v3 loses here too, the rating really is worse at predicting team points\n")
cat("and the +0.3668 is a rating problem. If the two are level here, the\n")
cat("production loss lives in how the pipeline uses the rating alongside elo,\n")
cat("psr and the rest -- a different problem with a different fix.\n")

saveRDS(list(v2 = g2, v3 = g3), file.path(OUT_DIR, "epr_gate_v2v3.rds"))
cat("done", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
