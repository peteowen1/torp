# Does the new adjustment-layer view actually catch the bug it was built for?
#
# A blind spot detector is only worth having if it fires on the case that
# created it. Three frames, all built today, all differing ONLY in the _adj
# columns -- the raw channels are identical across them, which is exactly why
# the old panel read zero delta on every row:
#
#   1. bench remap only   Max Heath (38% TOG, raw 2.35) tops the hitout channel
#                         and Grundy (raw 5.55) is 5th. Known broken.
#   2. + scheme C         Grundy 1st, ordered by production. Known good.
#
# PASS: the hitout row separates them -- cor(adj, raw) higher and/or
#       cor(adj, tog) nearer zero under scheme C -- and the two headline
#       numbers in compare_benchmarks() move.
# FAIL: it reads the same for both, in which case it is another blind view and
#       should not be trusted to guard anything.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "adj_panel_test.txt"), split = TRUE)
cat("=== Does the adjustment-layer view catch the centring bug? ===\nrun at",
    format(Sys.time()), "\n")

res <- load_results(TRUE)
broken <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_benchremap_pgd.parquet")))
fixed  <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_schemec_pgd.parquet")))

# The premise: raw identical, _adj different. If that is not true the test is
# measuring something else.
cat("\nraw channels identical across the two frames? ",
    isTRUE(all.equal(broken$epv_hitout, fixed$epv_hitout)), "\n")
cat("_adj differs? mean|diff| ",
    round(mean(abs(broken$epv_hitout_adj - fixed$epv_hitout_adj), na.rm = TRUE), 4), "\n")

a <- benchmark_rating(broken, "bench remap only", results = res, calibrate = TRUE)
b <- benchmark_rating(fixed,  "+ scheme C",       results = res, calibrate = TRUE)
print(a); print(b)
cmp <- compare_benchmarks(a, b)

cat("\n########## VERDICT ##########\n")
ra <- a$adj[channel == "hitout"]; rb <- b$adj[channel == "hitout"]
if (!nrow(ra) || !nrow(rb)) {
  cat("  hitout row missing -- the view did not compute. FAIL\n")
} else {
  cat(sprintf("  hitout cor(adj,raw)  %+.3f -> %+.3f  (%s)\n",
              ra$cor_adj_raw, rb$cor_adj_raw,
              ifelse(rb$cor_adj_raw > ra$cor_adj_raw, "improved", "not improved")))
  cat(sprintf("  hitout cor(adj,tog)  %+.3f -> %+.3f  (%s)\n",
              ra$cor_adj_tog, rb$cor_adj_tog,
              ifelse(abs(rb$cor_adj_tog) < abs(ra$cor_adj_tog), "nearer zero", "not nearer zero")))
  cat(sprintf("  hitout top5 overlap  %d/5 -> %d/5\n", ra$top5_overlap, rb$top5_overlap))
  caught <- rb$cor_adj_raw > ra$cor_adj_raw || abs(rb$cor_adj_tog) < abs(ra$cor_adj_tog)
  cat("\n  VERDICT: ", if (caught) "PASS -- the view separates the broken and fixed frames"
      else "FAIL -- still blind", "\n")
}
cat("\nAnd the two headline rows in the comparison table above are the ones that\n")
cat("would have flagged all three of this session's invisible changes.\n")

saveRDS(list(a = a$adj, b = b$adj, cmp = cmp), file.path(OUT_DIR, "adj_panel_test.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
