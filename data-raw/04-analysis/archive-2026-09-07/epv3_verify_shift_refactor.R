# Prove the .SDcols shift refactor in build_aerial_contests() is output-identical
# to the get()-based loop it replaced, and measure the memory it was costing.
#
# The old form ran 30 grouped `:=` calls, each `data.table::shift(get(stem), k)`.
# `get()` inside a grouped j defeats data.table's column analysis, so the FULL
# .SD is built for every match group. This script runs both constructions over
# the same real chains and compares all 30 generated columns exactly.
suppressPackageStartupMessages({
  library(data.table)
  devtools::load_all(".", quiet = TRUE)
})

season <- 2026
chains <- torp::load_chains(seasons = season)
setDT(chains)
cat("chains rows:", nrow(chains), " matches:", uniqueN(chains$match_id), "\n")

stems <- c("description", "player_id", "team_id", "x", "y")
cols  <- unlist(lapply(1:6, function(k) paste0(".f", k, "_", stems)))

old <- copy(chains); setorder(old, match_id, display_order)
new <- copy(chains); setorder(new, match_id, display_order)

t_old <- system.time({
  for (k in 1:6) for (stem in stems) {
    old[, (paste0(".f", k, "_", stem)) :=
          data.table::shift(get(stem), k, type = "lead"), by = match_id]
  }
})[["elapsed"]]

t_new <- system.time({
  for (k in 1:6) {
    new[, (paste0(".f", k, "_", stems)) :=
          data.table::shift(.SD, k, type = "lead"),
        by = match_id, .SDcols = stems]
  }
})[["elapsed"]]

mismatch <- character(0)
for (cc in cols) {
  a <- old[[cc]]; b <- new[[cc]]
  if (!identical(a, b)) mismatch <- c(mismatch, cc)
}

cat("\ncolumns compared:", length(cols), "\n")
cat("mismatched      :", length(mismatch), "\n")
if (length(mismatch)) { cat("MISMATCH IN:\n"); print(mismatch) }
cat(sprintf("elapsed old (get)   : %.1fs\n", t_old))
cat(sprintf("elapsed new (.SDcols): %.1fs\n", t_new))
cat(sprintf("speedup             : %.2fx\n", t_old / max(t_new, 1e-9)))

stopifnot(length(mismatch) == 0)
cat("\nVERIFIED: all 30 shift columns identical.\n")
