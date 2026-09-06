# Does the face-validity check actually decide anything?
#
# A check is only worth having if it separates the changes we SHIPPED from the
# ones we REJECTED. Tuning it against failures alone would produce something that
# rejects good work too, and this panel's entire problem to date has been rows
# that measure something without deciding anything.
#
# Three real cases from 2026-08-06, all with saved leaderboards:
#
#   centring alone   SHIPPED   must PASS
#     position mix identical, Spearman 0.9636, movers explicable
#   combined arm     REJECTED  must FAIL
#     key defenders 5 of the top 40 -> 1, Spearman 0.8673
#   ws26 calibration REJECTED  must FAIL
#     Sean Darcy 125 -> 5, Mason Cox 506 -> 79, six of eight risers rucks
#
# If any case comes out the wrong way the thresholds are wrong and must be
# changed HERE, with the new numbers shown, rather than quietly in the function.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "face_validity_verify.txt"), split = TRUE)
cat("=== Does the face-validity check separate ship from reject? ===\nrun at",
    format(Sys.time()), "\n")

latest <- function(x) {
  x <- as.data.table(x)
  s <- max(x$season, na.rm = TRUE)
  y <- x[season == s][, .SD[which.max(round)], by = player_id]
  y[is.finite(epr)]
}
rt <- function(f) {
  p <- file.path(OUT_DIR, paste0(f, ".parquet"))
  if (!file.exists(p)) return(NULL)
  latest(read_parquet(p))
}

results <- list()
run_case <- function(name, before, after, expect) {
  cat("\n##########", name, " -- must", expect, "##########\n")
  if (is.null(before) || is.null(after)) {
    cat("  SKIPPED: a rating frame is missing.\n"); return(invisible(NULL))
  }
  fv <- face_validity(before, after)
  print(fv)
  got <- attr(fv, "overall")
  ok <- identical(toupper(got), toupper(expect))
  cat(sprintf("  >>> expected %s, got %s -- %s\n", expect, got,
              if (ok) "CORRECT" else "*** WRONG, THRESHOLDS NEED CHANGING ***"))
  results[[name]] <<- ok
  invisible(fv)
}

# --- case 1: the centring fixes, which shipped -------------------------------
run_case("centring alone (SHIPPED)",
         rt("centring_rt_before"), rt("centring_rt_after"), "pass")

# --- case 2: the combined arm, rejected today --------------------------------
run_case("combined arm (REJECTED)",
         rt("comb_rt_prod_global"), rt("comb_rt_comb_percal"), "FAIL")

# --- case 3: the ws26 per-channel calibration, rejected ----------------------
run_case("ws26 per-channel (REJECTED)",
         rt("v2cal_rt_global"), rt("v2cal_rt_percal"), "FAIL")

cat("\n########## VERDICT ##########\n")
if (!length(results)) {
  cat("No cases ran -- the saved rating frames are absent. The check is UNVERIFIED.\n")
} else {
  for (nm in names(results))
    cat(sprintf("  %-32s %s\n", nm, if (results[[nm]]) "correct" else "WRONG"))
  cat(sprintf("\n  %d of %d cases separated correctly.\n",
              sum(unlist(results)), length(results)))
  if (all(unlist(results))) {
    cat("  The check decides. Wire it into the panel.\n")
  } else {
    cat("  The check does NOT decide. Do not wire it in until the thresholds\n")
    cat("  separate all three -- a gate that rejects the change we shipped is\n")
    cat("  worse than no gate, because it will be ignored.\n")
  }
}
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
