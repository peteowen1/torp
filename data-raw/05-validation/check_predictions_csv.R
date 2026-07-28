# Is the predictions CSV — the file Squiggle actually reads — in step with the parquet?
# =====================================================================================
# squiggle.com.au pulls torp's tips from predictions_<season>.csv on the
# torpdata `predictions` release. Everything else in this repo reads the
# PARQUET, so a CSV that fails to upload is invisible from the inside: the
# release updates, the pipeline goes green, our loaders see fresh data, and
# Squiggle keeps serving the previous round's tips.
#
# Observed 2026-07-28: parquet stamped 14:03, CSV still 05:53. Caught only by
# eyeballing asset timestamps while checking something else.
#
# save_to_release(also_csv = TRUE) warns on a CSV failure but deliberately does
# not abort -- the parquet has already uploaded by then, and discarding a good
# data release over a secondary copy is the wrong trade. This is the other half
# of that decision: a cheap, explicit way to ask "did it actually land?".
#
# Usage:  Rscript torp/data-raw/05-validation/check_predictions_csv.R [SEASON]
# Exit 0 = CSV matches the parquet, 1 = it does not.

suppressMessages({
  library(data.table); library(arrow); library(jsonlite)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

SEASON <- { a <- commandArgs(trailingOnly = TRUE)[1]
            if (is.na(a)) get_afl_season() else as.integer(a) }
REPO <- "peteowen1/torpdata"
BASE <- sprintf("https://github.com/%s/releases/download/predictions/", REPO)
# CSV is text at ~6 significant figures, so exact equality is unachievable.
TOL <- 1e-6

pq_name <- sprintf("predictions_%d.parquet", SEASON)
cs_name <- sprintf("predictions_%d.csv", SEASON)

# ---- 1. asset timestamps ----------------------------------------------------
assets <- tryCatch(vb_list_assets(REPO, "predictions"), error = function(e) NULL)
if (!is.null(assets)) {
  a <- as.data.table(assets)
  get1 <- function(nm) a[name == nm]
  pq_a <- get1(pq_name); cs_a <- get1(cs_name)
  cat("=== release assets ===\n")
  if (nrow(pq_a)) cat(sprintf("  %-28s %s\n", pq_name, pq_a$updated_at[1]))
  if (nrow(cs_a)) cat(sprintf("  %-28s %s\n", cs_name, cs_a$updated_at[1]))
  if (nrow(pq_a) && nrow(cs_a)) {
    lag <- as.numeric(difftime(as.POSIXct(pq_a$updated_at[1], tz = "UTC"),
                               as.POSIXct(cs_a$updated_at[1], tz = "UTC"), units = "mins"))
    cat(sprintf("  CSV is %.1f minutes %s than the parquet\n",
                abs(lag), if (lag > 0) "OLDER" else "newer"))
  }
}

# ---- 2. content, which is what actually matters -----------------------------
tf_pq <- file.path(tempdir(), pq_name); tf_cs <- file.path(tempdir(), cs_name)
download.file(paste0(BASE, pq_name), tf_pq, mode = "wb", quiet = TRUE)
download.file(paste0(BASE, cs_name), tf_cs, mode = "wb", quiet = TRUE)
p <- as.data.table(arrow::read_parquet(tf_pq))
cs <- fread(tf_cs)

cat(sprintf("\n=== content ===\nparquet %d rows x %d cols | csv %d rows x %d cols\n",
            nrow(p), ncol(p), nrow(cs), ncol(cs)))

bad <- FALSE
if (nrow(p) != nrow(cs)) {
  cli::cli_alert_danger("Row counts differ: parquet {nrow(p)} vs csv {nrow(cs)}")
  bad <- TRUE
} else {
  key <- intersect(c("week", "match_id"), names(p))
  setorderv(p, key); setorderv(cs, key)
  if (!identical(as.character(p$match_id), as.character(cs$match_id))) {
    cli::cli_alert_danger("The two files cover different matches.")
    bad <- TRUE
  }
  common <- intersect(names(p), names(cs))
  num <- common[vapply(common, function(x) is.numeric(p[[x]]) && is.numeric(cs[[x]]), logical(1))]
  worst <- vapply(num, function(x) max(abs(p[[x]] - cs[[x]]), na.rm = TRUE), numeric(1))
  cat("max |difference| across numeric columns:", format(max(worst), scientific = TRUE), "\n")
  if (max(worst) > TOL) {
    cli::cli_alert_danger("CSV and parquet disagree on {.val {names(worst)[worst > TOL]}}")
    bad <- TRUE
  }
  # as.numeric on both sides: arrow reads `week` as double, fread as integer, so
  # identical() reports a mismatch on values that are equal. A checker that
  # fails on correct data is worse than no checker -- it trains you to ignore
  # the one time it fires for real.
  rp <- sort(unique(as.numeric(p$week))); rc <- sort(unique(as.numeric(cs$week)))
  cat("rounds -- parquet:", paste(rp, collapse = ","), "| csv:", paste(rc, collapse = ","), "\n")
  if (!identical(rp, rc)) {
    cli::cli_alert_danger("Different rounds present -- the CSV is a different vintage.")
    bad <- TRUE
  }
}

cat("\n=== VERDICT ===\n")
if (bad) {
  cli::cli_alert_danger("The CSV Squiggle reads does NOT match the parquet. Re-run the predictions pipeline.")
  quit(status = 1)
}
cli::cli_alert_success("predictions_{SEASON}.csv matches the parquet -- Squiggle is reading current tips.")
quit(status = 0)
