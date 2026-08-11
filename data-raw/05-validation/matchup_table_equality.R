# Does the matchup table survive the .freeze_match_state() migration unchanged?
# =============================================================================
#
# `build_matchup_table()` feeds per-tie finals pricing to inthegame-blog. This
# script captures its 612-row output so a BEFORE and an AFTER can be compared
# across a code change, because the migration onto `build_prediction_state()`
# is the one step in this refactor that could move served numbers.
#
# Usage:
#   Rscript data-raw/05-validation/matchup_table_equality.R capture <label>
#   Rscript data-raw/05-validation/matchup_table_equality.R compare <a> <b>
#
# Capture BEFORE editing anything. There is no way to reconstruct the baseline
# afterwards.

suppressMessages(devtools::load_all("."))

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else "capture"
OUT <- file.path(Sys.getenv("TEMP", tempdir()), "matchup-equality")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

if (mode == "capture") {
  label <- if (length(args) >= 2) args[[2]] else "baseline"
  t0 <- Sys.time()
  tbl <- build_matchup_table()
  mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2)

  # Sanity-check the load before trusting it (torpverse house rule).
  cat("rows:", nrow(tbl), " cols:", ncol(tbl), " built in", mins, "min\n")
  cat("columns:", paste(names(tbl), collapse = ", "), "\n")
  num <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
  for (nm in num) {
    v <- tbl[[nm]]
    cat(sprintf("  %-18s min %10.4f  mean %10.4f  max %10.4f  NA %d\n",
                nm, min(v, na.rm = TRUE), mean(v, na.rm = TRUE),
                max(v, na.rm = TRUE), sum(is.na(v))))
  }
  if (nrow(tbl) == 0) stop("empty matchup table -- refusing to save a useless baseline")

  f <- file.path(OUT, paste0(label, ".rds"))
  saveRDS(tbl, f)
  cat("\nsaved:", f, "\n")

} else if (mode == "compare") {
  a <- readRDS(file.path(OUT, paste0(args[[2]], ".rds")))
  b <- readRDS(file.path(OUT, paste0(args[[3]], ".rds")))
  cat("comparing", args[[2]], "vs", args[[3]], "\n\n")

  cat("rows:", nrow(a), "vs", nrow(b), "\n")
  cat("cols:", ncol(a), "vs", ncol(b), "\n")
  only_a <- setdiff(names(a), names(b)); only_b <- setdiff(names(b), names(a))
  cat("only in", args[[2]], ":", if (length(only_a)) paste(only_a, collapse = ", ") else "(none)", "\n")
  cat("only in", args[[3]], ":", if (length(only_b)) paste(only_b, collapse = ", ") else "(none)", "\n\n")

  # Sort on a UNIQUE key so a reordering is not reported as a value change.
  # `venue` alone is not unique (it is "home"/"MCG", 2 values over 612 rows) --
  # aligning on it would leave row order inside each group unconstrained and
  # any diff it produced would be untrustworthy. Assert uniqueness rather than
  # hoping.
  key <- intersect(c("home", "away", "venue"), names(a))
  if (length(key) > 0 && nrow(a) == nrow(b)) {
    stopifnot(nrow(unique(a[key])) == nrow(a),
              nrow(unique(b[key])) == nrow(b))
    a <- a[do.call(order, a[key]), , drop = FALSE]
    b <- b[do.call(order, b[key]), , drop = FALSE]
    stopifnot(identical(a[key], b[key]))
    cat("aligned on unique key:", paste(key, collapse = " + "), "\n\n")
  } else {
    cat("!! no unique key found -- comparing in existing row order\n\n")
  }

  # `updated` is a build timestamp. It differs on every run by construction,
  # including two runs of unchanged code, so counting it as a difference makes
  # the verdict useless. Reported separately instead of silently dropped.
  ts <- intersect("updated", names(a))
  if (length(ts)) {
    cat("timestamp column (excluded from the verdict):", ts,
        "--", format(a[[ts]][1]), "vs", format(b[[ts]][1]), "\n\n")
  }

  if (nrow(a) != nrow(b)) {
    cat("VERDICT: DIFFERENT -- row counts differ\n"); quit(status = 0)
  }

  worst <- 0
  ndiff_total <- 0
  for (nm in setdiff(intersect(names(a), names(b)), ts)) {
    x <- a[[nm]]; y <- b[[nm]]
    if (identical(x, y)) next
    n <- sum(!(is.na(x) & is.na(y)) & (is.na(x) | is.na(y) | x != y), na.rm = TRUE)
    ndiff_total <- ndiff_total + n
    m <- if (is.numeric(x) && is.numeric(y)) max(abs(x - y), na.rm = TRUE) else NA_real_
    if (!is.na(m)) worst <- max(worst, m)
    cat(sprintf("  %-18s %5d rows differ   max|diff| = %s\n", nm, n,
                if (is.na(m)) "(non-numeric)" else format(m, digits = 8)))
  }

  cat("\n=========================================================\n")
  if (ndiff_total == 0 && length(only_a) == 0 && length(only_b) == 0) {
    cat("VERDICT: IDENTICAL -- the migration did not move a single served\n")
    cat("number across all", nrow(a), "rows.\n")
  } else {
    cat("VERDICT: DIFFERENT --", ndiff_total, "differing cells, worst |diff| =",
        format(worst, digits = 8), "\n")
    cat("These are numbers the blog serves. Do not ship until explained.\n")
  }
  cat("=========================================================\n")

} else {
  stop("mode must be 'capture' or 'compare'")
}
