# Guards against the vendored copies of versebus.R (this file's own header
# comment: "canonical copy: torpverse/torp/R/versebus.R") drifting apart
# silently -- torpmodels was missing `.vb_retry()` wiring in vb_read_manifest()
# / vb_download() until 2026-07-22 despite the helper itself landing
# 2026-07-21 (e0ccdb3), because that commit ported `.vb_retry()` for its own
# download_model_from_release() use without also re-syncing the two
# already-vendored call sites that had drifted from torp's 2751da7.
#
# Local-dev-only: CI (test-package.yml) runs rcmdcheck against a single
# checked-out repo with no sibling `torp`/`torpmodels` beside it -- skip
# rather than fail when the sibling can't be found.
#
# Compares function-by-function (name + deparsed body) rather than whole-file
# text, since the two copies legitimately differ in file-level comments. As
# of 2026-07-22 a full file diff showed the two copies byte-identical, so the
# shared set below is every top-level function in versebus.R. If a function
# is ever intentionally repo-specialized, remove it from this vector with a
# comment explaining why, rather than silently letting the identity check
# start failing. To update after a real change: diff the two R/versebus.R
# files, re-sync any unintentional drift, then edit this vector to match.
SHARED_VERSEBUS_FUNCTIONS <- c(
  ".vb_now_utc", ".vb_generation_stamp", ".vb_split_repo", ".vb_abort",
  "vb_classify_error", "vb_sha256", "vb_asset_entry", "vb_producer_info",
  "vb_write_manifest", "vb_atomic_write", "vb_guard_accumulate",
  "vb_list_assets", "vb_confirm_absent", "vb_read_manifest",
  "vb_read_prev_manifest", ".vb_manifest_entry_for", ".vb_check_parquet_magic",
  ".vb_retry", "vb_download", "vb_cache_validate", "vb_generation",
  "vb_publish", ".vb_merge_entries"
)

# Parse a versebus.R file into name -> deparsed function-body text, for every
# top-level `name <- function(...) ...` assignment.
.parse_versebus_functions <- function(path) {
  exprs <- parse(path, keep.source = FALSE)
  out <- list()
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
        length(e) == 3 && is.name(e[[2]]) &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      out[[as.character(e[[2]])]] <- paste(deparse(e[[3]]), collapse = "\n")
    }
  }
  out
}

# `rel_path` is relative to this package's root (e.g. "R/versebus.R" for our
# own copy, "../torpmodels/R/versebus.R" for the sibling's). Tries a few
# plausible working-directory depths so it works whether tests run via
# devtools::test() (cwd = pkg root), testthat::test_dir() (cwd = tests/), or
# an interactive testthat::test_file() (cwd = tests/testthat/).
.locate_from_pkg_root <- function(rel_path) {
  candidates <- file.path(c(".", "..", file.path("..", "..")), rel_path)
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) return(NULL)
  normalizePath(hit[[1]])
}

test_that("versebus.R shared function block matches torpmodels' vendored copy", {
  sibling <- .locate_from_pkg_root(file.path("..", "torpmodels", "R", "versebus.R"))
  if (is.null(sibling)) {
    testthat::skip("sibling repo ../torpmodels not found alongside this checkout -- local-dev-only guard, skipped on CI")
  }

  own <- .locate_from_pkg_root(file.path("R", "versebus.R"))
  if (is.null(own)) {
    testthat::skip("could not locate this package's own R/versebus.R (unexpected test working directory)")
  }

  mine <- .parse_versebus_functions(own)
  theirs <- .parse_versebus_functions(sibling)

  missing_here <- setdiff(SHARED_VERSEBUS_FUNCTIONS, names(mine))
  missing_there <- setdiff(SHARED_VERSEBUS_FUNCTIONS, names(theirs))
  expect_length(missing_here, 0)
  expect_length(missing_there, 0)

  for (fn in SHARED_VERSEBUS_FUNCTIONS) {
    expect_identical(
      mine[[fn]], theirs[[fn]],
      info = paste0(
        "versebus.R::", fn, "() has drifted from torpmodels' vendored copy -- ",
        "re-sync the shared logic, or if the difference is intentional, ",
        "remove it from SHARED_VERSEBUS_FUNCTIONS with a comment explaining why"
      )
    )
  }
})
