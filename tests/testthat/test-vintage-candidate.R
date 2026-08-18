# A CANDIDATE vintage write must be allowed while a drifted CANONICAL write is
# still refused.
#
# Why this file exists. check_vintage_alignment() ran unconditionally and always
# judged the run against canonical, so once any rating-defining constant changed
# the pipeline refused to write at all -- including to a candidate label that
# never touches canonical. That made the guard block the exact remedy its own
# mismatch error recommends ("publish it as a candidate vintage first"), so a
# constants change could not be staged. Found 2026-08-18 staging EPV v3.
#
# The fix must not become a bypass, so both directions are asserted here.

mk_manifest <- function(canonical = RATING_VINTAGE) {
  entry <- .build_rating_vintage_entry(n_rows = 10, version = RATING_VINTAGE,
                                       file = "torp_ratings.parquet",
                                       generated_utc = "2026-08-18T00:00:00Z")
  j <- jsonlite::toJSON(
    list(canonical = canonical,
         vintages = stats::setNames(list(entry), RATING_VINTAGE)),
    auto_unbox = TRUE, null = "null")
  jsonlite::fromJSON(j, simplifyVector = FALSE)
}

test_that("a candidate write is permitted even when constants have drifted", {
  manifest <- mk_manifest()
  # Plant a drift the canonical path must reject: pretend the manifest recorded a
  # different prior rate from the one now live.
  nm <- names(manifest$vintages[[RATING_VINTAGE]]$defining_constants)[1]
  manifest$vintages[[RATING_VINTAGE]]$defining_constants[[nm]] <- "definitely-not-the-live-value"

  # Canonical: still refused. This is the 2026-07-27/28 protection and it stays.
  expect_error(
    check_vintage_alignment(strict = TRUE, manifest = manifest),
    class = "torp_error_vintage_constants_drift"
  )

  # Candidate: allowed, because it writes torp_ratings_<label>.parquet and never
  # touches canonical.
  res <- check_vintage_alignment(strict = TRUE, manifest = manifest,
                                 candidate = "v99")
  expect_identical(res$candidate, "v99")
  expect_identical(res$canonical, manifest$canonical)
})

test_that("a candidate may not wear the canonical label", {
  manifest <- mk_manifest()
  # A canonical write flying a candidate flag is the one thing the relaxation
  # must never wave through.
  expect_error(
    check_vintage_alignment(strict = TRUE, manifest = manifest,
                            candidate = manifest$canonical),
    class = "torp_error_vintage_candidate_is_canonical"
  )
})

test_that("an unreadable manifest still refuses a candidate write", {
  # The candidate branch sits AFTER the manifest check on purpose: no manifest
  # means no provenance, and that licenses nothing.
  expect_error(
    check_vintage_alignment(strict = TRUE, manifest = NULL, candidate = "v99"),
    class = "torp_error_vintage_manifest_unreadable"
  )
})
