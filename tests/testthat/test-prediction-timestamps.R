# generated_utc on locked predictions.
#
# Motivation: the locked-predictions release records WHAT was predicted but not
# WHEN it was computed, so "is this row genuinely pre-game?" could only be
# answered by reconstructing against Squiggle's submitted tips. That is how
# three rounds of stored-vs-submitted divergence (13, 19, 20 in 2026) became an
# open forensic question rather than a lookup.
#
# With the stamp, the check is `generated_utc < start_time`, and
# .warn_post_hoc_predictions() surfaces violations at write time.

.pred_frame <- function(generated, start, week = 13) {
  data.frame(
    week = week,
    match_id = paste0("M", seq_along(generated)),
    utc_start_time = start,
    generated_utc = generated,
    pred_margin = 10,
    stringsAsFactors = FALSE
  )
}

test_that("a prediction computed before its game start is not flagged", {
  df <- .pred_frame(generated = "2026-06-04T02:00:00Z",
                    start     = "2026-06-04T09:30:00.000+0000")
  expect_silent(n <- torp:::.warn_post_hoc_predictions(df))
  expect_equal(n, 0L)
})

test_that("a prediction computed AFTER its game start is flagged, naming the round", {
  # The literal defect: a retrodiction sitting in the locked release.
  df <- .pred_frame(generated = "2026-06-05T04:00:00Z",
                    start     = "2026-06-04T09:30:00.000+0000", week = 13)
  expect_warning(n <- torp:::.warn_post_hoc_predictions(df), "computed AFTER")
  expect_warning(torp:::.warn_post_hoc_predictions(df), "13")
  expect_equal(suppressWarnings(torp:::.warn_post_hoc_predictions(df)), 1L)
})

test_that("rows with no stamp are skipped, not treated as post-hoc", {
  # Every row published before stamping existed has NA here. Treating absent as
  # post-hoc would warn on all of them forever and train everyone to ignore it.
  df <- .pred_frame(generated = NA_character_,
                    start     = "2026-06-04T09:30:00.000+0000")
  expect_silent(n <- torp:::.warn_post_hoc_predictions(df))
  expect_equal(n, 0L)
})

test_that("a frame with no generated_utc column at all is a no-op", {
  df <- data.frame(week = 1, match_id = "M1",
                   utc_start_time = "2026-06-04T09:30:00.000+0000",
                   stringsAsFactors = FALSE)
  expect_silent(n <- torp:::.warn_post_hoc_predictions(df))
  expect_equal(n, 0L)
})

test_that("mixed frames report only the offending rows", {
  df <- .pred_frame(
    generated = c("2026-06-04T02:00:00Z", "2026-06-05T04:00:00Z", NA_character_),
    start     = rep("2026-06-04T09:30:00.000+0000", 3)
  )
  expect_equal(suppressWarnings(torp:::.warn_post_hoc_predictions(df)), 1L)
})

test_that("an unparseable stamp is skipped rather than crashing the upload", {
  # Fail-safe direction: a malformed stamp must not abort a release whose data
  # is otherwise fine. It simply cannot be checked.
  df <- .pred_frame(generated = "not-a-timestamp",
                    start     = "2026-06-04T09:30:00.000+0000")
  expect_silent(n <- torp:::.warn_post_hoc_predictions(df))
  expect_equal(n, 0L)
})
