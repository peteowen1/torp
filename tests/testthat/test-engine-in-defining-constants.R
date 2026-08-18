# The drift guard must be able to see an engine flip.
#
# WHY THIS FILE EXISTS. check_vintage_alignment()'s third check compares the
# manifest's recorded defining_constants against the live ones, to catch a
# rating-defining constant being edited without bumping RATING_VINTAGE. Until
# 2026-08-18 that list recorded EPR decays, priors, the weight and the position
# map -- but not EPV_ENGINE, EPV3_CHANNELS or EPV3_POINTS_SCALE. So the single
# largest rating-defining change available, swapping the EPV engine, produced
# byte-identical defining_constants and sailed straight through the guard whose
# whole purpose is to stop that.
#
# Found staging v3 for promotion, with the engine already flipped to v3 in the
# constants and the published manifest showing no difference at all.

test_that("the EPV engine constants are recorded as rating-defining", {
  k <- names(.rating_defining_constants())
  expect_true(all(c("EPV_ENGINE", "EPV3_CHANNELS", "EPV3_POINTS_SCALE") %in% k))
})

test_that("an engine flip is visible as drift", {
  # The guard compares after a JSON round-trip, so this does too -- a constant
  # that survives R but not serialisation would fail open in production.
  live <- .rating_defining_constants()
  as_manifest <- function(x) jsonlite::fromJSON(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), simplifyVector = FALSE)

  expect_length(.diff_defining_constants(as_manifest(live), live), 0)

  flipped <- live
  flipped$EPV_ENGINE <- if (identical(live$EPV_ENGINE, "v3")) "v2" else "v3"
  diffs <- .diff_defining_constants(as_manifest(flipped), live)
  expect_true("EPV_ENGINE" %in% names(diffs))
})

test_that("a channel-count change is visible as drift", {
  # 3-channel vs 4-channel v3 price contests differently enough to move every
  # ruck in the file, so it has to count as a different vintage too.
  live <- .rating_defining_constants()
  as_manifest <- function(x) jsonlite::fromJSON(
    jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), simplifyVector = FALSE)
  bumped <- live
  bumped$EPV3_CHANNELS <- as.integer(live$EPV3_CHANNELS) + 1L
  expect_true("EPV3_CHANNELS" %in% names(.diff_defining_constants(as_manifest(bumped), live)))
})
