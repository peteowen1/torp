# Tests for match_xg_functions.R
# Tests for calculate_match_xgs function

# -----------------------------------------------------------------------------
# calculate_match_xgs Tests
# -----------------------------------------------------------------------------

test_that("calculate_match_xgs function exists and is exported", {
  expect_true(exists("calculate_match_xgs"))
  expect_true("calculate_match_xgs" %in% getNamespaceExports("torp"))
})

test_that("calculate_match_xgs has correct function signature", {
  fn_args <- names(formals(calculate_match_xgs))

  expect_true("season" %in% fn_args)
  expect_true("round" %in% fn_args)
  expect_true("quarter" %in% fn_args)
})

test_that("calculate_match_xgs has reasonable default arguments", {
  fn_formals <- formals(calculate_match_xgs)

  # nolint start: eval is safe here - only evaluating function formals
  expect_equal(eval(fn_formals$quarter), 1:4)
  # nolint end
})

# -----------------------------------------------------------------------------
# Deprecated match_xgs Tests
# -----------------------------------------------------------------------------

test_that("match_xgs deprecated alias exists and is exported", {
  expect_true(exists("match_xgs"))
  expect_true("match_xgs" %in% getNamespaceExports("torp"))
})

test_that("match_xgs has correct function signature", {
  fn_args <- names(formals(match_xgs))

  expect_true("season" %in% fn_args)
  expect_true("round" %in% fn_args)
  expect_true("quarter" %in% fn_args)

  # Ensure match_id parameter was removed (per code review fix)
  expect_false("match_id" %in% fn_args)
})

test_that("match_xgs has reasonable default arguments", {
  fn_formals <- formals(match_xgs)

  # nolint start: eval is safe here - only evaluating function formals
  expect_equal(eval(fn_formals$quarter), 1:4)
  # nolint end
})

# -----------------------------------------------------------------------------
# Network-Dependent Tests (reuse shared data from helper-test-data.R)
# -----------------------------------------------------------------------------

test_that("match_xgs shows deprecation warning", {
  skip_if(is.null(.shared$match_xgs), "Could not load match data")

  expect_warning(
    match_xgs(season = 2024, round = 1),
    "deprecated|Deprecated"
  )
})

test_that("calculate_match_xgs returns expected structure and valid values", {
  skip_if(is.null(.shared$match_xgs) || nrow(.shared$match_xgs) == 0, "Could not load match data")

  # Check structure of returned data
  expect_s3_class(.shared$match_xgs, "data.frame")

  expected_cols <- c(
    "match_id", "home_team", "home_shots_score", "home_xscore",
    "away_team", "away_shots_score", "away_xscore",
    "score_diff", "xscore_diff"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(.shared$match_xgs), info = paste("Missing column:", col))
  }

  # Validate numeric columns
  expect_type(.shared$match_xgs$home_shots_score, "double")
  expect_type(.shared$match_xgs$home_xscore, "double")
  expect_type(.shared$match_xgs$away_shots_score, "double")
  expect_type(.shared$match_xgs$away_xscore, "double")

  # Score diff should equal home - away
  expect_equal(
    .shared$match_xgs$score_diff,
    .shared$match_xgs$home_shots_score - .shared$match_xgs$away_shots_score
  )

  # XG should be non-negative
  expect_true(all(.shared$match_xgs$home_xscore >= 0))
  expect_true(all(.shared$match_xgs$away_xscore >= 0))
})

test_that("calculate_match_xgs accepts subset of quarters", {
  skip_if(is.null(.shared$match_xgs), "Could not load match data")

  # Test with first half only - reuses cached underlying data
  result_first_half <- calculate_match_xgs(season = 2024, round = 1, quarter = 1:2)

  expect_true(is.data.frame(result_first_half))

  # First half scores should be <= full game scores
  if (nrow(.shared$match_xgs) > 0) {
    expect_true(all(result_first_half$home_shots_score <= .shared$match_xgs$home_shots_score))
  }
})
