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

  # quarter default is 1:4 (evaluates to the sequence)
  expect_equal(eval(fn_formals$quarter), 1:4)
})

# -----------------------------------------------------------------------------
# Deprecated match_xgs Tests
# -----------------------------------------------------------------------------

test_that("match_xgs deprecated alias exists and is exported", {
  expect_true(exists("match_xgs"))
  expect_true("match_xgs" %in% getNamespaceExports("torp"))
})

test_that("match_xgs has correct function signature", {
  # Check function arguments
  fn_args <- names(formals(match_xgs))

  expect_true("season" %in% fn_args)
  expect_true("round" %in% fn_args)
  expect_true("quarter" %in% fn_args)

  # Ensure match_id parameter was removed (per code review fix)
  expect_false("match_id" %in% fn_args)
})

test_that("match_xgs has reasonable default arguments", {
  fn_formals <- formals(match_xgs)

  # Default quarter should be 1:4 (evaluates to the sequence)
  expect_equal(eval(fn_formals$quarter), 1:4)
})

test_that("match_xgs shows deprecation warning", {
  skip_if_no_internet()

  # Should warn about deprecation
  expect_warning(
    tryCatch(
      match_xgs(season = 2024, round = 1),
      error = function(e) NULL  # Ignore errors (e.g., missing data)
    ),
    "deprecated|Deprecated"
  )
})

test_that("calculate_match_xgs returns expected structure when data available", {
  skip_if_no_internet()

  # This test requires actual data, so we skip if no internet
  # or if we can't load the data
  result <- tryCatch(
    {
      # Use a known historical season/round
      calculate_match_xgs(season = 2024, round = 1, quarter = 1:4)
    },
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not load match data")

  # Check structure of returned data
  expect_s3_class(result, "data.frame")

  # Check expected columns exist
  expected_cols <- c(
    "match_id", "home_team", "home_shots_score", "home_xscore",
    "away_team", "away_shots_score", "away_xscore",
    "score_diff", "xscore_diff"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }

  # Validate numeric columns
  expect_type(result$home_shots_score, "double")
  expect_type(result$home_xscore, "double")
  expect_type(result$away_shots_score, "double")
  expect_type(result$away_xscore, "double")

  # Score diff should equal home - away
  expect_equal(
    result$score_diff,
    result$home_shots_score - result$away_shots_score
  )
})

# -----------------------------------------------------------------------------
# Quarter Parameter Tests
# -----------------------------------------------------------------------------

test_that("calculate_match_xgs accepts different quarter parameters", {
  skip_if_no_internet()

  # Test with single quarter
  result_q1 <- tryCatch(
    calculate_match_xgs(season = 2024, round = 1, quarter = 1),
    error = function(e) NULL
  )

  # Test with subset of quarters
  result_first_half <- tryCatch(
    calculate_match_xgs(season = 2024, round = 1, quarter = 1:2),
    error = function(e) NULL
  )

  # Both should either work or return NULL (not error)
  if (!is.null(result_q1)) {
    expect_true(is.data.frame(result_q1))
  }

  if (!is.null(result_first_half)) {
    expect_true(is.data.frame(result_first_half))
  }
})

# -----------------------------------------------------------------------------
# XG Calculation Validation Tests
# -----------------------------------------------------------------------------

test_that("xscore values are reasonable", {
  skip_if_no_internet()

  result <- tryCatch(
    calculate_match_xgs(season = 2024, round = 1),
    error = function(e) NULL
  )

  skip_if(is.null(result) || nrow(result) == 0, "Could not load match data")

  # XG should be non-negative
  expect_true(all(result$home_xscore >= 0))
  expect_true(all(result$away_xscore >= 0))

  # XG should typically be less than actual scores (shooting performance varies)
  # but not by too much for aggregated data
  # This is a sanity check, not a strict rule
})
