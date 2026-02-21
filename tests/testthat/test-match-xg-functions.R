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
  # nolint start: eval is safe here — only evaluating function formals
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
  # nolint start: eval is safe here — only evaluating function formals
  expect_equal(eval(fn_formals$quarter), 1:4)
  # nolint end
})

# -----------------------------------------------------------------------------
# Shared Test Data (loaded once for network-dependent tests)
# -----------------------------------------------------------------------------

.xg_can_load <- !identical(Sys.getenv("NOT_CRAN"), "") || interactive()

if (.xg_can_load && curl::has_internet()) {
  .xg_result <- tryCatch(
    calculate_match_xgs(season = 2024, round = 1, quarter = 1:4),
    error = function(e) {
      message("XG data load failed: ", conditionMessage(e))
      NULL
    }
  )
} else {
  .xg_result <- NULL
}

# -----------------------------------------------------------------------------
# Network-Dependent Tests (reuse cached result)
# -----------------------------------------------------------------------------

test_that("match_xgs shows deprecation warning", {
  skip_if_no_internet()
  skip_if(is.null(.xg_result), "Could not load match data")

  # Should warn about deprecation
  expect_warning(
    tryCatch(
      match_xgs(season = 2024, round = 1),
      error = function(e) NULL  # Ignore errors (e.g., missing data)
    ),
    "deprecated|Deprecated"
  )
})

test_that("calculate_match_xgs returns expected structure and valid values", {
  skip_if(is.null(.xg_result) || nrow(.xg_result) == 0, "Could not load match data")

  # Check structure of returned data
  expect_s3_class(.xg_result, "data.frame")

  # Check expected columns exist
  expected_cols <- c(
    "match_id", "home_team", "home_shots_score", "home_xscore",
    "away_team", "away_shots_score", "away_xscore",
    "score_diff", "xscore_diff"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(.xg_result), info = paste("Missing column:", col))
  }

  # Validate numeric columns
  expect_type(.xg_result$home_shots_score, "double")
  expect_type(.xg_result$home_xscore, "double")
  expect_type(.xg_result$away_shots_score, "double")
  expect_type(.xg_result$away_xscore, "double")

  # Score diff should equal home - away
  expect_equal(
    .xg_result$score_diff,
    .xg_result$home_shots_score - .xg_result$away_shots_score
  )

  # XG should be non-negative
  expect_true(all(.xg_result$home_xscore >= 0))
  expect_true(all(.xg_result$away_xscore >= 0))
})

test_that("calculate_match_xgs accepts subset of quarters", {
  skip_if_no_internet()

  # Test with first half only
  result_first_half <- tryCatch(
    calculate_match_xgs(season = 2024, round = 1, quarter = 1:2),
    error = function(e) NULL
  )

  if (!is.null(result_first_half)) {
    expect_true(is.data.frame(result_first_half))

    # First half scores should be <= full game scores
    if (!is.null(.xg_result) && nrow(.xg_result) > 0) {
      expect_true(all(result_first_half$home_shots_score <= .xg_result$home_shots_score))
    }
  }
})
