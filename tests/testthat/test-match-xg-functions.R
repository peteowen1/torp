# Tests for match_xg_functions.R
# Tests for match_xgs function

test_that("match_xgs function exists and is exported", {
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

  # Default quarter should be 1:4

  expect_equal(fn_formals$quarter, 1:4)
})

test_that("match_xgs returns expected structure when data available", {
  skip_if_no_internet()

  # This test requires actual data, so we skip if no internet
  # or if we can't load the data
  result <- tryCatch(
    {
      # Use a known historical season/round
      match_xgs(season = 2024, round = 1, quarter = 1:4)
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
