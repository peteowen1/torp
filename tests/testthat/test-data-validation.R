# Tests for Data Validation Pipeline
# ===================================

test_that("validate_data_schema works for chains data", {
  # Create valid chains data
  valid_chains <- data.frame(
    match_id = "CD_M20240114201",
    period = 1L,
    period_seconds = 500.0,
    team_id = 1L,
    x = 50.0,
    y = 10.0,
    description = "Kick",
    player_name = "Test Player",
    utc_start_time = Sys.time()
  )
  
  # Should pass validation
  result <- validate_data_schema(valid_chains, "chains_data", strict = FALSE)
  expect_true(result$valid)
  expect_equal(length(result$issues), 0)
  
  # Test with missing column
  invalid_chains <- valid_chains[, -which(names(valid_chains) == "period")]
  result_invalid <- validate_data_schema(invalid_chains, "chains_data", strict = FALSE)
  expect_false(result_invalid$valid)
  expect_true(length(result_invalid$issues) > 0)
})

test_that("validate_data_schema works for model data", {
  # Create valid model data
  valid_model <- data.frame(
    match_id = "CD_M20240114201", 
    period = 1L,
    period_seconds = 500.0,
    goal_x = 50.0,
    y = 10.0,
    exp_pts = 2.5,
    team_id_mdl = 1L,
    home = 1L
  )
  
  result <- validate_data_schema(valid_model, "model_data_epv", strict = FALSE)
  expect_true(result$valid)
  
  # Test constraint violations
  invalid_model <- valid_model
  invalid_model$goal_x <- -50  # Should be >= 0
  result_invalid <- validate_data_schema(invalid_model, "model_data_epv", strict = FALSE)
  expect_false(result_invalid$valid)
})

test_that("validate_data_quality detects issues", {
  # Create data with quality issues
  poor_quality_data <- data.frame(
    x = c(1, 2, NA, NA, NA, 6),  # High missing rate
    y = c(1, 1, 1, 1, 1, 1),     # Zero variance
    z = c(1, 2, 3, 1000, 5, 6)   # Outlier
  )
  
  result <- validate_data_quality(poor_quality_data, "test")
  
  expect_true(is.list(result))
  expect_true("quality_score" %in% names(result))
  expect_true("issues" %in% names(result))
  expect_true(result$quality_score < 1.0)  # Should detect issues
  expect_true(length(result$issues) > 0)
})

test_that("analyze_missing_data works correctly", {
  # Create data with missing values
  test_data <- data.frame(
    complete_col = 1:10,
    partial_missing = c(1:8, NA, NA),
    high_missing = c(1:3, rep(NA, 7))
  )
  
  result <- analyze_missing_data(test_data)
  
  expect_true(is.list(result))
  expect_true("overall_missing_rate" %in% names(result))
  expect_true("missing_by_column" %in% names(result))
  expect_true("high_missing_cols" %in% names(result))
  
  # Should identify high missing column
  expect_equal(result$high_missing_cols, 1)
  expect_true("high_missing" %in% names(result$missing_by_column))
  expect_true(result$missing_by_column["high_missing"] > 0.5)
})

test_that("analyze_duplicates detects duplicate records", {
  # Create data with duplicates
  test_data <- data.frame(
    id = c(1, 2, 3, 2, 4),  # Row 2 and 4 are duplicates
    value = c("a", "b", "c", "b", "d")
  )
  
  result <- analyze_duplicates(test_data)
  
  expect_true(is.list(result))
  expect_true("n_exact_duplicates" %in% names(result))
  expect_true("duplicate_rate" %in% names(result))
  expect_equal(result$n_total_rows, 5)
  expect_equal(result$n_exact_duplicates, 1)  # One duplicate row
})

test_that("detect_outliers identifies extreme values", {
  # Create data with outlier
  test_data <- data.frame(
    normal_col = c(1:9, 100),  # 100 is an outlier
    another_col = 1:10
  )
  
  result <- detect_outliers(test_data)
  
  expect_true(is.list(result))
  expect_true("column_results" %in% names(result))
  expect_true("extreme_outliers" %in% names(result))
  
  # Should detect outlier in normal_col
  expect_true("normal_col" %in% names(result$column_results))
  expect_true(result$column_results$normal_col$extreme_outliers_iqr > 0)
})

test_that("validate_chains_quality detects AFL-specific issues", {
  # Create chains data with issues
  problem_chains <- data.frame(
    match_id = c("CD_M20240114201", "INVALID_ID", "CD_M20240114201"),
    period = c(1, 5, 2),  # Period 5 is invalid
    period_seconds = c(500, 1000, 3000),  # 3000 > 2000 is invalid
    x = c(50, 60, 200),  # 200 > 120 is out of bounds
    y = c(10, 20, 30)
  )
  
  result <- validate_chains_quality(problem_chains)
  
  expect_true(is.list(result))
  expect_true(length(result) > 0)  # Should find issues
  expect_true(any(grepl("Invalid match_id", unlist(result))))
  expect_true(any(grepl("Invalid period", unlist(result))))
})

test_that("validate_data_freshness works correctly", {
  # Create data with recent timestamps
  fresh_data <- data.frame(
    utc_start_time = Sys.time() - as.difftime(2, units = "days"),
    value = 1:5
  )
  
  expect_true(validate_data_freshness(fresh_data, "utc_start_time", max_age_days = 7))
  
  # Create stale data
  stale_data <- data.frame(
    utc_start_time = Sys.time() - as.difftime(10, units = "days"),
    value = 1:5
  )
  
  expect_false(validate_data_freshness(stale_data, "utc_start_time", max_age_days = 7))
})

test_that("calculate_quality_score returns reasonable scores", {
  # No issues should give perfect score
  no_issues <- list()
  perfect_data <- data.frame(x = 1:10)
  score_perfect <- calculate_quality_score(perfect_data, no_issues)
  expect_equal(score_perfect, 1.0)
  
  # Major issues should reduce score significantly
  major_issues <- list(
    high_missing = "50% missing values",
    temporal_inconsistency = "Time data inconsistent"
  )
  score_poor <- calculate_quality_score(perfect_data, major_issues)
  expect_true(score_poor < 0.5)
  
  # Score should be bounded between 0 and 1
  many_issues <- list(
    empty_data = "No data",
    high_missing = "Missing",
    outliers = "Outliers",
    duplicates = "Duplicates"
  )
  score_bounded <- calculate_quality_score(perfect_data, many_issues)
  expect_true(score_bounded >= 0 && score_bounded <= 1)
})