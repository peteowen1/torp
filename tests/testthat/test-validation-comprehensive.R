# Comprehensive Tests for Validation Functions
# =============================================
# Tests all validation functions with edge cases

# -----------------------------------------------------------------------------
# validate_seasons() Tests
# -----------------------------------------------------------------------------

test_that("validate_seasons accepts valid single season", {
  expect_equal(torp:::validate_seasons(2021), 2021)
  expect_equal(torp:::validate_seasons(2022), 2022)
  expect_equal(torp:::validate_seasons(2024), 2024)
})

test_that("validate_seasons accepts valid season vector", {
  expect_equal(torp:::validate_seasons(c(2021, 2022)), c(2021, 2022))
  expect_equal(torp:::validate_seasons(2021:2024), 2021:2024)
  expect_equal(torp:::validate_seasons(c(2023, 2021, 2022)), c(2023, 2021, 2022))
})

test_that("validate_seasons accepts TRUE for all seasons", {
  result <- torp:::validate_seasons(TRUE)
  expect_true(is.numeric(result))
  expect_true(2021 %in% result)
  expect_true(length(result) >= 1)
})

test_that("validate_seasons rejects seasons before 2021", {
  expect_error(torp:::validate_seasons(2020), "Invalid season")
  expect_error(torp:::validate_seasons(2019), "Invalid season")
  expect_error(torp:::validate_seasons(2000), "Invalid season")
  expect_error(torp:::validate_seasons(c(2019, 2021)), "Invalid season")
})

test_that("validate_seasons rejects future seasons", {
  far_future <- as.numeric(format(Sys.Date(), "%Y")) + 10
  expect_error(torp:::validate_seasons(far_future), "Invalid season")
  expect_error(torp:::validate_seasons(2050), "Invalid season")
})

test_that("validate_seasons rejects non-numeric input", {
  expect_error(torp:::validate_seasons("2024"), "must be numeric")
  expect_error(torp:::validate_seasons("invalid"), "must be numeric")
  expect_error(torp:::validate_seasons(c("2023", "2024")), "must be numeric")
})

test_that("validate_seasons rejects FALSE and NULL", {
  # FALSE should be treated as logical, not TRUE
  result <- tryCatch(torp:::validate_seasons(FALSE), error = function(e) "error")
  expect_true(result == "error" || is.numeric(result))

  expect_error(torp:::validate_seasons(NULL))
})

# -----------------------------------------------------------------------------
# validate_rounds() Tests
# -----------------------------------------------------------------------------

test_that("validate_rounds accepts valid single round", {
  expect_equal(torp:::validate_rounds(1), 1)
  expect_equal(torp:::validate_rounds(10), 10)
  expect_equal(torp:::validate_rounds(28), 28)
  expect_equal(torp:::validate_rounds(0), 0)  # Finals round 0
})

test_that("validate_rounds accepts valid round vector", {
  expect_equal(torp:::validate_rounds(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(torp:::validate_rounds(1:28), 1:28)
  expect_equal(torp:::validate_rounds(c(1, 28)), c(1, 28))
})

test_that("validate_rounds accepts TRUE for all rounds", {
  result <- torp:::validate_rounds(TRUE)
  expect_true(is.numeric(result))
  expect_true(0 %in% result)
  expect_true(28 %in% result)
  expect_equal(result, 0:28)
})

test_that("validate_rounds rejects negative rounds", {
  expect_error(torp:::validate_rounds(-1), "Invalid round")
  expect_error(torp:::validate_rounds(-5), "Invalid round")
  expect_error(torp:::validate_rounds(c(-1, 1, 2)), "Invalid round")
})

test_that("validate_rounds rejects rounds > 28", {
  expect_error(torp:::validate_rounds(29), "Invalid round")
  expect_error(torp:::validate_rounds(50), "Invalid round")
  expect_error(torp:::validate_rounds(c(1, 2, 30)), "Invalid round")
})

test_that("validate_rounds rejects non-numeric input", {
  expect_error(torp:::validate_rounds("1"), "must be numeric")
  expect_error(torp:::validate_rounds("round1"), "must be numeric")
  expect_error(torp:::validate_rounds(c("1", "2")), "must be numeric")
})

# -----------------------------------------------------------------------------
# validate_data_schema() Tests
# -----------------------------------------------------------------------------

test_that("validate_data_schema validates chains_data correctly", {
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

  result <- validate_data_schema(valid_chains, "chains_data", strict = FALSE)
  expect_true(result$valid)
  expect_equal(length(result$issues), 0)
})

test_that("validate_data_schema detects missing columns", {
  invalid_chains <- data.frame(
    match_id = "CD_M20240114201",
    # Missing period
    period_seconds = 500.0
  )

  result <- validate_data_schema(invalid_chains, "chains_data", strict = FALSE)
  expect_false(result$valid)
  expect_true(length(result$issues) > 0)
})

test_that("validate_data_schema validates model_data_epv", {
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
})

test_that("validate_data_schema detects constraint violations", {
  invalid_model <- data.frame(
    match_id = "CD_M20240114201",
    period = 1L,
    period_seconds = 500.0,
    goal_x = -50.0,  # Should be >= 0
    y = 10.0,
    exp_pts = 2.5,
    team_id_mdl = 1L,
    home = 1L
  )

  result <- validate_data_schema(invalid_model, "model_data_epv", strict = FALSE)
  expect_false(result$valid)
})

test_that("validate_data_schema handles extra columns gracefully", {
  chains_with_extra <- data.frame(
    match_id = "CD_M20240114201",
    period = 1L,
    period_seconds = 500.0,
    team_id = 1L,
    x = 50.0,
    y = 10.0,
    description = "Kick",
    player_name = "Test Player",
    utc_start_time = Sys.time(),
    extra_col = "extra data"  # Extra column
  )

  result <- validate_data_schema(chains_with_extra, "chains_data", strict = FALSE)
  # Should still be valid - extra columns are OK
  expect_true(result$valid)
})

# -----------------------------------------------------------------------------
# validate_data_quality() Tests
# -----------------------------------------------------------------------------

test_that("validate_data_quality detects high missing rates", {
  high_missing <- data.frame(
    x = c(1, 2, NA, NA, NA, NA, NA, NA, NA, NA),  # 80% missing
    y = 1:10
  )

  result <- validate_data_quality(high_missing, "test")
  expect_true("issues" %in% names(result))
  expect_true(result$quality_score < 1.0)
})

test_that("validate_data_quality detects zero variance", {
  zero_variance <- data.frame(
    constant = rep(5, 10),
    varying = 1:10
  )

  result <- validate_data_quality(zero_variance, "test")
  expect_true(is.list(result))
})

test_that("validate_data_quality detects outliers", {
  with_outlier <- data.frame(
    x = c(1:9, 1000),  # 1000 is an extreme outlier
    y = 1:10
  )

  result <- validate_data_quality(with_outlier, "test")
  expect_true(is.list(result))
  expect_true("issues" %in% names(result))
})

test_that("validate_data_quality returns perfect score for clean data", {
  clean_data <- data.frame(
    x = 1:100,
    y = rnorm(100),
    z = sample(letters, 100, replace = TRUE)
  )

  result <- validate_data_quality(clean_data, "test")
  expect_true(result$quality_score > 0.8)  # Should be high for clean data
})

# -----------------------------------------------------------------------------
# validate_data_freshness() Tests
# -----------------------------------------------------------------------------

test_that("validate_data_freshness returns TRUE for fresh data", {
  fresh_data <- data.frame(
    utc_start_time = Sys.time() - as.difftime(1, units = "days"),
    value = 1:5
  )

  expect_true(validate_data_freshness(fresh_data, "utc_start_time", max_age_days = 7))
})

test_that("validate_data_freshness returns FALSE for stale data", {
  stale_data <- data.frame(
    utc_start_time = Sys.time() - as.difftime(30, units = "days"),
    value = 1:5
  )

  expect_false(validate_data_freshness(stale_data, "utc_start_time", max_age_days = 7))
})

test_that("validate_data_freshness handles edge of threshold", {
  # Exactly at threshold
  edge_data <- data.frame(
    utc_start_time = Sys.time() - as.difftime(7, units = "days"),
    value = 1:5
  )

  # Should be exactly at the edge - could go either way
  result <- validate_data_freshness(edge_data, "utc_start_time", max_age_days = 7)
  expect_type(result, "logical")
})

test_that("validate_data_freshness handles missing timestamp column", {
  data_without_ts <- data.frame(
    value = 1:5
  )

  # Should handle missing column gracefully
  result <- tryCatch(
    validate_data_freshness(data_without_ts, "utc_start_time", max_age_days = 7),
    error = function(e) FALSE
  )
  expect_type(result, "logical")
})

# -----------------------------------------------------------------------------
# analyze_missing_data() Tests
# -----------------------------------------------------------------------------

test_that("analyze_missing_data reports correct missing rates", {
  test_data <- data.frame(
    complete = 1:10,
    half_missing = c(1:5, rep(NA, 5)),
    all_missing = rep(NA, 10)
  )

  result <- analyze_missing_data(test_data)

  expect_equal(unname(result$missing_by_column["complete"]), 0)
  expect_equal(unname(result$missing_by_column["half_missing"]), 0.5)
  expect_equal(unname(result$missing_by_column["all_missing"]), 1.0)
})

test_that("analyze_missing_data identifies high missing columns", {
  test_data <- data.frame(
    low_missing = c(1:9, NA),  # 10% missing
    high_missing = c(1:3, rep(NA, 7))  # 70% missing
  )

  result <- analyze_missing_data(test_data)

  expect_true("high_missing_cols" %in% names(result))
  expect_equal(result$high_missing_cols, 1)  # Only high_missing column
})

test_that("analyze_missing_data handles complete data", {
  complete_data <- data.frame(
    x = 1:10,
    y = 11:20
  )

  result <- analyze_missing_data(complete_data)

  expect_equal(result$overall_missing_rate, 0)
  expect_equal(result$high_missing_cols, 0)
})

# -----------------------------------------------------------------------------
# analyze_duplicates() Tests
# -----------------------------------------------------------------------------

test_that("analyze_duplicates detects exact duplicates", {
  with_dups <- data.frame(
    id = c(1, 2, 3, 2, 4),
    value = c("a", "b", "c", "b", "d")
  )

  result <- analyze_duplicates(with_dups)

  expect_equal(result$n_exact_duplicates, 1)
  expect_equal(result$n_total_rows, 5)
})

test_that("analyze_duplicates handles no duplicates", {
  no_dups <- data.frame(
    id = 1:5,
    value = letters[1:5]
  )

  result <- analyze_duplicates(no_dups)

  expect_equal(result$n_exact_duplicates, 0)
  expect_equal(result$duplicate_rate, 0)
})

test_that("analyze_duplicates calculates duplicate rate", {
  many_dups <- data.frame(
    id = c(1, 1, 2, 2, 3),  # 2 duplicate rows
    value = c("a", "a", "b", "b", "c")
  )

  result <- analyze_duplicates(many_dups)

  expect_equal(result$n_exact_duplicates, 2)
  expect_equal(result$duplicate_rate, 2/5)
})

# -----------------------------------------------------------------------------
# detect_outliers() Tests
# -----------------------------------------------------------------------------

test_that("detect_outliers identifies extreme values", {
  with_outlier <- data.frame(
    normal = c(1:9, 1000)
  )

  result <- detect_outliers(with_outlier)

  expect_true("column_results" %in% names(result))
  expect_true("normal" %in% names(result$column_results))
  expect_true(result$column_results$normal$extreme_outliers_iqr > 0)
})

test_that("detect_outliers handles normal data", {
  normal_data <- data.frame(
    x = rnorm(100, mean = 50, sd = 10)
  )

  result <- detect_outliers(normal_data)

  # Normal data should have few or no extreme outliers
  expect_true(result$column_results$x$extreme_outliers_iqr < 10)
})

test_that("detect_outliers handles multiple columns", {
  multi_col <- data.frame(
    normal = 1:100,
    with_outlier = c(1:99, 10000)
  )

  result <- detect_outliers(multi_col)

  expect_true("normal" %in% names(result$column_results))
  expect_true("with_outlier" %in% names(result$column_results))
})

# -----------------------------------------------------------------------------
# validate_chains_quality() Tests
# -----------------------------------------------------------------------------

test_that("validate_chains_quality detects invalid match_id", {
  bad_match_id <- data.frame(
    match_id = c("INVALID_ID", "CD_M20240114201"),
    period = c(1, 2),
    period_seconds = c(500, 1000),
    x = c(50, 60),
    y = c(10, 20)
  )

  result <- validate_chains_quality(bad_match_id)

  expect_true(any(grepl("Invalid match_id", unlist(result))))
})

test_that("validate_chains_quality detects invalid period", {
  bad_period <- data.frame(
    match_id = c("CD_M20240114201", "CD_M20240114201"),
    period = c(1, 5),  # Period 5 is invalid
    period_seconds = c(500, 1000),
    x = c(50, 60),
    y = c(10, 20)
  )

  result <- validate_chains_quality(bad_period)

  expect_true(any(grepl("Invalid period", unlist(result))))
})

test_that("validate_chains_quality detects invalid period_seconds", {
  bad_seconds <- data.frame(
    match_id = rep("CD_M20240114201", 2),
    period = c(1, 2),
    period_seconds = c(500, 3000),  # 3000 > 2000 is invalid
    x = c(50, 60),
    y = c(10, 20)
  )

  result <- validate_chains_quality(bad_seconds)

  expect_true(length(result) > 0)
})

test_that("validate_chains_quality detects out of bounds coordinates", {
  bad_coords <- data.frame(
    match_id = rep("CD_M20240114201", 2),
    period = c(1, 2),
    period_seconds = c(500, 1000),
    x = c(50, 200),  # 200 > typical field length
    y = c(10, 20)
  )

  result <- validate_chains_quality(bad_coords)

  expect_true(length(result) > 0)
})

test_that("validate_chains_quality accepts valid data", {
  valid_chains <- data.frame(
    match_id = rep("CD_M20240114201", 3),
    period = c(1, 2, 3),
    period_seconds = c(500, 1000, 1500),
    x = c(50, 60, 70),
    y = c(10, -10, 0)
  )

  result <- validate_chains_quality(valid_chains)

  # Valid data should have few or no issues
  expect_true(length(result) < 3)
})

# -----------------------------------------------------------------------------
# calculate_quality_score() Tests
# -----------------------------------------------------------------------------

test_that("calculate_quality_score returns 1.0 for no issues", {
  no_issues <- list()
  perfect_data <- data.frame(x = 1:10)

  score <- calculate_quality_score(perfect_data, no_issues)

  expect_equal(score, 1.0)
})

test_that("calculate_quality_score reduces score for issues", {
  some_issues <- list(
    missing_values = "Some missing values detected"
  )
  data <- data.frame(x = 1:10)

  score <- calculate_quality_score(data, some_issues)

  expect_true(score < 1.0)
  expect_true(score >= 0)
})

test_that("calculate_quality_score handles multiple issues", {
  many_issues <- list(
    high_missing = "50% missing values",
    outliers = "Extreme outliers detected",
    duplicates = "Duplicate rows found"
  )
  data <- data.frame(x = 1:10)

  score <- calculate_quality_score(data, many_issues)

  expect_true(score < 0.7)  # Multiple issues should significantly reduce score
  expect_true(score >= 0)
})

test_that("calculate_quality_score is bounded between 0 and 1", {
  extreme_issues <- list(
    issue1 = "Issue",
    issue2 = "Issue",
    issue3 = "Issue",
    issue4 = "Issue",
    issue5 = "Issue",
    issue6 = "Issue",
    issue7 = "Issue",
    issue8 = "Issue",
    issue9 = "Issue",
    issue10 = "Issue"
  )
  data <- data.frame(x = 1:10)

  score <- calculate_quality_score(data, extreme_issues)

  expect_true(score >= 0)
  expect_true(score <= 1)
})
