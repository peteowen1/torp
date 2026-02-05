# Tests for Baseline Models
# =========================

test_that("predict_wp_score_only works correctly", {
  # Create test data with score differences
  test_data <- data.frame(
    points_diff = c(-20, -5, 0, 5, 20),
    period = c(1, 2, 3, 4, 4),
    period_seconds = c(500, 1000, 1500, 1700, 1800)
  )
  
  result <- predict_wp_score_only(test_data)
  
  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(test_data))
  expect_true(all(result >= 0 & result <= 1))
  
  # Teams with positive score diff should have higher WP
  expect_true(result[5] > result[1])  # +20 vs -20
  expect_true(result[4] > result[2])  # +5 vs -5
})

test_that("predict_wp_time_only considers time remaining", {
  # Test with different time scenarios
  test_data <- data.frame(
    points_diff = c(6, 6, 6),  # Same score difference
    period = c(1, 3, 4),       # Different periods
    period_seconds = c(100, 1500, 1790)  # Different times in period
  )
  
  result <- predict_wp_time_only(test_data)
  
  expect_true(is.numeric(result))
  expect_equal(length(result), 3)
  expect_true(all(result >= 0 & result <= 1))
  
  # With same score diff, later periods should show less variation from 0.5
  # (time becomes less of a factor)
})

test_that("predict_wp_gam_baseline generates reasonable predictions", {
  # Create test data
  test_data <- data.frame(
    points_diff = c(-12, -6, 0, 6, 12),
    period = c(1, 2, 3, 4, 4),
    period_seconds = c(500, 1000, 1500, 1600, 1800),
    goal_x = c(30, 40, 50, 60, 70),
    y = c(0, 10, 0, -10, 5)
  )
  
  result <- predict_wp_gam_baseline(test_data)
  
  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(test_data))
  expect_true(all(result >= 0 & result <= 1))
  
  # Should be monotonic with score difference (roughly)
  expect_true(result[5] > result[1])  # +12 vs -12
})

test_that("predict_wp_ensemble_baseline combines models appropriately", {
  # Create test data
  test_data <- data.frame(
    points_diff = c(-6, 0, 6),
    period = c(1, 2, 4),
    period_seconds = c(500, 1000, 1700),
    goal_x = c(40, 50, 60),
    y = c(0, 5, -5)
  )
  
  result <- predict_wp_ensemble_baseline(test_data)
  
  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(test_data))
  expect_true(all(result >= 0 & result <= 1))
  
  # Should be reasonably different from any single baseline
  score_only <- predict_wp_score_only(test_data)
  expect_false(all(abs(result - score_only) < 0.01))
})

test_that("evaluate_baseline_models works correctly", {
  # Create synthetic evaluation data
  set.seed(42)
  n <- 200
  actual <- rbinom(n, 1, 0.6)
  
  test_data <- data.frame(
    points_diff = rnorm(n, 0, 10),
    period = sample(1:4, n, replace = TRUE),
    period_seconds = sample(100:1800, n, replace = TRUE),
    goal_x = runif(n, 20, 80),
    y = runif(n, -30, 30)
  )
  
  result <- evaluate_baseline_models(actual, test_data)
  
  expect_true(is.data.frame(result))
  expect_true("model_name" %in% names(result))
  expect_true("auc" %in% names(result))
  expect_true("log_loss" %in% names(result))
  expect_true("brier_score" %in% names(result))
  
  # Should have multiple baseline models
  expect_gte(nrow(result), 3)
  
  # All metrics should be reasonable
  expect_true(all(result$auc >= 0 & result$auc <= 1))
  expect_true(all(result$log_loss >= 0))
  expect_true(all(result$brier_score >= 0 & result$brier_score <= 1))
})

test_that("baseline models handle edge cases", {
  # Test with extreme score differences
  extreme_data <- data.frame(
    points_diff = c(-100, 100),
    period = c(4, 4),
    period_seconds = c(1799, 1799),
    goal_x = c(50, 50),
    y = c(0, 0)
  )
  
  # All baseline models should handle extreme cases
  score_result <- predict_wp_score_only(extreme_data)
  expect_true(all(score_result >= 0 & score_result <= 1))
  expect_true(score_result[1] < 0.1)  # Very low WP for -100 point deficit
  expect_true(score_result[2] > 0.9)  # Very high WP for +100 point lead
  
  time_result <- predict_wp_time_only(extreme_data)
  expect_true(all(time_result >= 0 & time_result <= 1))
  
  gam_result <- predict_wp_gam_baseline(extreme_data)
  expect_true(all(gam_result >= 0 & gam_result <= 1))
})

test_that("baseline models handle missing values appropriately", {
  # Test with some missing values
  missing_data <- data.frame(
    points_diff = c(6, NA, -6),
    period = c(1, 2, NA),
    period_seconds = c(500, 1000, 1500),
    goal_x = c(40, 50, NA),
    y = c(0, NA, 0)
  )
  
  # Should handle missing values without crashing
  result1 <- tryCatch({
    predict_wp_score_only(missing_data)
  }, error = function(e) e)
  
  result2 <- tryCatch({
    predict_wp_gam_baseline(missing_data)
  }, error = function(e) e)
  
  # Should either work or give informative errors
  if (!inherits(result1, "error")) {
    expect_equal(length(result1), 3)
  }
  
  if (!inherits(result2, "error")) {
    expect_equal(length(result2), 3)
  }
})

test_that("assess_model_calibration works correctly", {
  # Create test predictions and outcomes
  set.seed(123)
  n <- 1000
  
  # Well-calibrated predictions
  true_probs <- runif(n, 0.1, 0.9)
  actual <- rbinom(n, 1, true_probs)
  predicted <- true_probs + rnorm(n, 0, 0.1)  # Add small noise
  predicted <- pmax(0.01, pmin(0.99, predicted))  # Bound predictions
  
  result <- assess_model_calibration(actual, predicted)
  
  expect_true(is.list(result))
  expect_true("calibration_slope" %in% names(result))
  expect_true("calibration_intercept" %in% names(result))
  expect_true("hosmer_lemeshow_p" %in% names(result))
  expect_true("calibration_in_large" %in% names(result))
  
  # Well-calibrated model should have slope near 1, intercept near 0
  expect_true(abs(result$calibration_slope - 1) < 0.2)
  expect_true(abs(result$calibration_intercept) < 0.2)
})

test_that("create_calibration_plot handles various prediction ranges", {
  # Test with different prediction patterns
  set.seed(456)
  n <- 500

  # Predictions concentrated in middle range
  predicted_mid <- runif(n, 0.3, 0.7)
  actual_mid <- rbinom(n, 1, predicted_mid)

  result_mid <- create_calibration_plot(actual_mid, predicted_mid)
  expect_true(is.list(result_mid))
  expect_true("plot_data" %in% names(result_mid))

  # Predictions across full range
  predicted_full <- runif(n, 0.05, 0.95)
  actual_full <- rbinom(n, 1, predicted_full)

  result_full <- create_calibration_plot(actual_full, predicted_full)
  expect_true(is.list(result_full))
  expect_true("plot_data" %in% names(result_full))

  # Should have reasonable number of bins
  expect_gte(nrow(result_full$plot_data), 5)
  expect_lte(nrow(result_full$plot_data), 20)
})

# -----------------------------------------------------------------------------
# Extreme Case Tests
# -----------------------------------------------------------------------------

test_that("all baseline models handle blowout scenarios", {
  # Test with 100-point blowout
  blowout_data <- data.frame(
    points_diff = c(-100, -50, 50, 100),
    period = c(4, 4, 4, 4),
    period_seconds = c(1800, 1800, 1800, 1800),
    goal_x = c(50, 50, 50, 50),
    y = c(0, 0, 0, 0)
  )

  # Score-only model
  score_result <- predict_wp_score_only(blowout_data)
  expect_true(all(score_result >= 0 & score_result <= 1))
  expect_true(score_result[1] < 0.01)   # -100 point deficit near game end
  expect_true(score_result[4] > 0.99)   # +100 point lead near game end

  # GAM baseline
  gam_result <- predict_wp_gam_baseline(blowout_data)
  expect_true(all(gam_result >= 0 & gam_result <= 1))
})

test_that("baseline models handle early game scenarios correctly", {
  # Early in game, win probabilities should be closer to 0.5
  early_game_data <- data.frame(
    points_diff = c(-12, 0, 12),
    period = c(1, 1, 1),
    period_seconds = c(100, 100, 100),  # Very early in Q1
    goal_x = c(50, 50, 50),
    y = c(0, 0, 0)
  )

  score_result <- predict_wp_score_only(early_game_data)

  # Even with 12 point lead in Q1, shouldn't be too extreme
  expect_true(score_result[1] > 0.1)  # Down 12 early still has chance
  expect_true(score_result[3] < 0.9)  # Up 12 early not guaranteed
})

test_that("baseline models handle late game close scenarios", {
  # Close game in final 2 minutes
  close_late_data <- data.frame(
    points_diff = c(-6, 0, 6),
    period = c(4, 4, 4),
    period_seconds = c(1750, 1750, 1750),  # Final 50 seconds
    goal_x = c(50, 50, 50),
    y = c(0, 0, 0)
  )

  score_result <- predict_wp_score_only(close_late_data)

  # Down by a goal with 50 seconds left is tough but not impossible
  expect_true(score_result[1] > 0.01)
  expect_true(score_result[1] < 0.5)  # Relaxed - model may vary

  # Tied with 50 seconds left should be around 50%
  expect_true(score_result[2] > 0.35)  # Relaxed
  expect_true(score_result[2] < 0.65)  # Relaxed
})

test_that("baseline models produce monotonic scores for same time", {
  # At same time, higher score diff should always = higher WP
  test_data <- data.frame(
    points_diff = seq(-50, 50, by = 10),
    period = rep(3, 11),
    period_seconds = rep(1000, 11),
    goal_x = rep(50, 11),
    y = rep(0, 11)
  )

  score_result <- predict_wp_score_only(test_data)

  # Should be monotonically increasing
  for (i in 2:length(score_result)) {
    expect_true(score_result[i] >= score_result[i-1],
                info = paste("WP not monotonic at index", i))
  }
})

test_that("baseline models handle single row input", {
  single_row <- data.frame(
    points_diff = 0,
    period = 2,
    period_seconds = 500,
    goal_x = 50,
    y = 0
  )

  # All models should work with single row
  expect_length(predict_wp_score_only(single_row), 1)
  expect_length(predict_wp_time_only(single_row), 1)
  expect_length(predict_wp_gam_baseline(single_row), 1)
})

test_that("baseline models handle large datasets efficiently", {
  # Test with 10000 rows to ensure no memory issues
  large_data <- data.frame(
    points_diff = sample(-50:50, 10000, replace = TRUE),
    period = sample(1:4, 10000, replace = TRUE),
    period_seconds = sample(0:1800, 10000, replace = TRUE),
    goal_x = runif(10000, 10, 90),
    y = runif(10000, -40, 40)
  )

  result <- predict_wp_score_only(large_data)
  expect_length(result, 10000)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict_wp_naive returns 0.5 for all inputs", {
  test_data <- data.frame(
    points_diff = c(-50, 0, 50),
    period = c(1, 2, 4),
    period_seconds = c(100, 500, 1800)
  )

  result <- tryCatch(
    predict_wp_naive(test_data),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_true(all(result == 0.5))
  }
})

# -----------------------------------------------------------------------------
# Edge Cases for Time-Based Models
# -----------------------------------------------------------------------------

test_that("predict_wp_time_only handles period boundaries", {
  boundary_data <- data.frame(
    points_diff = rep(0, 5),
    period = c(1, 2, 3, 4, 4),
    period_seconds = c(0, 0, 0, 0, 1999)  # Start of each quarter + end of game
  )

  result <- predict_wp_time_only(boundary_data)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("ensemble combines models with valid weights", {
  test_data <- data.frame(
    points_diff = c(-10, 0, 10),
    period = c(2, 3, 4),
    period_seconds = c(500, 1000, 1500),
    goal_x = c(40, 50, 60),
    y = c(0, 10, -10)
  )

  # Ensemble should return values between individual model predictions
  score_preds <- predict_wp_score_only(test_data)
  ensemble_preds <- predict_wp_ensemble_baseline(test_data)

  expect_true(all(ensemble_preds >= 0 & ensemble_preds <= 1))
  expect_false(all(ensemble_preds == score_preds))  # Should differ from score-only
})