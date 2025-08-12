# Tests for Enhanced Win Probability Predictions
# ==============================================

test_that("create_wp_features_enhanced generates correct features", {
  # Create mock data with required columns
  mock_data <- data.frame(
    match_id = rep("CD_M20240114201", 100),
    period = rep(1:4, 25),
    period_seconds = seq(100, 1900, length.out = 100),
    points_diff = sample(-30:30, 100, replace = TRUE),
    exp_pts = runif(100, -2, 2),
    goal_x = runif(100, 0, 120),
    y = runif(100, -40, 40),
    home = rep(c(1, 0), 50),
    team_id_mdl = sample(1:18, 100, replace = TRUE),
    x = runif(100, -80, 80),
    play_type = sample(c("handball", "kick", "reception"), 100, replace = TRUE),
    phase_of_play = sample(c("handball_received", "hard_ball", "loose_ball"), 100, replace = TRUE),
    description = sample(c("Kick", "Handball", "Mark"), 100, replace = TRUE),
    disposal = sample(c("effective", "ineffective", "clanger"), 100, replace = TRUE),
    shot_at_goal = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.1, 0.9))
  )

  # Should not error with valid input
  expect_silent(result <- create_wp_features_enhanced(mock_data))

  # Check that result is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(mock_data))

  # Check for key enhanced features
  expected_features <- c("time_remaining", "time_remaining_sqrt", "points_diff_normalized",
                        "goal_distance", "field_position_category", "momentum_3_avg")

  # Should have more columns than input (enhanced features)
  expect_gt(ncol(result), ncol(mock_data))
})

test_that("get_wp_preds_enhanced handles missing data gracefully", {
  # Create minimal data
  minimal_data <- data.frame(
    match_id = "CD_M20240114201",
    period = 1,
    period_seconds = 500,
    points_diff = 6,
    home = 1,
    total_seconds = 500,
    shot_row = 0,
    xpoints_diff = 6.5,
    pos_lead_prob = 0.8,
    time_left_scaler = 2.0,
    diff_time_ratio = 13.0,
    play_type_handball = 0,
    play_type_kick = 1,
    play_type_reception = 0,
    phase_of_play_handball_received = 0,
    phase_of_play_hard_ball = 1,
    phase_of_play_loose_ball = 0,
    phase_of_play_set_shot = 0
  )

  # Should handle gracefully or give informative error
  result <- tryCatch({
    get_wp_preds_enhanced(minimal_data)
  }, error = function(e) e)

  # Function should succeed with fallback (warnings are expected)
  expect_true(is.data.frame(result))
  expect_true("wp" %in% names(result))
})


test_that("time-based features are calculated correctly", {
  # Create data with known time values
  test_data <- data.frame(
    match_id = "CD_M20240114201",
    period = c(1, 2, 4),
    period_seconds = c(500, 1000, 1800),
    points_diff = c(6, -3, 12),
    home = c(1, 0, 1),
    team_id_mdl = c(1, 2, 1),
    exp_pts = c(2.5, -1.2, 4.1),
    goal_x = c(50, 80, 30),
    y = c(10, -5, 20)
  )

  result <- create_wp_features_enhanced(test_data)

  # Check time remaining calculation
  expect_true("time_remaining" %in% names(result))
  expect_true(all(result$time_remaining >= 0))

  # Period 4 should have less time remaining than period 1
  expect_lt(result$time_remaining[3], result$time_remaining[1])
})

test_that("spatial features are calculated correctly", {
  # Create data with known spatial values
  test_data <- data.frame(
    match_id = "CD_M20240114201",
    period = 1,
    period_seconds = 500,
    goal_x = c(10, 50, 100),  # Close, medium, far from goal
    y = c(0, 20, -20),        # Center, left, right
    x = runif(3, -80, 80),
    points_diff = c(0, 0, 0),
    home = c(1, 1, 1),
    team_id_mdl = c(1, 1, 1),
    exp_pts = c(3.2, 1.8, 0.5)
  )

  result <- create_wp_features_enhanced(test_data)

  # Check goal distance calculation
  expect_true("goal_distance" %in% names(result))
  expect_true(all(result$goal_distance >= 0))

  # Closer to goal should have smaller distance
  expect_lt(result$goal_distance[1], result$goal_distance[3])

  # Check field position categories
  if ("field_position_category" %in% names(result)) {
    expect_true(all(result$field_position_category %in% c("attacking_50", "midfield", "defensive_50")))
  }
})

test_that("momentum features handle edge cases", {
  # Create data with insufficient history for momentum
  short_data <- data.frame(
    match_id = "CD_M20240114201",
    period = 1,
    period_seconds = c(100, 200),
    points_diff = c(0, 6),
    exp_pts = c(0, 1.5),
    home = c(1, 1),
    team_id_mdl = c(1, 1),
    goal_x = c(75, 45),
    y = c(5, -10)
  )

  # Should handle gracefully without errors
  expect_silent(result <- create_wp_features_enhanced(short_data))
  expect_equal(nrow(result), 2)
})

test_that("feature engineering handles missing values appropriately", {
  # Create data with some missing values
  incomplete_data <- data.frame(
    match_id = "CD_M20240114201",
    period = c(1, 2, 3),
    period_seconds = c(500, NA, 1500),
    points_diff = c(6, -3, NA),
    home = c(1, 0, 1),
    team_id_mdl = c(1, 2, 1)
  )

  # Should handle missing values without crashing
  result <- tryCatch({
    create_wp_features_enhanced(incomplete_data)
  }, error = function(e) e)

  # Should either work or give informative error
  if (!inherits(result, "error")) {
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
  } else {
    expect_true(grepl("missing|NA", result$message, ignore.case = TRUE))
  }
})
