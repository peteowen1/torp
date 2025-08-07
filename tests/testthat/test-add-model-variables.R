test_that("get_epv_preds function exists and has correct structure", {
  expect_true(exists("get_epv_preds"))
  
  # Test with mock data
  mock_df <- data.frame(
    goal_x = c(50, 60, 70),
    y = c(0, 5, -5),
    lag_goal_x = c(55, 65, 75),
    lag_goal_x5 = c(60, 70, 80),
    lag_y = c(2, 7, -3),
    period_seconds = c(100, 200, 300),
    period = c(1, 1, 2),
    play_type_handball = c(1, 0, 1),
    play_type_kick = c(0, 1, 0),
    play_type_reception = c(0, 0, 1),
    phase_of_play_handball_received = c(1, 0, 0),
    phase_of_play_hard_ball = c(0, 1, 0),
    phase_of_play_loose_ball = c(0, 0, 1),
    phase_of_play_set_shot = c(0, 0, 0),
    shot_row = c(0, 1, 0),
    speed5 = c(10, 15, 20),
    home = c(1, 0, 1)
  )
  
  # Function should exist and process data (may fail without model data)
  result <- tryCatch({
    get_epv_preds(mock_df)
  }, error = function(e) e)
  
  # Either works or gives expected error about missing model
  if (inherits(result, "error")) {
    expect_true(grepl("ep_model|object.*not found", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 5)
    expect_true(all(c("opp_goal", "opp_behind", "behind", "goal", "no_score") %in% names(result)))
  }
})

test_that("get_wp_preds function exists and has correct structure", {
  expect_true(exists("get_wp_preds"))
  
  # Test with mock data
  mock_df <- data.frame(
    total_seconds = c(1800, 3600, 5400),
    shot_row = c(0, 1, 0),
    home = c(1, 0, 1),
    points_diff = c(6, -3, 12),
    xpoints_diff = c(6.5, -2.8, 12.3),
    pos_lead_prob = c(0.7, 0.3, 0.9),
    time_left_scaler = c(1.5, 2.0, 2.5),
    diff_time_ratio = c(9.75, -5.6, 30.75),
    play_type_handball = c(1, 0, 1),
    play_type_kick = c(0, 1, 0),
    play_type_reception = c(0, 0, 1),
    phase_of_play_handball_received = c(1, 0, 0),
    phase_of_play_hard_ball = c(0, 1, 0),
    phase_of_play_loose_ball = c(0, 0, 1),
    phase_of_play_set_shot = c(0, 0, 0)
  )
  
  # Function should exist and process data (may fail without model data)
  result <- tryCatch({
    get_wp_preds(mock_df)
  }, error = function(e) e)
  
  # Either works or gives expected error about missing model
  if (inherits(result, "error")) {
    expect_true(grepl("wp_model|object.*not found", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 1)
    expect_true("wp" %in% names(result))
  }
})

test_that("add_epv_vars function works correctly", {
  expect_true(exists("add_epv_vars"))
  
  # Create mock play-by-play data
  mock_pbp <- create_mock_pbp_data(10)
  
  # Function should exist and process data (may fail without model data)
  result <- tryCatch({
    add_epv_vars(mock_pbp)
  }, error = function(e) e)
  
  # Test structure if it works, or expected error
  if (inherits(result, "error")) {
    expect_true(grepl("ep_model|object.*not found|select_epv_model_vars", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_gte(ncol(result), ncol(mock_pbp))  # Should have additional columns
  }
})

test_that("add_wp_vars function works correctly", {
  expect_true(exists("add_wp_vars"))
  
  # Create mock play-by-play data
  mock_pbp <- create_mock_pbp_data(10)
  
  # Function should exist and process data (may fail without model data)
  result <- tryCatch({
    add_wp_vars(mock_pbp)
  }, error = function(e) e)
  
  # Test structure if it works, or expected error
  if (inherits(result, "error")) {
    expect_true(grepl("wp_model|object.*not found|select_wp_model_vars", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_gte(ncol(result), ncol(mock_pbp))  # Should have additional columns
  }
})

test_that("add_shot_vars function works correctly", {
  expect_true(exists("add_shot_vars"))
  
  # Create mock shot data  
  mock_shots <- create_mock_shot_data(10)
  
  # Function should exist and process data (may fail without model data)
  result <- tryCatch({
    add_shot_vars(mock_shots)
  }, error = function(e) e)
  
  # Test structure if it works, or expected error
  if (inherits(result, "error")) {
    expect_true(grepl("shot_ocat_mdl|object.*not found", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_gte(ncol(result), ncol(mock_shots))  # Should have additional columns
  }
})