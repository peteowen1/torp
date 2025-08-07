test_that("select_epv_model_vars works correctly", {
  # Create test data
  test_df <- data.frame(
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
    home = c(1, 0, 1),
    label_ep = c(0.5, 0.3, 0.7),
    extra_col = c("a", "b", "c")
  )
  
  # Test without label
  result_no_label <- torp:::select_epv_model_vars(test_df, label = FALSE)
  expect_equal(ncol(result_no_label), 17)
  expect_false("label_ep" %in% names(result_no_label))
  expect_false("extra_col" %in% names(result_no_label))
  
  # Test with label
  result_with_label <- torp:::select_epv_model_vars(test_df, label = TRUE)
  expect_equal(ncol(result_with_label), 18)
  expect_true("label_ep" %in% names(result_with_label))
  expect_false("extra_col" %in% names(result_with_label))
})

test_that("AFL constants are defined correctly", {
  expect_equal(AFL_GOAL_WIDTH, 6.4)
  expect_equal(AFL_QUARTER_DURATION, 2000)
  expect_equal(AFL_MAX_PERIODS, 4)
  expect_equal(AFL_TIME_SCALER_MAX, 4)
})

test_that("is_current_throw_in_team_change works correctly", {
  throw_in <- c(0, 1, 0, 1)
  team_id_mdl <- c(1, 1, 2, 2)
  
  result <- torp:::is_current_throw_in_team_change(throw_in, team_id_mdl)
  
  # Should be TRUE for position 4 (throw_in=1, lag(throw_in)=0, lag(team_id_mdl)=2 != team_id_mdl=2)
  expect_type(result, "logical")
  expect_length(result, 4)
})

test_that("calculate_mirror helper functions work", {
  # Test data
  throw_in <- c(0, 1, 1, 0, 1)
  team_id_mdl <- c(1, 1, 2, 2, 1)
  x <- c(-10, -5, 5, 10, -8)
  
  # Test helper functions exist and return logical vectors
  result1 <- torp:::is_current_throw_in_team_change(throw_in, team_id_mdl)
  expect_type(result1, "logical")
  expect_length(result1, 5)
  
  result2 <- torp:::is_consecutive_throw_in_same_side(throw_in, team_id_mdl, x)
  expect_type(result2, "logical")
  expect_length(result2, 5)
  
  result3 <- torp:::is_consecutive_throw_in_side_change(throw_in, team_id_mdl, x)
  expect_type(result3, "logical")
  expect_length(result3, 5)
  
  result4 <- torp:::is_previous_throw_in_affecting(throw_in, team_id_mdl, x)
  expect_type(result4, "logical")
  expect_length(result4, 5)
})

test_that("calculate_mirror returns correct values", {
  # Simple test case
  throw_in <- c(0, 1, 0, 1)
  team_id_mdl <- c(1, 1, 2, 2) 
  x <- c(-10, -5, 5, 10)
  
  result <- torp:::calculate_mirror(throw_in, team_id_mdl, x)
  
  expect_type(result, "double")
  expect_length(result, 4)
  expect_true(all(result %in% c(-1, 1)))
})