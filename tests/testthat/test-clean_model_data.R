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
    est_qtr_remaining = c(1100, 1000, 900),
    est_match_remaining = c(4700, 4600, 2500),
    label_ep = c(0.5, 0.3, 0.7),
    extra_col = c("a", "b", "c")
  )

  # Test without label
  result_no_label <- torp:::select_epv_model_vars(test_df, label = FALSE)
  expect_equal(ncol(result_no_label), 19)
  expect_false("label_ep" %in% names(result_no_label))
  expect_false("extra_col" %in% names(result_no_label))

  # Test with label
  result_with_label <- torp:::select_epv_model_vars(test_df, label = TRUE)
  expect_equal(ncol(result_with_label), 20)
  expect_true("label_ep" %in% names(result_with_label))
  expect_false("extra_col" %in% names(result_with_label))
})

test_that("AFL constants are defined correctly", {
  expect_equal(AFL_GOAL_WIDTH, 6.4)
  expect_equal(AFL_QUARTER_DURATION, 2000)
  expect_equal(AFL_MAX_PERIODS, 4)
  expect_equal(AFL_TIME_SCALER_MAX, 4)
})

# NOTE: is_current_throw_in_team_change, calculate_mirror, and related
# dplyr-based mirror helpers were removed in favour of the data.table
# fcase() implementation in add_epv_team_vars_dt().