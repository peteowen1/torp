# Tests for Constants
# ===================
# Verify constants have expected values and are reasonable

# -----------------------------------------------------------------------------
# AFL Field and Game Constants
# -----------------------------------------------------------------------------

test_that("AFL_GOAL_WIDTH has expected value", {
  expect_equal(torp:::AFL_GOAL_WIDTH, 6.4)
  expect_true(torp:::AFL_GOAL_WIDTH > 0)
})

test_that("AFL_QUARTER_DURATION has expected value", {
  expect_equal(torp:::AFL_QUARTER_DURATION, 2000)
  expect_true(torp:::AFL_QUARTER_DURATION > 1000)  # Should be at least 1000 seconds
  expect_true(torp:::AFL_QUARTER_DURATION < 3000)  # Should be less than 50 mins
})

test_that("AFL_TOTAL_GAME_SECONDS has expected value", {
  expect_equal(torp:::AFL_TOTAL_GAME_SECONDS, 8000)
  # Should equal 4 quarters
  expect_equal(
    torp:::AFL_TOTAL_GAME_SECONDS,
    torp:::AFL_QUARTER_DURATION * 4
  )
})

test_that("AFL_MAX_PERIODS has expected value", {
  expect_equal(torp:::AFL_MAX_PERIODS, 4)
})

test_that("AFL_TIME_SCALER_MAX has expected value", {
  expect_equal(torp:::AFL_TIME_SCALER_MAX, 4)
  expect_true(torp:::AFL_TIME_SCALER_MAX > 0)
})

# -----------------------------------------------------------------------------
# Rating System Constants
# -----------------------------------------------------------------------------

test_that("RATING_DECAY_DEFAULT_DAYS has expected value", {
  expect_equal(torp:::RATING_DECAY_DEFAULT_DAYS, 365)
  expect_true(torp:::RATING_DECAY_DEFAULT_DAYS > 0)
})

test_that("RATING_LOADING_DEFAULT has expected value", {
  expect_equal(torp:::RATING_LOADING_DEFAULT, 1.5)
  expect_true(torp:::RATING_LOADING_DEFAULT > 0)
  expect_true(torp:::RATING_LOADING_DEFAULT < 10)  # Reasonable upper bound
})

test_that("RATING_PRIOR_GAMES_RECV has expected value", {
  expect_equal(torp:::RATING_PRIOR_GAMES_RECV, 4)
  expect_true(torp:::RATING_PRIOR_GAMES_RECV > 0)
  expect_true(torp:::RATING_PRIOR_GAMES_RECV < 50)  # Reasonable bound
})

test_that("RATING_PRIOR_GAMES_DISP has expected value", {
  expect_equal(torp:::RATING_PRIOR_GAMES_DISP, 6)
  expect_true(torp:::RATING_PRIOR_GAMES_DISP > 0)
  expect_true(torp:::RATING_PRIOR_GAMES_DISP < 50)  # Reasonable bound
})

test_that("RATING_SPOIL_MULTIPLIER has expected value", {
  expect_equal(torp:::RATING_SPOIL_MULTIPLIER, 1.2)
  expect_true(torp:::RATING_SPOIL_MULTIPLIER > 0)
  expect_true(torp:::RATING_SPOIL_MULTIPLIER < 5)  # Reasonable multiplier
})

# -----------------------------------------------------------------------------
# Simulation Constants
# -----------------------------------------------------------------------------

test_that("SIM_NOISE_SD has expected value", {
  expect_equal(torp:::SIM_NOISE_SD, 26)
  expect_true(torp:::SIM_NOISE_SD > 0)
  expect_true(torp:::SIM_NOISE_SD < 100)  # Reasonable bound for noise
})

test_that("SIM_WP_SCALING_FACTOR has expected value", {
  expect_equal(torp:::SIM_WP_SCALING_FACTOR, 50)
  expect_true(torp:::SIM_WP_SCALING_FACTOR > 0)
})

test_that("SIM_HOME_ADVANTAGE has expected value", {
  expect_equal(torp:::SIM_HOME_ADVANTAGE, 6)
  expect_true(torp:::SIM_HOME_ADVANTAGE > 0)
  expect_true(torp:::SIM_HOME_ADVANTAGE < 20)  # Reasonable home advantage
})

# -----------------------------------------------------------------------------
# Win Probability Constants
# -----------------------------------------------------------------------------

test_that("WP_FINAL_5_MINS_SECONDS has expected value", {
  expect_equal(torp:::WP_FINAL_5_MINS_SECONDS, 300)
  expect_equal(torp:::WP_FINAL_5_MINS_SECONDS, 5 * 60)  # 5 minutes in seconds
})

test_that("WP_FINAL_2_MINS_SECONDS has expected value", {
  expect_equal(torp:::WP_FINAL_2_MINS_SECONDS, 120)
  expect_equal(torp:::WP_FINAL_2_MINS_SECONDS, 2 * 60)  # 2 minutes in seconds
})

test_that("WP thresholds are ordered correctly", {
  expect_true(torp:::WP_FINAL_2_MINS_SECONDS < torp:::WP_FINAL_5_MINS_SECONDS)
})

# -----------------------------------------------------------------------------
# Data Validation Constants
# -----------------------------------------------------------------------------

test_that("VALIDATION_HIGH_MISSING_THRESHOLD has expected value", {
  expect_equal(torp:::VALIDATION_HIGH_MISSING_THRESHOLD, 0.5)
  expect_true(torp:::VALIDATION_HIGH_MISSING_THRESHOLD > 0)
  expect_true(torp:::VALIDATION_HIGH_MISSING_THRESHOLD <= 1)
})

test_that("VALIDATION_MIN_CALIBRATION_BIN_SIZE has expected value", {
  expect_equal(torp:::VALIDATION_MIN_CALIBRATION_BIN_SIZE, 10)
  expect_true(torp:::VALIDATION_MIN_CALIBRATION_BIN_SIZE > 0)
})

# -----------------------------------------------------------------------------
# Constants Consistency Checks
# -----------------------------------------------------------------------------

test_that("game duration constants are consistent", {
  # Total game seconds should equal quarters * quarter duration
  expected_total <- torp:::AFL_MAX_PERIODS * torp:::AFL_QUARTER_DURATION
  expect_equal(torp:::AFL_TOTAL_GAME_SECONDS, expected_total)
})

test_that("rating prior games constants are reasonable relative to each other", {
  # Disposal prior should be >= receiving prior (more data needed for disposal)
  expect_gte(
    torp:::RATING_PRIOR_GAMES_DISP,
    torp:::RATING_PRIOR_GAMES_RECV
  )
})

test_that("simulation constants produce valid probabilities", {
  # Test that typical TORP difference produces reasonable WP
  # Using the formula: WP = pnorm(torp_diff / SIM_WP_SCALING_FACTOR)
  torp_diff <- 20  # Typical advantage
  wp <- pnorm(torp_diff / torp:::SIM_WP_SCALING_FACTOR)
  expect_true(wp > 0.5)  # Positive diff should give > 50% WP
  expect_true(wp < 0.9)  # Should not be too extreme for 20 point diff

  # Large diff should give high WP but not 1.0
  large_diff <- 100
  large_wp <- pnorm(large_diff / torp:::SIM_WP_SCALING_FACTOR)
  expect_true(large_wp > 0.95)
  expect_true(large_wp < 1.0)
})

test_that("home advantage constant affects simulations appropriately", {
  # Home advantage of 6 points with noise SD of 26
  # should give roughly 59% win rate for equal teams
  home_adv <- torp:::SIM_HOME_ADVANTAGE
  noise_sd <- torp:::SIM_NOISE_SD

  # Using normal approximation: P(home wins) = pnorm(home_adv / noise_sd)
  expected_home_win_rate <- pnorm(home_adv / noise_sd)
  expect_true(expected_home_win_rate > 0.55)
  expect_true(expected_home_win_rate < 0.65)
})

# -----------------------------------------------------------------------------
# Type Checks
# -----------------------------------------------------------------------------

test_that("all constants are numeric", {
  expect_type(torp:::AFL_GOAL_WIDTH, "double")
  expect_type(torp:::AFL_QUARTER_DURATION, "double")
  expect_type(torp:::AFL_TOTAL_GAME_SECONDS, "double")
  expect_type(torp:::AFL_MAX_PERIODS, "double")
  expect_type(torp:::RATING_DECAY_DEFAULT_DAYS, "double")
  expect_type(torp:::SIM_NOISE_SD, "double")
  expect_type(torp:::SIM_HOME_ADVANTAGE, "double")
})

test_that("all constants are scalar (length 1)", {
  expect_length(torp:::AFL_GOAL_WIDTH, 1)
  expect_length(torp:::AFL_QUARTER_DURATION, 1)
  expect_length(torp:::AFL_TOTAL_GAME_SECONDS, 1)
  expect_length(torp:::AFL_MAX_PERIODS, 1)
  expect_length(torp:::SIM_NOISE_SD, 1)
  expect_length(torp:::SIM_HOME_ADVANTAGE, 1)
})
