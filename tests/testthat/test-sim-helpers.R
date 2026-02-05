# Tests for sim-helpers.R
# Tests for simulate_season and process_games functions

# -----------------------------------------------------------------------------
# simulate_season Tests
# -----------------------------------------------------------------------------

test_that("simulate_season function exists and is exported", {
  expect_true(exists("simulate_season"))
  expect_true("simulate_season" %in% getNamespaceExports("torp"))
})

test_that("sim_season deprecated alias exists", {
  expect_true(exists("sim_season"))
  expect_true("sim_season" %in% getNamespaceExports("torp"))
})

test_that("sim_season shows deprecation warning", {
  sim_teams <- create_test_sim_teams(4)
  sim_games <- data.frame(
    roundnum = 1,
    home_team = "Adelaide Crows",
    away_team = "Brisbane Lions",
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_,
    stringsAsFactors = FALSE
  )

  # Should warn about deprecation
  set.seed(42)
  expect_warning(
    sim_season(sim_teams, sim_games),
    "deprecated|Deprecated"
  )
})

test_that("process_games is available as internal function", {
  expect_true(exists("process_games", envir = asNamespace("torp")))
})

test_that("simulate_season has correct function signature", {
  fn_args <- names(formals(simulate_season))
  expect_true("sim_teams" %in% fn_args)
  expect_true("sim_games" %in% fn_args)
})

# -----------------------------------------------------------------------------
# simulate_season Return Structure Tests
# -----------------------------------------------------------------------------

test_that("simulate_season returns correct structure with mock data", {
  # Create mock team data
  sim_teams <- create_test_sim_teams(n_teams = 4)

  # Create mock game data with some NA results (to be simulated)
  sim_games <- data.frame(
    roundnum = c(1, 1, 2, 2),
    home_team = c("Adelaide Crows", "Carlton", "Brisbane Lions", "Adelaide Crows"),
    away_team = c("Brisbane Lions", "Collingwood", "Carlton", "Collingwood"),
    result = c(NA, NA, NA, NA),
    torp_home_round = c(NA, NA, NA, NA),
    torp_away_round = c(NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )

  # Run simulation
  set.seed(42)
  result <- simulate_season(sim_teams, sim_games)

  # Check structure
  expect_s3_class(result, "data.frame")

  # Check that results were filled in
  expect_true(all(!is.na(result$result)))

  # Check that win probability was calculated
  expect_true("wp" %in% names(result))
  expect_true(all(result$wp >= 0 & result$wp <= 1))

  # Check that outcomes were determined
  expect_true("outcome" %in% names(result))

  # Check that TORP ratings were recorded
  expect_true("torp_home_round" %in% names(result) || "home_torp" %in% names(result))
})

# -----------------------------------------------------------------------------
# Constants Usage Tests
# -----------------------------------------------------------------------------

test_that("simulation uses SIM_NOISE_SD constant", {
  # Verify the constant exists and has expected value
  expect_equal(torp:::SIM_NOISE_SD, 26)
})

test_that("simulation uses SIM_HOME_ADVANTAGE constant", {
  # Verify the constant exists and has expected value
  expect_equal(torp:::SIM_HOME_ADVANTAGE, 6)
})

test_that("simulation uses SIM_WP_SCALING_FACTOR constant", {
  expect_equal(torp:::SIM_WP_SCALING_FACTOR, 50)
})

test_that("process_games handles single round correctly", {
  process_games <- torp:::process_games

  # Create minimal mock data
  sim_teams <- data.frame(
    team = c("Team A", "Team B"),
    torp = c(10, -5),
    stringsAsFactors = FALSE
  )

  sim_games <- data.frame(
    roundnum = c(1),
    home_team = c("Team A"),
    away_team = c("Team B"),
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_,
    stringsAsFactors = FALSE
  )

  set.seed(123)
  result <- process_games(sim_teams, sim_games, round_num = 1)

  # Result should be a list with sim_teams and sim_games

  expect_type(result, "list")
  expect_true("sim_teams" %in% names(result))
  expect_true("sim_games" %in% names(result))

  # Check sim_games has result filled in
  expect_true(!is.na(result$sim_games$result[1]))
})

test_that("simulation win probability follows expected pattern", {
  # Team with higher TORP should have higher win probability on average
  sim_teams <- data.frame(
    team = c("Strong Team", "Weak Team"),
    torp = c(30, -30),  # Large TORP difference
    stringsAsFactors = FALSE
  )

  sim_games <- data.frame(
    roundnum = rep(1, 100),
    home_team = rep("Strong Team", 100),
    away_team = rep("Weak Team", 100),
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_,
    stringsAsFactors = FALSE
  )

  # Simulate multiple times
  set.seed(42)
  process_games <- torp:::process_games
  result <- process_games(sim_teams, sim_games, round_num = 1)

  # Strong team should have high win probability
  expect_gt(mean(result$sim_games$wp), 0.7)  # Should be heavily favored

  # More wins than losses for strong team
  wins <- sum(result$sim_games$result > 0)
  losses <- sum(result$sim_games$result < 0)
  expect_gt(wins, losses)
})

test_that("simulation respects home ground advantage", {
  # Create teams with equal ratings
  sim_teams <- data.frame(
    team = c("Home Team", "Away Team"),
    torp = c(0, 0),  # Equal ratings
    stringsAsFactors = FALSE
  )

  sim_games <- data.frame(
    roundnum = rep(1, 1000),
    home_team = rep("Home Team", 1000),
    away_team = rep("Away Team", 1000),
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_,
    stringsAsFactors = FALSE
  )

  set.seed(42)
  process_games <- torp:::process_games
  result <- process_games(sim_teams, sim_games, round_num = 1)

  # Home team should have > 50% win probability due to home advantage
  avg_wp <- mean(result$sim_games$wp)
  expect_gt(avg_wp, 0.5)
})
