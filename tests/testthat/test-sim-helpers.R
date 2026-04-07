# Tests for sim-helpers.R
# Tests for simulate_season and process_games functions

# -----------------------------------------------------------------------------
# simulate_season Tests
# -----------------------------------------------------------------------------

test_that("simulate_season function exists and is exported", {
  expect_true(exists("simulate_season"))
  expect_true("simulate_season" %in% getNamespaceExports("torp"))
})

test_that("sim_season deprecated alias has been removed", {
  expect_false("sim_season" %in% getNamespaceExports("torp"))
})

test_that("process_games_dt is available as internal function", {
  expect_true(exists("process_games_dt", envir = asNamespace("torp")))
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

test_that("process_games_dt handles single round correctly", {
  process_games_dt <- torp:::process_games_dt

  # Create minimal mock data as data.tables
  sim_teams <- data.table::data.table(
    team = c("Team A", "Team B"),
    torp = c(10, -5)
  )

  sim_games <- data.table::data.table(
    roundnum = 1L,
    home_team = "Team A",
    away_team = "Team B",
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_
  )

  set.seed(123)
  result <- process_games_dt(sim_teams, sim_games, round_num = 1)

  expect_type(result, "list")
  expect_true("sim_teams" %in% names(result))
  expect_true("sim_games" %in% names(result))

  # Check sim_games has result filled in
  expect_true(!is.na(result$sim_games$result[1]))
})

test_that("simulation win probability follows expected pattern", {
  # Team with higher TORP should have higher win probability on average
  sim_teams <- data.table::data.table(
    team = c("Strong Team", "Weak Team"),
    torp = c(30, -30)
  )

  sim_games <- data.table::data.table(
    roundnum = rep(1L, 100),
    home_team = rep("Strong Team", 100),
    away_team = rep("Weak Team", 100),
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_
  )

  set.seed(42)
  process_games_dt <- torp:::process_games_dt
  result <- process_games_dt(sim_teams, sim_games, round_num = 1)

  # Strong team should have high win probability
  expect_gt(mean(result$sim_games$wp), 0.7)

  # More wins than losses for strong team
  wins <- sum(result$sim_games$result > 0)
  losses <- sum(result$sim_games$result < 0)
  expect_gt(wins, losses)
})

# -----------------------------------------------------------------------------
# Injury Schedule Integration Tests
# -----------------------------------------------------------------------------

test_that("simulate_season applies injury_schedule boosts at correct round", {
  sim_teams <- data.frame(
    team = c("Team A", "Team B"),
    torp = c(0, 0),
    stringsAsFactors = FALSE
  )

  sim_games <- data.frame(
    roundnum = c(1L, 1L, 2L, 2L),
    home_team = c("Team A", "Team A", "Team A", "Team A"),
    away_team = c("Team B", "Team B", "Team B", "Team B"),
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_,
    stringsAsFactors = FALSE
  )

  # Team A gets a +20 TORP boost at round 2 (star player returns)
  inj_sched <- data.table::data.table(
    team = "Team A",
    torp_boost = 20,
    return_round = 2L
  )

  set.seed(42)
  result <- simulate_season(sim_teams, sim_games, return_teams = TRUE,
                            injury_schedule = inj_sched)

  games_dt <- result[["games"]]
  teams_dt <- result[["teams"]]

  # Round 1: teams start equal (torp = 0 each)
  # Round 2: Team A gets +20 boost before games are simulated
  r1_games <- games_dt[games_dt$roundnum == 1L, ]
  r2_games <- games_dt[games_dt$roundnum == 2L, ]

  # Team A's recorded TORP in round 2 should be higher than round 1
  # (torp_home_round captures the pre-game rating)
  expect_gt(mean(r2_games$torp_home_round), mean(r1_games$torp_home_round))

  # Final team rating for Team A should reflect the boost
  final_a <- teams_dt[teams_dt$team == "Team A", ]$torp
  final_b <- teams_dt[teams_dt$team == "Team B", ]$torp
  expect_gt(final_a, final_b)
})

test_that("simulate_season works with empty injury_schedule", {
  sim_teams <- data.frame(
    team = c("Team A", "Team B"),
    torp = c(5, -5),
    stringsAsFactors = FALSE
  )

  sim_games <- data.frame(
    roundnum = c(1L),
    home_team = "Team A",
    away_team = "Team B",
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_,
    stringsAsFactors = FALSE
  )

  empty_sched <- data.table::data.table(
    team = character(), torp_boost = numeric(), return_round = integer()
  )

  set.seed(42)
  result <- simulate_season(sim_teams, sim_games, injury_schedule = empty_sched)
  expect_s3_class(result, "data.frame")
  expect_true(all(!is.na(result$result)))
})

test_that("simulation respects home ground advantage", {
  sim_teams <- data.table::data.table(
    team = c("Home Team", "Away Team"),
    torp = c(0, 0)
  )

  sim_games <- data.table::data.table(
    roundnum = rep(1L, 1000),
    home_team = rep("Home Team", 1000),
    away_team = rep("Away Team", 1000),
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_
  )

  set.seed(42)
  process_games_dt <- torp:::process_games_dt
  result <- process_games_dt(sim_teams, sim_games, round_num = 1)

  # Home team should have > 50% win probability due to home advantage
  avg_wp <- mean(result$sim_games$wp)
  expect_gt(avg_wp, 0.5)
})
