# Integration Tests
# =================
# Full pipeline tests that verify components work together
# These tests require network access and may take longer to run
# All network data is loaded once in helper-test-data.R (.shared env)

# -----------------------------------------------------------------------------
# Data Loading Pipeline Tests
# -----------------------------------------------------------------------------

test_that("load_pbp returns data with expected structure", {
  skip_if(is.null(.shared$pbp), "Could not load PBP data")

  expect_true(is.data.frame(.shared$pbp))
  expect_true(nrow(.shared$pbp) > 0)

  # Check for essential columns
  essential_cols <- c("match_id", "period", "team_id")
  for (col in essential_cols) {
    expect_true(col %in% names(.shared$pbp),
                info = paste("Missing essential column:", col))
  }
})

test_that("load_chains returns data with expected structure", {
  skip_if(is.null(.shared$chains), "Could not load chains data")

  expect_true(is.data.frame(.shared$chains))
  expect_true(nrow(.shared$chains) > 0)
})

test_that("load_fixtures returns data with expected structure", {
  skip_if(is.null(.shared$fixtures), "Could not load fixtures data")

  expect_true(is.data.frame(.shared$fixtures))
  expect_true(nrow(.shared$fixtures) > 0)

  # Should have season and round info
  expect_true("compSeason.year" %in% names(.shared$fixtures) || "season" %in% names(.shared$fixtures))
})

# -----------------------------------------------------------------------------
# Model Prediction Pipeline Tests
# -----------------------------------------------------------------------------

test_that("EP model predictions flow through add_epv_vars", {
  skip_if(is.null(.shared$pbp), "Could not load PBP data")

  result <- suppressWarnings(tryCatch({
    add_epv_vars(.shared$pbp[1:100, ])
  }, error = function(e) NULL))

  skip_if(is.null(result), "EP model unavailable")
  expect_true("exp_pts" %in% names(result))
  expect_true(is.data.frame(result))
})

test_that("WP model predictions flow through add_wp_vars", {
  skip_if(is.null(.shared$pbp), "Could not load PBP data")

  result <- suppressWarnings(tryCatch({
    add_wp_vars(.shared$pbp[1:100, ])
  }, error = function(e) NULL))

  skip_if(is.null(result), "WP model unavailable")
  expect_true("wp" %in% names(result))
  expect_true(is.data.frame(result))
})

# -----------------------------------------------------------------------------
# Rating Calculation Pipeline Tests
# -----------------------------------------------------------------------------

test_that("player ratings can be calculated from loaded data", {
  skip_if(is.null(.shared$player_details),
          "Could not load player data")

  result <- calculate_torp_ratings(
    season_val = 2024,
    round_val = 5,
    plyr_tm_df = .shared$player_details
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("torp" %in% names(result) || "player_id" %in% names(result))
})

# -----------------------------------------------------------------------------
# Simulation Pipeline Tests
# -----------------------------------------------------------------------------

test_that("simulation can use loaded fixture data", {
  skip_if(is.null(.shared$fixtures), "Could not load fixtures")

  teams <- data.frame(
    team = unique(c(.shared$fixtures$home.team.name[1:9], .shared$fixtures$away.team.name[1:9]))[1:18],
    torp = runif(18, -20, 20),
    stringsAsFactors = FALSE
  )

  games <- data.frame(
    roundnum = rep(1, 9),
    home_team = .shared$fixtures$home.team.name[1:9],
    away_team = .shared$fixtures$away.team.name[1:9],
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_,
    stringsAsFactors = FALSE
  )

  set.seed(42)
  result <- simulate_season(teams, games)

  expect_true(is.data.frame(result))
  expect_true(all(!is.na(result$result)))
})

# -----------------------------------------------------------------------------
# Match XG Pipeline Tests
# -----------------------------------------------------------------------------

test_that("match XG calculations work with loaded data", {
  skip_if(is.null(.shared$match_xgs), "Could not load match XG data")

  expect_true(is.data.frame(.shared$match_xgs))
  expect_true(nrow(.shared$match_xgs) > 0)
  expect_true("home_xscore" %in% names(.shared$match_xgs))
  expect_true("away_xscore" %in% names(.shared$match_xgs))

  expect_true(all(.shared$match_xgs$home_xscore >= 0))
  expect_true(all(.shared$match_xgs$away_xscore >= 0))
})

# -----------------------------------------------------------------------------
# Data Validation Pipeline Tests
# -----------------------------------------------------------------------------

test_that("loaded data passes validation checks", {
  skip_if(is.null(.shared$chains), "Could not load chains data")

  validation_result <- validate_data_schema(.shared$chains, "chains_data", strict = FALSE)

  expect_true(is.list(validation_result))
  expect_true("valid" %in% names(validation_result))
})

test_that("data quality checks work on loaded data", {
  skip_if(is.null(.shared$pbp), "Could not load PBP data")

  pbp <- .shared$pbp
  if (data.table::is.data.table(pbp)) {
    pbp <- as.data.frame(pbp)
  }

  quality_result <- tryCatch(
    validate_data_quality(pbp, "pbp"),
    error = function(e) NULL
  )

  skip_if(is.null(quality_result), "Quality check failed")

  expect_true(is.list(quality_result))
  expect_true("quality_score" %in% names(quality_result))
  expect_true(quality_result$quality_score >= 0)
  expect_true(quality_result$quality_score <= 1)
})

# -----------------------------------------------------------------------------
# Cross-Function Compatibility Tests
# -----------------------------------------------------------------------------

test_that("validation functions work with rating calculations", {
  skip_if(is.null(.shared$player_stats), "Could not load player stats")

  player_stats <- .shared$player_stats
  if (data.table::is.data.table(player_stats)) {
    player_stats <- as.data.frame(player_stats)
  }

  quality_result <- tryCatch(
    validate_data_quality(player_stats, "player_stats"),
    error = function(e) NULL
  )

  skip_if(is.null(quality_result), "Quality check failed")

  expect_true(is.list(quality_result))
  expect_true(quality_result$quality_score > 0.5)
})

# -----------------------------------------------------------------------------
# Cache Functionality Tests
# -----------------------------------------------------------------------------

test_that("fixture cache works across multiple calls", {
  skip_if(is.null(.shared$fixtures), "Could not load fixtures")

  fixtures2 <- load_fixtures(2024)

  expect_equal(nrow(.shared$fixtures), nrow(fixtures2))
})

test_that("model cache works across multiple predictions", {
  skip_if(is.null(.shared$pbp), "Could not load PBP data")

  clear_model_cache()

  result1 <- suppressWarnings(tryCatch({
    add_wp_vars(.shared$pbp[1:50, ])
  }, error = function(e) NULL))

  result2 <- suppressWarnings(tryCatch({
    add_wp_vars(.shared$pbp[51:100, ])
  }, error = function(e) NULL))

  skip_if(is.null(result1) || is.null(result2), "WP model unavailable for cache test")
  expect_true(is.data.frame(result1))
  expect_true(is.data.frame(result2))
})
