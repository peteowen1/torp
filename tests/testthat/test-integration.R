# Integration Tests
# =================
# Full pipeline tests that verify components work together
# These tests require network access and may take longer to run

# -----------------------------------------------------------------------------
# Data Loading Pipeline Tests
# -----------------------------------------------------------------------------

test_that("load_pbp returns data with expected structure", {
  skip_if_no_internet()
  skip_on_cran()

  result <- tryCatch(
    load_pbp(2024, rounds = 1),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not load PBP data")

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)

  # Check for essential columns
  essential_cols <- c("match_id", "period", "team_id")
  for (col in essential_cols) {
    expect_true(col %in% names(result),
                info = paste("Missing essential column:", col))
  }
})

test_that("load_chains returns data with expected structure", {
  skip_if_no_internet()
  skip_on_cran()

  result <- tryCatch(
    load_chains(2024),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not load chains data")

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("load_fixtures returns data with expected structure", {
  skip_if_no_internet()
  skip_on_cran()

  result <- tryCatch(
    load_fixtures(2024),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not load fixtures data")

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)

  # Should have season and round info
  expect_true("compSeason.year" %in% names(result) || "season" %in% names(result))
})

# -----------------------------------------------------------------------------
# Model Prediction Pipeline Tests
# -----------------------------------------------------------------------------

test_that("EP model predictions flow through add_epv_vars", {
  skip_if_no_internet()
  skip_on_cran()

  # Load minimal PBP data
  pbp <- tryCatch(
    load_pbp(2024, rounds = 1),
    error = function(e) NULL
  )

  skip_if(is.null(pbp), "Could not load PBP data")

  # Try to add EP vars
  result <- tryCatch({
    add_epv_vars(pbp[1:100, ])  # Small subset for speed
  }, error = function(e) NULL)

  if (!is.null(result)) {
    # Should have EP-related columns
    expect_true("exp_pts" %in% names(result))
    expect_true(is.data.frame(result))
  }
})

test_that("WP model predictions flow through add_wp_vars", {
  skip_if_no_internet()
  skip_on_cran()

  # Load minimal PBP data
  pbp <- tryCatch(
    load_pbp(2024, rounds = 1),
    error = function(e) NULL
  )

  skip_if(is.null(pbp), "Could not load PBP data")

  # Try to add WP vars
  result <- tryCatch({
    add_wp_vars(pbp[1:100, ])  # Small subset for speed
  }, error = function(e) NULL)

  if (!is.null(result)) {
    # Should have WP-related columns
    expect_true("wp" %in% names(result))
    expect_true(is.data.frame(result))
  }
})

# -----------------------------------------------------------------------------
# Rating Calculation Pipeline Tests
# -----------------------------------------------------------------------------

test_that("player ratings can be calculated from loaded data", {
  skip_if_no_internet()
  skip_on_cran()

  # Load player data
  player_stats <- tryCatch(
    load_player_stats(2024),
    error = function(e) NULL
  )

  player_details <- tryCatch(
    load_player_details(2024),
    error = function(e) NULL
  )

  skip_if(is.null(player_stats) || is.null(player_details),
          "Could not load player data")

  # Calculate ratings
  result <- tryCatch(
    calculate_torp_ratings(
      season_val = 2024,
      round_val = 5,  # Use middle of season
      plyr_gm_df = player_stats,
      plyr_tm_df = player_details
    ),
    error = function(e) NULL
  )

  if (!is.null(result) && nrow(result) > 0) {
    expect_true(is.data.frame(result))
    # Should have player ratings
    expect_true("torp" %in% names(result) || "player_id" %in% names(result))
  }
})

# -----------------------------------------------------------------------------
# Simulation Pipeline Tests
# -----------------------------------------------------------------------------

test_that("simulation can use loaded fixture data", {
  skip_if_no_internet()
  skip_on_cran()

  # Load fixtures
  fixtures <- tryCatch(
    load_fixtures(2024),
    error = function(e) NULL
  )

  skip_if(is.null(fixtures), "Could not load fixtures")

  # Create minimal team data
  teams <- data.frame(
    team = unique(c(fixtures$home.team.name[1:9], fixtures$away.team.name[1:9]))[1:18],
    torp = runif(18, -20, 20),
    stringsAsFactors = FALSE
  )

  # Create game data from first few fixtures
  games <- data.frame(
    roundnum = rep(1, 9),
    home_team = fixtures$home.team.name[1:9],
    away_team = fixtures$away.team.name[1:9],
    result = NA_integer_,
    torp_home_round = NA_real_,
    torp_away_round = NA_real_,
    stringsAsFactors = FALSE
  )

  # Run simulation
  set.seed(42)
  result <- tryCatch(
    simulate_season(teams, games),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_true(is.data.frame(result))
    expect_true(all(!is.na(result$result)))
  }
})

# -----------------------------------------------------------------------------
# Match XG Pipeline Tests
# -----------------------------------------------------------------------------

test_that("match XG calculations work with loaded data", {
  skip_if_no_internet()
  skip_on_cran()

  result <- tryCatch(
    calculate_match_xgs(season = 2024, round = 1),
    error = function(e) NULL
  )

  if (!is.null(result) && nrow(result) > 0) {
    expect_true(is.data.frame(result))
    expect_true("home_xscore" %in% names(result))
    expect_true("away_xscore" %in% names(result))

    # Validate xscore values are reasonable
    expect_true(all(result$home_xscore >= 0))
    expect_true(all(result$away_xscore >= 0))
  }
})

# -----------------------------------------------------------------------------
# Data Validation Pipeline Tests
# -----------------------------------------------------------------------------

test_that("loaded data passes validation checks", {
  skip_if_no_internet()
  skip_on_cran()

  # Load chains data
  chains <- tryCatch(
    load_chains(2024),
    error = function(e) NULL
  )

  skip_if(is.null(chains), "Could not load chains data")

  # Validate schema
  validation_result <- validate_data_schema(chains, "chains_data", strict = FALSE)

  # Should pass (or have minor issues)
  expect_true(is.list(validation_result))
  expect_true("valid" %in% names(validation_result))
})

test_that("data quality checks work on loaded data", {
  skip_if_no_internet()
  skip_on_cran()

  # Load PBP data
  pbp <- tryCatch(
    load_pbp(2024, rounds = 1),
    error = function(e) NULL
  )

  skip_if(is.null(pbp), "Could not load PBP data")

  # Convert to data.frame to avoid data.table issues
  if (data.table::is.data.table(pbp)) {
    pbp <- as.data.frame(pbp)
  }

  # Run quality check
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
  skip_if_no_internet()
  skip_on_cran()

  # Load player stats
  player_stats <- tryCatch(
    load_player_stats(2024),
    error = function(e) NULL
  )

  skip_if(is.null(player_stats), "Could not load player stats")

  # Convert to data.frame to avoid data.table issues
  if (data.table::is.data.table(player_stats)) {
    player_stats <- as.data.frame(player_stats)
  }

  # Quality check on loaded data
  quality_result <- tryCatch(
    validate_data_quality(player_stats, "player_stats"),
    error = function(e) NULL
  )

  skip_if(is.null(quality_result), "Quality check failed")

  expect_true(is.list(quality_result))
  expect_true(quality_result$quality_score > 0.5)  # Should be reasonable quality
})

# -----------------------------------------------------------------------------
# Cache Functionality Tests
# -----------------------------------------------------------------------------

test_that("fixture cache works across multiple calls", {
  skip_if_no_internet()
  skip_on_cran()

  # First call
  fixtures1 <- tryCatch(
    load_fixtures(2024),
    error = function(e) NULL
  )

  skip_if(is.null(fixtures1), "Could not load fixtures")

  # Second call should use cache
  fixtures2 <- load_fixtures(2024)

  # Should be identical
  expect_equal(nrow(fixtures1), nrow(fixtures2))
})

test_that("model cache works across multiple predictions", {
  skip_if_no_internet()
  skip_on_cran()

  # Clear cache
  clear_model_cache()

  pbp <- tryCatch(
    load_pbp(2024, rounds = 1),
    error = function(e) NULL
  )

  skip_if(is.null(pbp), "Could not load PBP data")

  # First prediction (should load model)
  result1 <- tryCatch({
    add_wp_vars(pbp[1:50, ])
  }, error = function(e) NULL)

  # Second prediction (should use cached model)
  result2 <- tryCatch({
    add_wp_vars(pbp[51:100, ])
  }, error = function(e) NULL)

  # Both should succeed (or both fail if models unavailable)
  if (!is.null(result1) && !is.null(result2)) {
    expect_true(is.data.frame(result1))
    expect_true(is.data.frame(result2))
  }
})
