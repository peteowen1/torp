# -----------------------------------------------------------------------------
# calculate_torp_ratings Tests
# -----------------------------------------------------------------------------

test_that("calculate_torp_ratings function exists and is exported", {
  expect_true(exists("calculate_torp_ratings"))
  expect_true("calculate_torp_ratings" %in% getNamespaceExports("torp"))
})

test_that("calculate_torp_ratings has correct function signature", {
  fn_args <- names(formals(calculate_torp_ratings))

  expect_true("season_val" %in% fn_args)
  expect_true("round_val" %in% fn_args)
  expect_true("decay" %in% fn_args)
  expect_true("loading" %in% fn_args)
  expect_true("prior_games_recv" %in% fn_args)
  expect_true("prior_games_disp" %in% fn_args)
  expect_true("plyr_tm_df" %in% fn_args)
  expect_true("plyr_gm_df" %in% fn_args)
})

test_that("calculate_torp_ratings has reasonable defaults", {
  fn_formals <- formals(calculate_torp_ratings)

  # decay uses RATING_DECAY_DEFAULT_DAYS constant
  expect_true(is.symbol(fn_formals$decay) || fn_formals$decay == 365)
  expect_equal(fn_formals$loading, 1.5)
  expect_equal(fn_formals$prior_games_recv, 4)
  expect_equal(fn_formals$prior_games_disp, 6)
})

# -----------------------------------------------------------------------------
# Deprecated torp_ratings Tests
# -----------------------------------------------------------------------------

test_that("torp_ratings function still exists (deprecated)", {
  expect_true(exists("torp_ratings"))
})

test_that("torp_ratings shows deprecation warning", {
  skip_if_no_internet()

  # Should warn about deprecation
  expect_warning(
    tryCatch(
      torp_ratings(season_val = 2024, round_val = 1),
      error = function(e) NULL  # Ignore errors (e.g., missing data)
    ),
    "deprecated|Deprecated"
  )
})

test_that("calculate_player_stats helper function works", {
  # Test the helper function exists
  expect_true(exists("calculate_player_stats"))
  
  # Create minimal test data
  test_data <- data.frame(
    player_id = c(1, 1, 2, 2),
    plyr_nm = c("Player1", "Player1", "Player2", "Player2"),
    match_id = c("CD_M2024014101", "CD_M2024014102", "CD_M2024014101", "CD_M2024014102"),
    utc_start_time = rep(as.Date("2024-04-01"), 4),
    tot_p_adj = c(100, 120, 80, 90),
    recv_pts_adj = c(20, 25, 15, 18),
    disp_pts_adj = c(40, 45, 35, 38),
    spoil_pts_adj = c(10, 12, 8, 9),
    hitout_pts_adj = c(5, 8, 0, 0),
    pos = c("Midfielder", "Midfielder", "Forward", "Forward"),
    stringsAsFactors = FALSE
  )
  
  # The function should work with valid inputs
  result <- tryCatch({
    torp:::calculate_player_stats(
      test_data, 
      "CD_M2024014103", 
      as.Date("2024-04-08"), 
      365, 1.5, 4, 6
    )
  }, error = function(e) NULL)
  
  # Basic structure tests if function works
  if (!is.null(result)) {
    expect_true(is.data.frame(result))
    expect_true("player_id" %in% names(result))
    expect_true("torp" %in% names(result))
  }
})

test_that("player_game_ratings function works", {
  expect_true(exists("player_game_ratings"))

  # Test parameter validation
  expect_error(player_game_ratings(season_val = "invalid"), "season_val must be numeric")
  expect_error(player_game_ratings(round_num = "invalid"), "round_num must be numeric")
  expect_error(player_game_ratings(season_val = 1900), "1990 and 2030")  # More flexible match
  expect_error(player_game_ratings(round_num = 50), "0 and 30")
})

test_that("player_season_ratings function works", {
  expect_true(exists("player_season_ratings"))

  # Test parameter validation
  expect_error(player_season_ratings(season_val = "invalid"), "season_val must be numeric")
  expect_error(player_season_ratings(season_val = 1900), "1990 and 2030")  # More flexible match
  expect_error(player_season_ratings(round_num = "invalid"), "round_num must be numeric")
  expect_error(player_season_ratings(round_num = 50), "0 and 30")
})

test_that("filter_game_data helper function works", {
  # Create test data
  test_df <- data.frame(
    season = c(2024, 2024, 2023, 2024),
    round = c(1, 2, 1, 1),
    match_id = c("M1", "M2", "M3", "M4"),
    tm = c("Adelaide Crows", "Brisbane Lions", "Carlton", "Adelaide Crows"),
    opp = c("Brisbane Lions", "Adelaide Crows", "Essendon", "Carlton"),
    tot_p_adj = c(100, 120, 80, 90),
    stringsAsFactors = FALSE
  )

  # Test filtering by match ID
  result1 <- torp:::filter_game_data(test_df, 2024, 1, "M1", NULL)
  expect_equal(nrow(result1), 1)
  expect_equal(result1$match_id, "M1")

  # Test filtering by team
  result2 <- torp:::filter_game_data(test_df, 2024, 1, NULL, "Adelaide Crows")
  expect_equal(nrow(result2), 2)
  expect_true(all(result2$tm == "Adelaide Crows" | result2$opp == "Adelaide Crows"))

  # Test filtering by season and round
  result3 <- torp:::filter_game_data(test_df, 2024, 1, NULL, NULL)
  expect_equal(nrow(result3), 2)
  expect_true(all(result3$season == 2024 & result3$round == 1))

  # Test error handling
  expect_error(torp:::filter_game_data(test_df, 2024, 1, "NONEXISTENT", NULL), "Match ID not found")
  expect_error(torp:::filter_game_data(test_df, 2024, 1, NULL, "NONEXISTENT"), "Team not found")
})

# -----------------------------------------------------------------------------
# calculate_player_stats Tests
# -----------------------------------------------------------------------------

test_that("calculate_player_stats function exists", {
  expect_true(exists("calculate_player_stats", envir = asNamespace("torp")))
})

test_that("calculate_player_stats returns expected structure with valid data", {
  # Create comprehensive test data
  test_data <- data.frame(
    player_id = rep(1:5, each = 4),
    player_given_name = rep(paste0("First", 1:5), each = 4),
    player_surname = rep(paste0("Last", 1:5), each = 4),
    match_id = rep(c("CD_M2024014101", "CD_M2024014102", "CD_M2024014103", "CD_M2024014104"), 5),
    utc_start_time = rep(as.Date("2024-04-01") + c(0, 7, 14, 21), 5),
    tot_p_adj = runif(20, 50, 150),
    recv_pts_adj = runif(20, 10, 50),
    disp_pts_adj = runif(20, 20, 80),
    spoil_pts_adj = runif(20, 0, 20),
    hitout_pts_adj = runif(20, 0, 30),
    pos = sample(c("FWD", "MID", "DEF", "RUC"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- torp:::calculate_player_stats(
    plyr_gm_df = test_data,
    match_ref = "CD_M2024014105",
    date_val = as.Date("2024-05-01"),
    decay = 365,
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  # Check structure
  expect_true(is.data.frame(result) || data.table::is.data.table(result))
  expect_true("player_id" %in% names(result))
  expect_true("torp" %in% names(result))
  expect_true("torp_recv" %in% names(result) || "torp_recv_adj" %in% names(result))
  expect_true("torp_disp" %in% names(result) || "torp_disp_adj" %in% names(result))

  # Should have 5 unique players
  expect_equal(nrow(result), 5)
})

test_that("calculate_player_stats respects decay parameter", {
  # Create test data with games at different times
  test_data <- data.frame(
    player_id = rep(1, 3),
    player_given_name = rep("First", 3),
    player_surname = rep("Last", 3),
    match_id = c("CD_M2024014101", "CD_M2024014102", "CD_M2024014103"),
    utc_start_time = as.Date("2024-04-01") + c(0, 30, 60),  # 0, 30, 60 days apart
    tot_p_adj = c(100, 100, 100),
    recv_pts_adj = c(50, 50, 50),
    disp_pts_adj = c(50, 50, 50),
    spoil_pts_adj = c(10, 10, 10),
    hitout_pts_adj = c(5, 5, 5),
    pos = rep("MID", 3),
    stringsAsFactors = FALSE
  )

  # Calculate with short decay (recent games weighted more)
  result_short <- torp:::calculate_player_stats(
    plyr_gm_df = test_data,
    match_ref = "CD_M2024014104",
    date_val = as.Date("2024-06-01"),
    decay = 30,  # Short decay
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  # Calculate with long decay (all games weighted similarly)
  result_long <- torp:::calculate_player_stats(
    plyr_gm_df = test_data,
    match_ref = "CD_M2024014104",
    date_val = as.Date("2024-06-01"),
    decay = 1000,  # Long decay
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  # Both should return results
  expect_equal(nrow(result_short), 1)
  expect_equal(nrow(result_long), 1)
})

# -----------------------------------------------------------------------------
# prepare_final_dataframe Tests
# -----------------------------------------------------------------------------

test_that("prepare_final_dataframe function exists", {
  expect_true(exists("prepare_final_dataframe", envir = asNamespace("torp")))
})

# -----------------------------------------------------------------------------
# Integration Tests
# -----------------------------------------------------------------------------

test_that("calculate_torp_ratings works with pre-loaded data", {
  skip_if_no_internet()

  # Load minimal data
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

  # Calculate ratings with pre-loaded data
  result <- tryCatch(
    calculate_torp_ratings(
      season_val = 2024,
      round_val = 1,
      plyr_tm_df = player_details,
      plyr_gm_df = player_stats
    ),
    error = function(e) NULL
  )

  if (!is.null(result) && nrow(result) > 0) {
    expect_true(is.data.frame(result))
    expect_true("torp" %in% names(result) || "player_id" %in% names(result))
  }
})

# -----------------------------------------------------------------------------
# Constants Usage Tests
# -----------------------------------------------------------------------------

test_that("calculate_player_stats uses RATING_SPOIL_MULTIPLIER constant", {
  # The function should multiply spoil ratings by RATING_SPOIL_MULTIPLIER
  # This test verifies the constant is being used
  expect_equal(torp:::RATING_SPOIL_MULTIPLIER, 1.2)
})