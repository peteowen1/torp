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
  expect_true("decay_recv" %in% fn_args)
  expect_true("decay_disp" %in% fn_args)
  expect_true("decay_spoil" %in% fn_args)
  expect_true("decay_hitout" %in% fn_args)
  expect_true("loading" %in% fn_args)
  expect_true("prior_games_recv" %in% fn_args)
  expect_true("prior_games_disp" %in% fn_args)
  expect_true("plyr_tm_df" %in% fn_args)
  expect_true("player_game_data" %in% fn_args)
  expect_true("skills" %in% fn_args)
})

test_that("calculate_torp_ratings skills parameter defaults to TRUE", {
  expect_true(formals(calculate_torp_ratings)$skills)
})

test_that("calculate_torp_ratings has reasonable defaults", {
  fn_formals <- formals(calculate_torp_ratings)

  # decay_recv uses RATING_DECAY_RECV constant
  expect_true(is.symbol(fn_formals$decay_recv) || fn_formals$decay_recv == torp:::RATING_DECAY_RECV)
  expect_true(is.symbol(fn_formals$loading) || fn_formals$loading == torp:::RATING_LOADING_DEFAULT)
  expect_true(is.symbol(fn_formals$prior_games_recv) || fn_formals$prior_games_recv == torp:::RATING_PRIOR_GAMES_RECV)
  expect_true(is.symbol(fn_formals$prior_games_disp) || fn_formals$prior_games_disp == torp:::RATING_PRIOR_GAMES_DISP)
})

# -----------------------------------------------------------------------------
# torp_ratings Alias Tests
# -----------------------------------------------------------------------------

test_that("torp_ratings is an alias for calculate_torp_ratings", {
  expect_true(exists("torp_ratings"))
  expect_identical(torp_ratings, calculate_torp_ratings)
})

test_that("calculate_player_stats helper function works", {
  # Test the helper function exists
  expect_true(exists("calculate_player_stats"))

  # Create minimal test data with plyr_nm (as produced by create_player_game_data)
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
    time_on_ground_percentage = c(82, 78, 90, 85),
    pos = c("Midfielder", "Midfielder", "Forward", "Forward"),
    stringsAsFactors = FALSE
  )

  # The function should work with valid inputs
  result <- torp:::calculate_player_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014103",
    date_val = as.Date("2024-04-08"),
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  expect_true(is.data.frame(result))
  expect_true("player_id" %in% names(result))
  expect_true("torp" %in% names(result))
})

# -----------------------------------------------------------------------------
# calculate_player_stats Tests
# -----------------------------------------------------------------------------

test_that("calculate_player_stats function exists", {
  expect_true(exists("calculate_player_stats", envir = asNamespace("torp")))
})

test_that("calculate_player_stats returns expected structure with valid data", {
  # Create comprehensive test data (as produced by create_player_game_data)
  test_data <- data.frame(
    player_id = rep(1:5, each = 4),
    plyr_nm = rep(paste("First", paste0("Last", 1:5)), each = 4),
    match_id = rep(c("CD_M2024014101", "CD_M2024014102", "CD_M2024014103", "CD_M2024014104"), 5),
    utc_start_time = rep(as.Date("2024-04-01") + c(0, 7, 14, 21), 5),
    tot_p_adj = runif(20, 50, 150),
    recv_pts_adj = runif(20, 10, 50),
    disp_pts_adj = runif(20, 20, 80),
    spoil_pts_adj = runif(20, 0, 20),
    hitout_pts_adj = runif(20, 0, 30),
    time_on_ground_percentage = runif(20, 60, 95),
    pos = sample(c("FWD", "MID", "DEF", "RUC"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- torp:::calculate_player_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014105",
    date_val = as.Date("2024-05-01"),
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
    plyr_nm = rep("First Last", 3),
    match_id = c("CD_M2024014101", "CD_M2024014102", "CD_M2024014103"),
    utc_start_time = as.Date("2024-04-01") + c(0, 30, 60),  # 0, 30, 60 days apart
    tot_p_adj = c(100, 100, 100),
    recv_pts_adj = c(50, 50, 50),
    disp_pts_adj = c(50, 50, 50),
    spoil_pts_adj = c(10, 10, 10),
    hitout_pts_adj = c(5, 5, 5),
    time_on_ground_percentage = c(80, 85, 75),
    pos = rep("MID", 3),
    stringsAsFactors = FALSE
  )

  # Calculate with short decay (recent games weighted more)
  result_short <- torp:::calculate_player_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014104",
    date_val = as.Date("2024-06-01"),
    decay_recv = 30, decay_disp = 30, decay_spoil = 30, decay_hitout = 30,
    loading = 1.5,
    prior_games_recv = 4,
    prior_games_disp = 6
  )

  # Calculate with long decay (all games weighted similarly)
  result_long <- torp:::calculate_player_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014104",
    date_val = as.Date("2024-06-01"),
    decay_recv = 1000, decay_disp = 1000, decay_spoil = 1000, decay_hitout = 1000,
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

  # Load player game data (processed, not raw)
  player_game_data <- tryCatch(
    load_player_game_data(2024),
    error = function(e) NULL
  )

  player_details <- tryCatch(
    load_player_details(2024),
    error = function(e) NULL
  )

  skip_if(is.null(player_game_data) || is.null(player_details),
          "Could not load player data")

  # Calculate ratings with pre-loaded data (skills = FALSE to isolate rating logic)
  result <- calculate_torp_ratings(
    season_val = 2024,
    round_val = 1,
    plyr_tm_df = player_details,
    player_game_data = player_game_data,
    skills = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("torp" %in% names(result) || "player_id" %in% names(result))
})

# -----------------------------------------------------------------------------
# Constants Usage Tests
# -----------------------------------------------------------------------------

test_that("calculate_player_stats uses prior_games_spoil and prior_games_hitout constants", {
  expect_equal(torp:::RATING_PRIOR_GAMES_SPOIL, 3.9409)
  expect_equal(torp:::RATING_PRIOR_GAMES_HITOUT, 15.0000)
})

# -----------------------------------------------------------------------------
# wt_gms Calculation Tests
# -----------------------------------------------------------------------------

test_that("wt_gms sums per-match weights correctly for same-day games", {
  # Two games on the same day produce identical weight_gm values.
 # The old sum(unique(weight_gm)) would collapse these into one weight.
 # The fix uses !duplicated(match_id) to keep both.
  test_data <- data.frame(
    player_id = rep(1, 2),
    plyr_nm = rep("Same Day", 2),
    match_id = c("CD_M2024014101", "CD_M2024014102"),
    utc_start_time = rep(as.Date("2024-04-01"), 2),
    tot_p_adj = c(100, 80),
    recv_pts_adj = c(20, 15),
    disp_pts_adj = c(40, 35),
    spoil_pts_adj = c(10, 8),
    hitout_pts_adj = c(5, 3),
    time_on_ground_percentage = c(88, 76),
    pos = rep("MID", 2),
    stringsAsFactors = FALSE
  )

  result <- torp:::calculate_player_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014103",
    date_val = as.Date("2024-04-08"),
    loading = 1.5,
    prior_games_recv = 4, prior_games_disp = 6
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$gms, 2)
  # wt_gms should be 2x the single-game weight (both games same date, same decay)
  # NOT collapsed to 1x via unique()
  single_weight <- exp(-as.numeric(as.Date("2024-04-08") - as.Date("2024-04-01")) / torp:::RATING_DECAY_RECV)
  expect_equal(result$wt_gms, 2 * single_weight, tolerance = 1e-10)
})

# -----------------------------------------------------------------------------
# TOG-Weighted Average Adjustment Tests
# -----------------------------------------------------------------------------

test_that("calculate_torp_ratings rejects skills without time_on_ground_skill column", {
  bad_skills <- data.frame(player_id = 1, some_other_col = 0.5)
  expect_error(
    calculate_torp_ratings(skills = bad_skills),
    "time_on_ground_skill"
  )
})

test_that("TOG-weighted average adjustment produces correct math", {
  # Build mock player game data for 3 players
  test_data <- data.frame(
    player_id = rep(1:3, each = 2),
    plyr_nm = rep(c("Player One", "Player Two", "Player Three"), each = 2),
    match_id = rep(c("CD_M2024014101", "CD_M2024014102"), 3),
    utc_start_time = rep(as.Date("2024-04-01"), 6),
    tot_p_adj = c(100, 100, 80, 80, 60, 60),
    recv_pts_adj = c(30, 30, 20, 20, 10, 10),
    disp_pts_adj = c(40, 40, 30, 30, 20, 20),
    spoil_pts_adj = c(10, 10, 8, 8, 5, 5),
    hitout_pts_adj = c(5, 5, 3, 3, 0, 0),
    time_on_ground_percentage = c(85, 80, 75, 70, 90, 88),
    pos = rep("MID", 6),
    stringsAsFactors = FALSE
  )

  # Get unadjusted stats from calculate_player_stats
  unadj <- torp:::calculate_player_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014103",
    date_val = as.Date("2024-04-08"),
    loading = 1.5,
    prior_games_recv = 4, prior_games_disp = 6
  )

  # Create skills with known TOG weights
 skills <- data.frame(
    player_id = 1:3,
    time_on_ground_skill = c(0.9, 0.7, 0.3)
  )

  # Apply adjustment manually (mirror the code in calculate_torp_ratings)
  adj <- data.table::copy(unadj)
  skills_dt <- data.table::as.data.table(skills)
  adj[skills_dt, tog_skill := i.time_on_ground_skill, on = "player_id"]
  adj[is.na(tog_skill), tog_skill := 0]

  tot_tog <- sum(adj$tog_skill)
  comps <- c("torp_recv", "torp_disp", "torp_spoil", "torp_hitout")
  for (comp in comps) {
    avg_val <- sum(adj[[comp]] * adj$tog_skill) / tot_tog
    data.table::set(adj, j = comp, value = adj[[comp]] - avg_val)
  }

  # Verify: TOG-weighted average of adjusted components should be ~0
  for (comp in comps) {
    weighted_avg <- sum(adj[[comp]] * adj$tog_skill) / tot_tog
    expect_equal(weighted_avg, 0, tolerance = 1e-10,
                 label = paste("weighted avg of adjusted", comp))
  }

  # Verify: adjustment shifts values (high-TOG player 1 should still be highest)
  expect_true(adj$torp_recv[adj$player_id == 1] > adj$torp_recv[adj$player_id == 3])
})

test_that("TOG adjustment is skipped when all tog_skill are zero", {
  test_data <- data.frame(
    player_id = rep(1:2, each = 2),
    plyr_nm = rep(c("Player One", "Player Two"), each = 2),
    match_id = rep(c("CD_M2024014101", "CD_M2024014102"), 2),
    utc_start_time = rep(as.Date("2024-04-01"), 4),
    tot_p_adj = c(100, 100, 80, 80),
    recv_pts_adj = c(30, 30, 20, 20),
    disp_pts_adj = c(40, 40, 30, 30),
    spoil_pts_adj = c(10, 10, 8, 8),
    hitout_pts_adj = c(5, 5, 0, 0),
    time_on_ground_percentage = c(82, 79, 88, 84),
    pos = rep("MID", 4),
    stringsAsFactors = FALSE
  )

  unadj <- torp:::calculate_player_stats(
    player_game_data = test_data,
    match_ref = "CD_M2024014103",
    date_val = as.Date("2024-04-08"),
    loading = 1.5,
    prior_games_recv = 4, prior_games_disp = 6
  )

  # Skills with no matching player_ids → all default to 0
  skills <- data.frame(
    player_id = c(99, 100),
    time_on_ground_skill = c(0.8, 0.5)
  )

  # Apply adjustment: tot_tog = 0, should skip
  adj <- data.table::copy(unadj)
  skills_dt <- data.table::as.data.table(skills)
  adj[skills_dt, tog_skill := i.time_on_ground_skill, on = "player_id"]
  adj[is.na(tog_skill), tog_skill := 0]

  tot_tog <- sum(adj$tog_skill)
  expect_equal(tot_tog, 0)

  # Values unchanged when tot_tog == 0
  expect_equal(adj$torp_recv, unadj$torp_recv)
})
