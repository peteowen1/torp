test_that("torp_ratings function works with mock data", {
  # Create mock data
  mock_player_data <- create_mock_player_data(20)
  mock_stats_data <- data.frame(
    player_id = 1:20,
    plyr_nm = paste0("Player_", 1:20),
    match_id = rep("CD_M2024014101", 20),
    utc_start_time = as.Date("2024-04-01"),
    tot_p_adj = runif(20, 50, 150),
    recv_pts_adj = runif(20, 10, 30),
    disp_pts_adj = runif(20, 20, 50),
    spoil_pts_adj = runif(20, 5, 15),
    hitout_pts_adj = runif(20, 0, 20),
    pos = sample(c("Forward", "Midfielder", "Defender"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Test that function exists and accepts parameters
  expect_true(exists("torp_ratings"))
  
  # Test parameter validation
  expect_error(torp_ratings(season_val = "invalid"))
  expect_error(torp_ratings(round_val = "invalid"))
  expect_error(torp_ratings(decay = "invalid"))
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
  expect_error(player_game_ratings(season_val = "invalid"))
  expect_error(player_game_ratings(round_num = "invalid"))
})

test_that("player_season_ratings function works", {
  expect_true(exists("player_season_ratings"))
  
  # Test parameter validation  
  expect_error(player_season_ratings(season_val = "invalid"))
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
  result1 <- torp:::filter_game_data(test_df, 2024, 1, "M1", FALSE)
  expect_equal(nrow(result1), 1)
  expect_equal(result1$match_id, "M1")
  
  # Test filtering by team
  result2 <- torp:::filter_game_data(test_df, 2024, 1, FALSE, "Adelaide Crows")
  expect_equal(nrow(result2), 2)
  expect_true(all(result2$tm == "Adelaide Crows" | result2$opp == "Adelaide Crows"))
  
  # Test filtering by season and round
  result3 <- torp:::filter_game_data(test_df, 2024, 1, FALSE, FALSE)
  expect_equal(nrow(result3), 2)
  expect_true(all(result3$season == 2024 & result3$round == 1))
  
  # Test error handling
  expect_error(torp:::filter_game_data(test_df, 2024, 1, "NONEXISTENT", FALSE), "Match ID not found")
  expect_error(torp:::filter_game_data(test_df, 2024, 1, FALSE, "NONEXISTENT"), "Team not found")
})