# Test helper functions and fixtures
# This file is sourced before running tests

# Create mock data for testing
create_mock_pbp_data <- function(n_rows = 100) {
  data.frame(
    match_id = rep(paste0("CD_M2024014", sprintf("%02d", 1:5)), length.out = n_rows),
    period = sample(1:4, n_rows, replace = TRUE),
    period_seconds = sample(1:1800, n_rows, replace = TRUE),
    team_id = sample(1:18, n_rows, replace = TRUE),
    home_team_id = sample(1:18, n_rows, replace = TRUE),
    away_team_id = sample(1:18, n_rows, replace = TRUE),
    home_points = sample(0:150, n_rows, replace = TRUE),
    away_points = sample(0:150, n_rows, replace = TRUE),
    x = runif(n_rows, -80, 80),
    y = runif(n_rows, -60, 60),
    venue_length = rep(c(160, 165, 170), length.out = n_rows),
    throw_in = sample(0:1, n_rows, replace = TRUE, prob = c(0.9, 0.1)),
    description = sample(c("Kick", "Handball", "Mark", "Free For", "Centre Bounce"), 
                        n_rows, replace = TRUE),
    player_name = paste0("Player_", sample(1:500, n_rows, replace = TRUE)),
    play_type = sample(c("handball", "kick", "reception"), n_rows, replace = TRUE),
    phase_of_play = sample(c("handball_received", "hard_ball", "loose_ball", "set_shot"), 
                          n_rows, replace = TRUE),
    shot_at_goal = sample(c(TRUE, FALSE), n_rows, replace = TRUE, prob = c(0.1, 0.9)),
    points_shot = ifelse(sample(c(TRUE, FALSE), n_rows, replace = TRUE, prob = c(0.6, 0.4)), 6, 1),
    disposal = sample(c("effective", "ineffective", "clanger"), n_rows, replace = TRUE),
    player_id = sample(1:800, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

create_mock_shot_data <- function(n_rows = 50) {
  data.frame(
    goal_x = runif(n_rows, 5, 50),
    y = runif(n_rows, -30, 30),
    shot_at_goal = TRUE,
    points_shot = sample(c(1, 6), n_rows, replace = TRUE, prob = c(0.3, 0.7)),
    disposal = sample(c("effective", "ineffective", "clanger"), n_rows, replace = TRUE),
    play_type = sample(c("kick", "handball"), n_rows, replace = TRUE),
    phase_of_play = sample(c("set_shot", "general_play"), n_rows, replace = TRUE),
    player_id = sample(1:100, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

create_mock_team_data <- function() {
  data.frame(
    team_id = 1:18,
    team_name = c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood", 
                  "Essendon", "Fremantle", "Geelong Cats", "Gold Coast Suns",
                  "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne",
                  "Port Adelaide", "Richmond", "St Kilda", "Sydney Swans",
                  "West Coast Eagles", "Western Bulldogs"),
    stringsAsFactors = FALSE
  )
}

create_mock_player_data <- function(n_players = 100) {
  data.frame(
    player_id = 1:n_players,
    providerId = 1:n_players,
    player_name = paste0("Player_", 1:n_players),
    team = sample(c("Adelaide Crows", "Brisbane Lions", "Carlton"), n_players, replace = TRUE),
    position = sample(c("Forward", "Midfielder", "Defender", "Ruck"), n_players, replace = TRUE),
    season = rep(2024, n_players),
    dateOfBirth = as.Date("1995-01-01") + sample(1:3650, n_players, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Mock HTTP responses for testing
mock_successful_rds_response <- function() {
  # This would return a serialized R object
  serialize(data.frame(test = 1), NULL)
}

# Safe test environment setup
skip_if_no_internet <- function() {
  testthat::skip_if_not(torp:::check_internet_connection(), "No internet connection")
}

skip_if_no_github_access <- function() {
  result <- try({
    httr::GET("https://api.github.com/repos/peteowen1/torpdata", 
              httr::timeout(5))
  }, silent = TRUE)
  testthat::skip_if(inherits(result, "try-error"), "Cannot access GitHub")
}

# Test that seasons are reasonable
expect_reasonable_season <- function(season) {
  testthat::expect_type(season, "double")
  testthat::expect_gte(season, 2021)
  testthat::expect_lte(season, as.numeric(format(Sys.Date(), "%Y")) + 1)
}

# Test that rounds are reasonable
expect_reasonable_round <- function(round) {
  testthat::expect_type(round, "double")
  testthat::expect_gte(round, 0)
  testthat::expect_lte(round, 28)
}