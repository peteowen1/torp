# Test helper functions and fixtures
# This file is sourced before running tests

# -----------------------------------------------------------------------------
# Mock Data Creation Functions
# -----------------------------------------------------------------------------

#' Create mock play-by-play data for testing
#' @param n_rows Number of rows to generate
#' @return A data frame with mock PBP data
create_mock_pbp_data <- function(n_rows = 100) {
  data.frame(
    match_id = rep(paste0("CD_M2024014", sprintf("%02d", 1:5)), length.out = n_rows),
    period = sample(1:4, n_rows, replace = TRUE),
    period_seconds = sample(1:1800, n_rows, replace = TRUE),
    total_seconds = sample(1:7200, n_rows, replace = TRUE),
    est_match_elapsed = sample(0:4800, n_rows, replace = TRUE),
    est_match_remaining = sample(0:4800, n_rows, replace = TRUE),
    team_id = sample(1:18, n_rows, replace = TRUE),
    home_team_id = sample(1:18, n_rows, replace = TRUE),
    away_team_id = sample(1:18, n_rows, replace = TRUE),
    team = sample(c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood"), n_rows, replace = TRUE),
    utc_start_time = as.Date("2024-04-01") + sample(0:100, n_rows, replace = TRUE),
    round_number = sample(1:24, n_rows, replace = TRUE),
    home_points = sample(0:150, n_rows, replace = TRUE),
    away_points = sample(0:150, n_rows, replace = TRUE),
    shot_row = sample(0:1, n_rows, replace = TRUE, prob = c(0.95, 0.05)), # Add shot_row
    home = sample(0:1, n_rows, replace = TRUE), # Add home
    points_diff = sample(-50:50, n_rows, replace = TRUE), # Add points_diff
    xpoints_diff = runif(n_rows, -10, 10), # Add xpoints_diff
    pos_lead_prob = runif(n_rows, 0, 1), # Add pos_lead_prob
    time_left_scaler = runif(n_rows, 0.5, 4), # Add time_left_scaler
    diff_time_ratio = runif(n_rows, -100, 100), # Add diff_time_ratio
    x = runif(n_rows, -80, 80),
    y = runif(n_rows, -60, 60),
    goal_x = runif(n_rows, 10, 80), # Add goal_x for EPV
    lag_goal_x = runif(n_rows, 10, 80), # Add lag_goal_x for EPV
    lag_goal_x5 = runif(n_rows, 10, 80), # Add lag_goal_x5 for EPV
    lag_y = runif(n_rows, -60, 60), # Add lag_y for EPV
    speed5 = runif(n_rows, -5, 10), # Add speed5 for EPV
    exp_pts = runif(n_rows, -2, 2), # Add exp_pts for enhanced predictions
    team_id_mdl = sample(1:18, n_rows, replace = TRUE), # Add team_id_mdl for enhanced predictions
    venue_length = rep(c(160, 165, 170), length.out = n_rows),
    throw_in = sample(0:1, n_rows, replace = TRUE, prob = c(0.9, 0.1)),
    description = sample(c("Kick", "Handball", "Mark", "Free For", "Centre Bounce"), 
                        n_rows, replace = TRUE),
    lag_desc = sample(c("Kick", "Handball", "Mark", "Free For", "Centre Bounce"), 
                     n_rows, replace = TRUE),
    lead_desc = sample(c("Kick", "Handball", "Mark", "Free For", "Centre Bounce", "Out of Bounds"), 
                      n_rows, replace = TRUE),
    player_name = paste0("Player_", sample(1:500, n_rows, replace = TRUE)),
    player_name_given_name = paste0("First", sample(1:500, n_rows, replace = TRUE)),
    player_name_surname = paste0("Last", sample(1:500, n_rows, replace = TRUE)),
    play_type = sample(c("handball", "kick", "reception"), n_rows, replace = TRUE),
    phase_of_play = sample(c("handball_received", "hard_ball", "loose_ball", "set_shot"), 
                          n_rows, replace = TRUE),
    # Add dummy variables for play_type
    play_type_handball = ifelse(sample(c("handball", "kick", "reception"), n_rows, replace = TRUE) == "handball", 1, 0),
    play_type_kick = ifelse(sample(c("handball", "kick", "reception"), n_rows, replace = TRUE) == "kick", 1, 0),
    play_type_reception = ifelse(sample(c("handball", "kick", "reception"), n_rows, replace = TRUE) == "reception", 1, 0),
    # Add dummy variables for phase_of_play
    phase_of_play_handball_received = ifelse(sample(c("handball_received", "hard_ball", "loose_ball", "set_shot"), n_rows, replace = TRUE) == "handball_received", 1, 0),
    phase_of_play_hard_ball = ifelse(sample(c("handball_received", "hard_ball", "loose_ball", "set_shot"), n_rows, replace = TRUE) == "hard_ball", 1, 0),
    phase_of_play_loose_ball = ifelse(sample(c("handball_received", "hard_ball", "loose_ball", "set_shot"), n_rows, replace = TRUE) == "loose_ball", 1, 0),
    phase_of_play_set_shot = ifelse(sample(c("handball_received", "hard_ball", "loose_ball", "set_shot"), n_rows, replace = TRUE) == "set_shot", 1, 0),
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
  has_internet <- tryCatch({
    con <- url("https://www.google.com", open = "r")
    close(con)
    TRUE
  }, error = function(e) FALSE)
  testthat::skip_if_not(has_internet, "No internet connection")
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

# -----------------------------------------------------------------------------
# Enhanced Test Data Creators
# -----------------------------------------------------------------------------

#' Create test model data suitable for WP/EP models
#' @param n_rows Number of rows
#' @return Data frame with model-ready features
create_test_model_data <- function(n_rows = 100) {
  data.frame(
    match_id = rep(paste0("CD_M2024014", sprintf("%02d", 1:5)), length.out = n_rows),
    period = sample(1:4, n_rows, replace = TRUE),
    period_seconds = sample(0:2000, n_rows, replace = TRUE),
    total_seconds = sample(0:8000, n_rows, replace = TRUE),
    est_match_elapsed = sample(0:4800, n_rows, replace = TRUE),
    est_match_remaining = sample(0:4800, n_rows, replace = TRUE),
    points_diff = sample(-100:100, n_rows, replace = TRUE),
    xpoints_diff = runif(n_rows, -10, 10),
    exp_pts = runif(n_rows, -3, 3),
    goal_x = runif(n_rows, 5, 100),
    y = runif(n_rows, -40, 40),
    home = sample(0:1, n_rows, replace = TRUE),
    shot_row = sample(0:1, n_rows, replace = TRUE, prob = c(0.9, 0.1)),
    label_wp = sample(0:1, n_rows, replace = TRUE),
    time_remaining_pct = runif(n_rows, 0, 1),
    stringsAsFactors = FALSE
  )
}

#' Create test player game data
#' @param n_rows Number of rows
#' @return Data frame with player game stats
create_test_player_game_data <- function(n_rows = 200) {
  data.frame(
    match_id = rep(paste0("CD_M2024014", sprintf("%02d", 1:10)), length.out = n_rows),
    player_id = sample(1:100, n_rows, replace = TRUE),
    player_given_name = paste0("First", sample(1:100, n_rows, replace = TRUE)),
    player_surname = paste0("Last", sample(1:100, n_rows, replace = TRUE)),
    team_id = sample(1:18, n_rows, replace = TRUE),
    season = rep(2024, n_rows),
    round = sample(1:24, n_rows, replace = TRUE),
    utc_start_time = as.Date("2024-04-01") + sample(0:100, n_rows, replace = TRUE),
    position_group = sample(c("FWD", "MID", "DEF", "RUC"), n_rows, replace = TRUE),
    team = sample(c("Adelaide Crows", "Brisbane Lions", "Carlton"), n_rows, replace = TRUE),
    opponent = sample(c("Collingwood", "Essendon", "Fremantle"), n_rows, replace = TRUE),
    epv_adj = runif(n_rows, -5, 15),
    recv_epv_adj = runif(n_rows, -2, 8),
    disp_epv_adj = runif(n_rows, -2, 8),
    spoil_epv_adj = runif(n_rows, -1, 3),
    hitout_epv_adj = runif(n_rows, 0, 5),
    stringsAsFactors = FALSE
  )
}

#' Create test simulation data (teams)
#' @param n_teams Number of teams
#' @return Data frame with team ratings for simulation
create_test_sim_teams <- function(n_teams = 18) {
  data.frame(
    team = c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood",
             "Essendon", "Fremantle", "Geelong Cats", "Gold Coast Suns",
             "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne",
             "Port Adelaide", "Richmond", "St Kilda", "Sydney Swans",
             "West Coast Eagles", "Western Bulldogs")[1:n_teams],
    torp = runif(n_teams, -20, 20),
    stringsAsFactors = FALSE
  )
}

#' Create test simulation data (games/fixtures)
#' @param n_rounds Number of rounds to simulate
#' @param teams Vector of team names
#' @return Data frame with fixture data for simulation
create_test_sim_games <- function(n_rounds = 3, teams = NULL) {
  if (is.null(teams)) {
    teams <- c("Adelaide Crows", "Brisbane Lions", "Carlton", "Collingwood",
               "Essendon", "Fremantle", "Geelong Cats", "Gold Coast Suns",
               "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne",
               "Port Adelaide", "Richmond", "St Kilda", "Sydney Swans",
               "West Coast Eagles", "Western Bulldogs")
  }

  games_list <- list()
  for (rnd in 1:n_rounds) {
    shuffled <- sample(teams)
    n_games <- length(shuffled) %/% 2
    for (g in 1:n_games) {
      games_list[[length(games_list) + 1]] <- data.frame(
        roundnum = rnd,
        home_team = shuffled[(g-1)*2 + 1],
        away_team = shuffled[(g-1)*2 + 2],
        result = NA_integer_,
        torp_home_round = NA_real_,
        torp_away_round = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, games_list)
}

#' Create test fixture data
#' @param n_fixtures Number of fixtures
#' @return Data frame resembling AFL fixture data
create_test_fixtures <- function(n_fixtures = 50) {
  data.frame(
    compSeason.year = rep(2024, n_fixtures),
    round.roundNumber = rep(1:25, length.out = n_fixtures),
    utcStartTime = as.POSIXct("2024-03-14") + (0:(n_fixtures-1)) * 86400 * 7,
    home.team.name = sample(c("Adelaide Crows", "Brisbane Lions", "Carlton"), n_fixtures, replace = TRUE),
    away.team.name = sample(c("Collingwood", "Essendon", "Fremantle"), n_fixtures, replace = TRUE),
    venue.name = sample(c("MCG", "Adelaide Oval", "Gabba"), n_fixtures, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Mock Model Prediction Creators
# -----------------------------------------------------------------------------

#' Create mock EP model predictions
#' Used to test EP variable addition without requiring actual model
#' @param n_rows Number of rows to generate
#' @return Data frame with EP prediction columns matching model output
create_mock_ep_predictions <- function(n_rows = 100) {
  # EP model outputs 5 probability columns that should sum to ~1
  opp_goal <- runif(n_rows, 0.05, 0.25)
  opp_behind <- runif(n_rows, 0.05, 0.20)
  behind <- runif(n_rows, 0.05, 0.20)
  goal <- runif(n_rows, 0.15, 0.45)
  no_score <- 1 - (opp_goal + opp_behind + behind + goal)
  # Ensure no_score is valid
  no_score <- pmax(0.05, pmin(0.5, no_score))

  data.frame(
    opp_goal = opp_goal,
    opp_behind = opp_behind,
    behind = behind,
    goal = goal,
    no_score = no_score,
    stringsAsFactors = FALSE
  )
}

#' Create mock WP model predictions
#' Used to test WP variable addition without requiring actual model
#' @param n_rows Number of rows to generate
#' @return Data frame with WP prediction column
create_mock_wp_predictions <- function(n_rows = 100) {
  data.frame(
    wp = runif(n_rows, 0.15, 0.85),
    stringsAsFactors = FALSE
  )
}

#' Create mock shot model predictions
#' Used to test shot variable addition without requiring actual model
#' @param n_rows Number of rows to generate
#' @return Data frame with shot outcome prediction columns
create_mock_shot_predictions <- function(n_rows = 50) {
  # Shot model outputs probabilities for clanger, behind, goal
  clanger_prob <- runif(n_rows, 0.02, 0.25)
  behind_prob <- runif(n_rows, 0.15, 0.40)
  goal_prob <- 1 - (clanger_prob + behind_prob)

  data.frame(
    clanger_prob = clanger_prob,
    behind_prob = behind_prob,
    goal_prob = goal_prob,
    stringsAsFactors = FALSE
  )
}

#' Create minimal valid PBP data for pipeline tests
#' Contains the minimum required columns for clean_pbp() to work
#' @param n_rows Number of rows to generate
#' @return Data frame with minimal valid PBP structure
create_minimal_pbp <- function(n_rows = 20) {
  # Ensure we have sorted data within matches
  match_ids <- rep(paste0("CD_M2024014", sprintf("%02d", 1:2)), each = n_rows / 2)

  data.frame(
    match_id = match_ids,
    period = rep(1:4, length.out = n_rows),
    period_seconds = sort(sample(1:1800, n_rows, replace = TRUE)),
    total_seconds = sort(sample(1:7200, n_rows, replace = TRUE)),
    team_id = sample(1:18, n_rows, replace = TRUE),
    home_team_id = rep(c(1L, 3L), each = n_rows / 2),
    away_team_id = rep(c(2L, 4L), each = n_rows / 2),
    team = sample(c("Carlton", "Collingwood"), n_rows, replace = TRUE),
    utc_start_time = rep(as.POSIXct("2024-04-01 13:00:00"), n_rows),
    round_number = rep(1L, n_rows),
    home_points = cumsum(sample(c(0, 0, 0, 1, 6), n_rows, replace = TRUE)),
    away_points = cumsum(sample(c(0, 0, 0, 1, 6), n_rows, replace = TRUE)),
    x = runif(n_rows, -80, 80),
    y = runif(n_rows, -60, 60),
    description = sample(c("Kick", "Handball", "Mark", "Free For"), n_rows, replace = TRUE),
    player_name = paste0("Player_", sample(1:50, n_rows, replace = TRUE)),
    player_id = sample(1:800, n_rows, replace = TRUE),
    venue_length = rep(160, n_rows),
    stringsAsFactors = FALSE
  )
}

#' Create mock XG data for match XG tests
#' @param n_matches Number of matches
#' @return Data frame with XG data structure
create_mock_xg_data <- function(n_matches = 5) {
  rows_per_match <- 40
  n_rows <- n_matches * rows_per_match

  match_ids <- rep(paste0("CD_M2024014", sprintf("%02d", 1:n_matches)), each = rows_per_match)
  teams <- rep(c(rep("Carlton", rows_per_match/2), rep("Collingwood", rows_per_match/2)), n_matches)

  data.frame(
    match_id = match_ids,
    team = teams,
    period = rep(rep(1:4, each = rows_per_match/4), n_matches),
    xscore = runif(n_rows, 0, 6),
    points_shot = sample(c(0, 1, 6), n_rows, replace = TRUE, prob = c(0.5, 0.2, 0.3)),
    shot_at_goal = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Create mock API response for scraper tests
#' @return List mimicking AFL API response structure
create_mock_api_response <- function() {
  list(
    content = list(
      matches = list(
        list(
          id = "CD_M20240141001",
          homeTeam = list(
            name = "Carlton",
            teamId = 1
          ),
          awayTeam = list(
            name = "Collingwood",
            teamId = 2
          ),
          venue = list(name = "MCG"),
          utcStartTime = "2024-03-14T03:20:00.000Z"
        ),
        list(
          id = "CD_M20240141002",
          homeTeam = list(
            name = "Brisbane Lions",
            teamId = 3
          ),
          awayTeam = list(
            name = "Adelaide Crows",
            teamId = 4
          ),
          venue = list(name = "Gabba"),
          utcStartTime = "2024-03-14T06:45:00.000Z"
        )
      )
    )
  )
}
# -----------------------------------------------------------------------------
# Shared Network Data (loaded once per test session, reused by all test files)
# -----------------------------------------------------------------------------
# Avoids each test file independently downloading the same datasets.
# Access via .shared$pbp, .shared$chains, etc.

.shared <- new.env(parent = emptyenv())
.shared$can_load <- !identical(Sys.getenv("NOT_CRAN"), "") || interactive()

if (.shared$can_load && identical(Sys.getenv("TESTTHAT"), "true") && curl::has_internet()) {
  .shared$pbp <- tryCatch(load_pbp(2024, rounds = 1), error = function(e) NULL)
  .shared$chains <- tryCatch(load_chains(2024, rounds = 1), error = function(e) NULL)
  .shared$fixtures <- tryCatch(load_fixtures(2024), error = function(e) NULL)
  .shared$player_stats <- tryCatch(load_player_stats(2024), error = function(e) NULL)
  .shared$player_details <- tryCatch(load_player_details(2024), error = function(e) NULL)
  .shared$player_game_data <- tryCatch(load_player_game_data(2024), error = function(e) NULL)
  .shared$teams <- tryCatch(load_teams(2024), error = function(e) NULL)
  .shared$match_xgs <- tryCatch(
    calculate_match_xgs(season = 2024, round = 1, quarter = 1:4),
    error = function(e) NULL
  )
} else {
  .shared$pbp <- .shared$chains <- .shared$fixtures <- NULL
  .shared$player_stats <- .shared$player_details <- NULL
  .shared$player_game_data <- .shared$teams <- NULL
  .shared$match_xgs <- NULL
}

# -----------------------------------------------------------------------------
# Mock Data for Plot Testing
# -----------------------------------------------------------------------------

create_mock_ep_wp_data <- function(n_rows = 200) {
  data.frame(
    match_id = rep("CD_M20240140101", n_rows),
    season = 2024L,
    round_number = 1L,
    period = rep(1:4, each = n_rows / 4),
    total_seconds = seq(0, 7999, length.out = n_rows),
    home_team_name = "Adelaide Crows",
    away_team_name = "Brisbane Lions",
    team = sample(c("Adelaide Crows", "Brisbane Lions"), n_rows, replace = TRUE),
    exp_pts = cumsum(rnorm(n_rows, 0, 0.3)),
    wp = plogis(cumsum(rnorm(n_rows, 0, 0.05))),
    delta_epv = rnorm(n_rows, 0, 0.5),
    wpa = rnorm(n_rows, 0, 0.02),
    description = sample(c("Kick", "Handball", "Mark"), n_rows, replace = TRUE),
    player_name = paste0("Player_", sample(1:40, n_rows, replace = TRUE)),
    play_type = sample(c("kick", "handball"), n_rows, replace = TRUE),
    shot_row = sample(c(0, 1), n_rows, replace = TRUE, prob = c(0.95, 0.05)),
    points_shot = sample(c(NA, 1, 6), n_rows, replace = TRUE, prob = c(0.9, 0.05, 0.05)),
    stringsAsFactors = FALSE
  )
}

create_mock_player_game_ratings <- function(n_games = 50) {
  data.frame(
    season = rep(c(2023, 2024), each = n_games / 2),
    round_number = rep(1:(n_games / 2), 2),
    match_id = paste0("CD_M2024014", sprintf("%04d", 1:n_games)),
    player_id = rep("CD_I123456", n_games),
    player_name = rep("Test Player", n_games),
    team = rep("Adelaide Crows", n_games),
    torp_value = rnorm(n_games, 0, 2),
    epv = rnorm(n_games, 0, 1.5),
    psv = rnorm(n_games, 0, 1),
    osv = rnorm(n_games, 0, 0.8),
    dsv = rnorm(n_games, 0, 0.8),
    stringsAsFactors = FALSE
  )
}

create_mock_sim_results <- function(n_sims = 10) {
  teams <- torp::AFL_TEAMS$name
  n_teams <- length(teams)

  ladders <- do.call(rbind, lapply(1:n_sims, function(sim) {
    w <- sample(2:18, n_teams, replace = TRUE)
    data.table::data.table(
      sim_id = sim, team = teams,
      played = 22L, wins = w, losses = 22L - w, draws = 0L,
      points_for = w * 90 + sample(-50:50, n_teams),
      points_against = (22L - w) * 90 + sample(-50:50, n_teams),
      percentage = 80 + w * 4 + rnorm(n_teams, 0, 5),
      ladder_points = w * 4L,
      rank = rank(-w, ties.method = "random")
    )
  }))

  finals <- data.table::data.table(
    sim_id = rep(1:n_sims, each = 8),
    team = rep(teams[1:8], n_sims),
    finals_finish = sample(1:4, n_sims * 8, replace = TRUE),
    finals_wins = sample(0:3, n_sims * 8, replace = TRUE),
    made_gf = sample(c(TRUE, FALSE), n_sims * 8, replace = TRUE, prob = c(0.2, 0.8)),
    won_gf = sample(c(TRUE, FALSE), n_sims * 8, replace = TRUE, prob = c(0.1, 0.9))
  )

  structure(
    list(
      season = 2024, n_sims = n_sims,
      ladders = ladders, finals = finals,
      games = data.table::data.table(),
      played_games = data.table::data.table(),
      original_ratings = NULL
    ),
    class = "torp_sim_results"
  )
}

create_mock_stat_rating_profile <- function() {
  stats <- c("goals", "behinds", "disposals", "kicks", "handballs",
             "marks", "tackles", "hitouts", "inside50s", "clearances")
  categories <- c("scoring", "scoring", "disposal", "disposal", "disposal",
                   "possession", "defensive", "ruck", "territory", "clearance")

  profile <- data.frame(
    stat = stats,
    category = categories,
    type = rep("rate", length(stats)),
    rating = runif(length(stats), 0.5, 3),
    raw_avg = runif(length(stats), 0.3, 2.5),
    league_avg = runif(length(stats), 0.8, 2),
    league_pct = runif(length(stats), 10, 95),
    pos_avg = runif(length(stats), 0.7, 2.2),
    pos_pct = runif(length(stats), 10, 95),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      player_info = data.frame(
        player_id = "CD_I123456", name = "Test Player",
        team = "Adelaide Crows", position = "Forward",
        pos_group = "fwd", stringsAsFactors = FALSE
      ),
      stat_ratings = profile,
      ref_date = as.Date("2024-06-01"),
      n_games = 50, n_80s = 40, wt_80s = 35
    ),
    class = "torp_stat_rating_profile"
  )
}
