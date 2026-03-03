#' Simulate a season of games
#'
#' This function simulates a season of games based on team ratings and fixture data.
#'
#' @param sim_teams A data frame containing team ratings.
#' @param sim_games A data frame containing fixture data.
#' @param return_teams Logical. If TRUE, return a list with both games and updated
#'   team ratings. Default FALSE returns just the games data.table for backward
#'   compatibility.
#' @return A data.table of simulated game results (default), or a list with
#'   `games` and `teams` elements when `return_teams = TRUE`.
#' @importFrom data.table as.data.table setkey rbindlist fifelse fcase
#' @importFrom stats rnorm
#' @export
simulate_season <- function(sim_teams, sim_games, return_teams = FALSE,
                            injury_sd = SIM_INJURY_SD) {
  # Convert to data.table for performance
  sim_teams_dt <- data.table::as.data.table(sim_teams)
  sim_games_dt <- data.table::as.data.table(sim_games)

  # Ensure columns have correct types (handle case where they start as logical NA)
  # Skipped when caller pre-ensures types (e.g. simulate_afl_season)
  if ("result" %in% names(sim_games_dt) && !is.integer(sim_games_dt$result)) {
    sim_games_dt[, result := as.integer(result)]
  }
  if ("torp_home_round" %in% names(sim_games_dt) && !is.numeric(sim_games_dt$torp_home_round)) {
    sim_games_dt[, torp_home_round := as.numeric(torp_home_round)]
  }
  if ("torp_away_round" %in% names(sim_games_dt) && !is.numeric(sim_games_dt$torp_away_round)) {
    sim_games_dt[, torp_away_round := as.numeric(torp_away_round)]
  }

  # Identify rounds that still need simulation (i.e. missing results)
  rounds_to_sim <- sort(unique(sim_games_dt[is.na(result), roundnum]))

  # Pre-split unplayed games by round to avoid redundant filtering
  games_by_round <- split(sim_games_dt[is.na(result)], by = "roundnum")

  # Pre-allocate a list to store simulated game data for each round
  simmed_games_list <- vector("list", length(rounds_to_sim))

  # Loop over each round and simulate games
  for (i in seq_along(rounds_to_sim)) {
    round_num <- rounds_to_sim[i]
    # Process only this round's games (~9 rows instead of ~216)
    result <- process_games_dt(sim_teams_dt,
                               games_by_round[[as.character(round_num)]],
                               round_num,
                               injury_sd = injury_sd)

    # Update the team data frame for the next round
    sim_teams_dt <- result$sim_teams

    # Store the results for the current round
    simmed_games_list[[i]] <- result$sim_games
  }

  # Combine the list of rounds into a single data frame of simulated games
  simmed_games <- data.table::rbindlist(simmed_games_list)

  if (return_teams) {
    return(list(games = simmed_games, teams = sim_teams_dt))
  }
  return(simmed_games)
}


#' Process games for a single round
#'
#' This function processes games for a single round of the season.
#' Uses optimized data.table implementation internally.
#'
#' @param sim_teams A data frame containing team ratings.
#' @param sim_games A data frame containing fixture data.
#' @param round_num The round number to process.
#' @return A list containing updated sim_teams and sim_games data frames.
#' @importFrom data.table as.data.table setkey copy fifelse fcase
#' @importFrom stats rnorm
#' @keywords internal
process_games <- function(sim_teams, sim_games, round_num,
                          injury_sd = SIM_INJURY_SD) {
  # Convert to data.table if needed
  sim_teams_dt <- data.table::as.data.table(sim_teams)
  sim_games_dt <- data.table::as.data.table(sim_games)

  # Filter to current round's unplayed games (process_games_dt expects this)
  round_games <- sim_games_dt[roundnum == round_num & is.na(result)]

  # Call optimized version
  result <- process_games_dt(sim_teams_dt, round_games, round_num,
                             injury_sd = injury_sd)

  # Return as data.frames for backwards compatibility
  return(list(
    sim_teams = as.data.frame(result$sim_teams),
    sim_games = as.data.frame(result$sim_games)
  ))
}

#' Process games for a single round (data.table optimized)
#'
#' Internal data.table implementation for processing games.
#'
#' @param sim_teams A data.table containing team ratings.
#' @param sim_games A data.table containing fixture data.
#' @param round_num The round number to process.
#' @return A list containing updated sim_teams and sim_games data.tables.
#' @importFrom data.table setkey copy fifelse fcase
#' @importFrom stats rnorm
#' @keywords internal
process_games_dt <- function(sim_teams, sim_games, round_num,
                             injury_sd = SIM_INJURY_SD) {
  # Copy teams (modified via rating shifts); sim_games is a round-level slice
  sim_teams <- data.table::copy(sim_teams)
  sim_games <- data.table::copy(sim_games)

  sim_teams[, roundnum := round_num]

  # Create ratings lookup
  sim_ratings <- sim_teams[, .(roundnum, team, torp)]
  data.table::setkey(sim_ratings, roundnum, team)

  # Join team ratings
  sim_games[sim_ratings, away_torp := i.torp, on = .(roundnum, away_team = team)]
  sim_games[sim_ratings, home_torp := i.torp, on = .(roundnum, home_team = team)]

  # Apply per-team injury/disruption noise to effective ratings for this round
  n_games <- nrow(sim_games)
  sim_games[, `:=`(
    home_torp_eff = home_torp + stats::rnorm(n_games, 0, injury_sd),
    away_torp_eff = away_torp + stats::rnorm(n_games, 0, injury_sd)
  )]

  # Calculate estimates using disrupted ratings, simulate results
  sim_games[, `:=`(
    estimate = SIM_HOME_ADVANTAGE + (home_torp_eff - away_torp_eff),
    wp = 1 / (10^(-(SIM_HOME_ADVANTAGE + (home_torp_eff - away_torp_eff)) / SIM_WP_SCALING_FACTOR) + 1)
  )]

  sim_games[is.na(result), result := as.integer(
    round(stats::rnorm(.N, estimate, SIM_NOISE_SD + (abs(estimate) / 3)))
  )]

  # Generate individual scores from margin + total
  sim_games[!is.na(result), c("home_score", "away_score") := {
    base_total <- if ("pred_xtotal" %in% names(sim_games)) {
      data.table::fifelse(is.na(pred_xtotal), SIM_AVG_TOTAL, pred_xtotal)
    } else {
      SIM_AVG_TOTAL
    }
    total <- pmax(stats::rnorm(.N, base_total, SIM_TOTAL_SD), 40)
    hs <- as.integer(pmax(round((total + result) / 2), 0))
    as_val <- as.integer(pmax(round((total - result) / 2), 0))
    list(hs, as_val)
  }]

  sim_games[, `:=`(
    outcome = data.table::fcase(
      is.na(result), NA_real_,
      result > 0, 1,
      result < 0, 0,
      default = 0.5
    ),
    torp_shift = 0.1 * (result - estimate)
  )]

  # Update team ratings based on results
  away_shift <- sim_games[, .(roundnum, team = away_team, torp_shift_away = torp_shift)]
  data.table::setkey(away_shift, roundnum, team)

  home_shift <- sim_games[, .(roundnum, team = home_team, torp_shift_home = torp_shift)]
  data.table::setkey(home_shift, roundnum, team)

  data.table::setkey(sim_teams, roundnum, team)
  sim_teams[away_shift, torp := torp - data.table::fifelse(is.na(i.torp_shift_away), 0, i.torp_shift_away)]
  sim_teams[home_shift, torp := torp - data.table::fifelse(is.na(i.torp_shift_home), 0, i.torp_shift_home)]

  # Mean reversion: pull ratings toward league average each round
  league_mean <- mean(sim_teams$torp)
  sim_teams[, torp := torp + SIM_MEAN_REVERSION * (league_mean - torp)]

  sim_games[, `:=`(
    torp_home_round = home_torp,
    torp_away_round = away_torp
  )]

  sim_games[, c("torp_shift", "home_torp", "away_torp",
                "home_torp_eff", "away_torp_eff") := NULL]

  return(list(sim_teams = sim_teams, sim_games = sim_games))
}

#' @rdname simulate_season
#' @description `sim_season()` is deprecated; use `simulate_season()` instead.
#' @export
sim_season <- function(sim_teams, sim_games) {
  .Deprecated("simulate_season", package = "torp", old = "sim_season")
  simulate_season(sim_teams = sim_teams, sim_games = sim_games)
}
