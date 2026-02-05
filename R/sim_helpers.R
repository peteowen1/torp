#' Simulate a season of games
#'
#' This function simulates a season of games based on team ratings and fixture data.
#'
#' @param sim_teams A data frame containing team ratings.
#' @param sim_games A data frame containing fixture data.
#' @return A data frame of simulated game results.
#' @importFrom data.table as.data.table setkey rbindlist fifelse fcase
#' @importFrom stats rnorm
#' @export
simulate_season <- function(sim_teams, sim_games) {
  # Convert to data.table for performance
  sim_teams_dt <- data.table::as.data.table(sim_teams)
  sim_games_dt <- data.table::as.data.table(sim_games)

  # Ensure columns have correct types (handle case where they start as logical NA)
  if ("result" %in% names(sim_games_dt)) {
    sim_games_dt[, result := as.integer(result)]
  }
  if ("torp_home_round" %in% names(sim_games_dt)) {
    sim_games_dt[, torp_home_round := as.numeric(torp_home_round)]
  }
  if ("torp_away_round" %in% names(sim_games_dt)) {
    sim_games_dt[, torp_away_round := as.numeric(torp_away_round)]
  }

  # Identify rounds that still need simulation (i.e. missing results)
  rounds_to_sim <- sort(unique(sim_games_dt[is.na(result), roundnum]))

  # Pre-allocate a list to store simulated game data for each round

  simmed_games_list <- vector("list", length(rounds_to_sim))

  # Loop over each round and simulate games
  for (i in seq_along(rounds_to_sim)) {
    round_num <- rounds_to_sim[i]
    # Process games for this round
    result <- process_games_dt(sim_teams_dt, sim_games_dt, round_num)

    # Update the team data frame for the next round
    sim_teams_dt <- result$sim_teams

    # Store the results for the current round
    simmed_games_list[[i]] <- result$sim_games
  }

  # Combine the list of rounds into a single data frame of simulated games
  simmed_games <- data.table::rbindlist(simmed_games_list)
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
process_games <- function(sim_teams, sim_games, round_num) {
  # Convert to data.table if needed
  sim_teams_dt <- data.table::as.data.table(sim_teams)
  sim_games_dt <- data.table::as.data.table(sim_games)

  # Call optimized version
  result <- process_games_dt(sim_teams_dt, sim_games_dt, round_num)

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
process_games_dt <- function(sim_teams, sim_games, round_num) {
  # Work on copies to avoid modifying originals unexpectedly
  sim_teams <- data.table::copy(sim_teams)
  sim_games <- data.table::copy(sim_games)

  sim_teams[, roundnum := round_num]

  # Create ratings lookup table with key for fast joins
  sim_ratings <- sim_teams[, .(roundnum, team, torp)]
  data.table::setkey(sim_ratings, roundnum, team)

  # Filter games that need simulation
  sim_games <- sim_games[is.na(result)]

  # Join away team ratings using keyed join
  data.table::setkey(sim_games, roundnum, away_team)
  sim_games[sim_ratings, away_torp := i.torp, on = .(roundnum, away_team = team)]


  # Join home team ratings using keyed join
  data.table::setkey(sim_games, roundnum, home_team)
  sim_games[sim_ratings, home_torp := i.torp, on = .(roundnum, home_team = team)]

  # Calculate estimates and simulate results
  n_games <- nrow(sim_games)
  sim_games[, `:=`(
    estimate = SIM_HOME_ADVANTAGE + (home_torp - away_torp),
    wp = 1 / (10^(-(SIM_HOME_ADVANTAGE + (home_torp - away_torp)) / SIM_WP_SCALING_FACTOR) + 1)
  )]

  # Simulate results for this round
  sim_games[roundnum == round_num & is.na(result), result := as.integer(
    round(stats::rnorm(.N, estimate, SIM_NOISE_SD + (abs(estimate) / 3)))
  )]

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
  # Create lookup for torp_shift by away team
  away_shift <- sim_games[roundnum == round_num, .(roundnum, team = away_team, torp_shift_away = torp_shift)]
  data.table::setkey(away_shift, roundnum, team)

  # Create lookup for torp_shift by home team
  home_shift <- sim_games[roundnum == round_num, .(roundnum, team = home_team, torp_shift_home = torp_shift)]
  data.table::setkey(home_shift, roundnum, team)

  # Apply away team adjustments
  data.table::setkey(sim_teams, roundnum, team)
  sim_teams[away_shift, torp := torp - data.table::fifelse(is.na(i.torp_shift_away), 0, i.torp_shift_away)]

  # Apply home team adjustments
  sim_teams[home_shift, torp := torp - data.table::fifelse(is.na(i.torp_shift_home), 0, i.torp_shift_home)]

  # Update torp_home_round and torp_away_round
  sim_games[roundnum == round_num, `:=`(
    torp_home_round = home_torp,
    torp_away_round = away_torp
  )]

  # Remove temporary columns
  sim_games[, c("torp_shift", "home_torp", "away_torp") := NULL]

  # Return only games from the current round (not all NA games)
  sim_games_round <- sim_games[roundnum == round_num]

  return(list(sim_teams = sim_teams, sim_games = sim_games_round))
}

#' Process games for a single round (legacy dplyr version)
#'
#' Original dplyr-based implementation for backwards compatibility testing.
#'
#' @param sim_teams A data frame containing team ratings.
#' @param sim_games A data frame containing fixture data.
#' @param round_num The round number to process.
#' @return A list containing updated sim_teams and sim_games data frames.
#' @importFrom dplyr filter mutate select inner_join left_join case_when
#' @importFrom stats rnorm
#' @keywords internal
process_games_dplyr <- function(sim_teams, sim_games, round_num) {
  sim_teams$roundnum <- round_num

  sim_ratings <- sim_teams %>%
    dplyr::select("roundnum", "team", "torp")

  sim_games <- sim_games %>%
    dplyr::filter(is.na(.data$result)) %>%
    dplyr::inner_join(sim_ratings, by = c("roundnum" = "roundnum", "away_team" = "team")) %>%
    dplyr::rename(away_torp = "torp") %>%
    dplyr::inner_join(sim_ratings, by = c("roundnum" = "roundnum", "home_team" = "team")) %>%
    dplyr::rename(home_torp = "torp") %>%
    dplyr::mutate(
      estimate = SIM_HOME_ADVANTAGE + (.data$home_torp - .data$away_torp),
      wp = 1 / (10^(-.data$estimate / SIM_WP_SCALING_FACTOR) + 1),
      result = dplyr::case_when(
        is.na(.data$result) & .data$roundnum == round_num ~
          as.integer(round(stats::rnorm(dplyr::n(), .data$estimate, SIM_NOISE_SD + (abs(.data$estimate) / 3)))),
        TRUE ~ as.integer(.data$result)
      ),
      outcome = dplyr::case_when(
        is.na(.data$result) ~ NA_real_,
        .data$result > 0 ~ 1,
        .data$result < 0 ~ 0,
        TRUE ~ 0.5
      ),
      torp_shift = 0.1 * (.data$result - .data$estimate)
    )

  sim_teams <- sim_teams %>%
    dplyr::left_join(
      sim_games %>%
        dplyr::filter(.data$roundnum == round_num) %>%
        dplyr::select("roundnum", "away_team", "torp_shift"),
      by = c("roundnum" = "roundnum", "team" = "away_team")
    ) %>%
    dplyr::mutate(torp = .data$torp - ifelse(!is.na(.data$torp_shift), .data$torp_shift, 0)) %>%
    dplyr::select(-"torp_shift") %>%
    dplyr::left_join(
      sim_games %>%
        dplyr::filter(.data$roundnum == round_num) %>%
        dplyr::select("roundnum", "home_team", "torp_shift"),
      by = c("roundnum" = "roundnum", "team" = "home_team")
    ) %>%
    dplyr::mutate(torp = torp - dplyr::coalesce(torp_shift, 0)) %>%
    dplyr::select(-"torp_shift")

  sim_games <- sim_games %>%
    dplyr::mutate(
      torp_home_round = dplyr::case_when(
        .data$roundnum == round_num ~ .data$home_torp,
        TRUE ~ .data$torp_home_round
      ),
      torp_away_round = dplyr::case_when(
        .data$roundnum == round_num ~ .data$away_torp,
        TRUE ~ .data$torp_away_round
      )
    ) %>%
    dplyr::select(-"torp_shift", -"home_torp", -"away_torp")
  return(list(sim_teams = sim_teams, sim_games = sim_games))
}

#' @rdname simulate_season
#' @description `sim_season()` is deprecated; use `simulate_season()` instead.
#' @export
sim_season <- function(sim_teams, sim_games) {
  .Deprecated("simulate_season")
  simulate_season(sim_teams = sim_teams, sim_games = sim_games)
}
