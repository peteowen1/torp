#' Simulate a season of games
#'
#' This function simulates a season of games based on team ratings and fixture data.
#'
#' @param sim_teams A data frame containing team ratings.
#' @param sim_games A data frame containing fixture data.
#' @param return_teams Logical. If TRUE, return a list with both games and updated
#'   team ratings. Default FALSE returns just the games data.table for backward
#'   compatibility.
#' @param injury_sd Standard deviation for injury impact on team ratings. Default is \code{SIM_INJURY_SD}.
#' @param injury_schedule Optional data.table from [build_injury_schedule()] with
#'   columns `team`, `torp_boost`, `return_round`. When provided, returning
#'   players' TORP contributions are added back at the appropriate round.
#' @return A data.table of simulated game results (default), or a list with
#'   `games` and `teams` elements when `return_teams = TRUE`.
#' @importFrom data.table as.data.table setkey rbindlist fifelse fcase
#' @importFrom stats rnorm
#' @export
simulate_season <- function(sim_teams, sim_games, return_teams = FALSE,
                            injury_sd = SIM_INJURY_SD,
                            injury_schedule = NULL) {
  sim_teams_dt <- data.table::as.data.table(sim_teams)
  sim_games_dt <- data.table::as.data.table(sim_games)

  # Draw team quality residuals once per simulation from N(mean, se)
  # These represent epistemic uncertainty in team quality beyond player TORP
  has_residuals <- "residual_mean" %in% names(sim_teams_dt) &&
    any(sim_teams_dt$residual_se > 0, na.rm = TRUE)
  if (has_residuals) {
    sim_teams_dt[, residual := stats::rnorm(.N, residual_mean, residual_se)]
  } else if (!"residual" %in% names(sim_teams_dt)) {
    sim_teams_dt[, residual := 0]
  }

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
  unplayed <- sim_games_dt[is.na(result)]
  rounds_to_sim <- sort(unique(unplayed$roundnum))

  # Pre-split unplayed games by round to avoid redundant filtering
  games_by_round <- split(unplayed, by = "roundnum")

  # Pre-allocate a list to store simulated game data for each round
  simmed_games_list <- vector("list", length(rounds_to_sim))

  has_xtotal <- "pred_xtotal" %in% names(sim_games_dt)

  # Pre-index injury schedule by round for O(1) lookup
  inj_by_round <- if (!is.null(injury_schedule) && nrow(injury_schedule) > 0) {
    split(injury_schedule, by = "return_round")
  } else {
    NULL
  }

  # Build team index for direct vector lookups (avoids data.table join overhead)
  team_idx <- stats::setNames(seq_len(nrow(sim_teams_dt)), sim_teams_dt$team)
  torp_vec <- sim_teams_dt$torp
  resid_vec <- sim_teams_dt$residual

  # Loop over each round and simulate games
  for (i in seq_along(rounds_to_sim)) {
    round_num <- rounds_to_sim[i]

    # Add back TORP contributions of players returning from injury this round
    if (!is.null(inj_by_round)) {
      returning <- inj_by_round[[as.character(round_num)]]
      if (!is.null(returning) && nrow(returning) > 0) {
        ret_idx <- team_idx[returning$team]
        torp_vec[ret_idx] <- torp_vec[ret_idx] + returning$torp_boost
      }
    }

    # Process this round's games using direct vector lookups
    sim_games_r <- games_by_round[[as.character(round_num)]]
    n_games <- nrow(sim_games_r)

    home_idx <- team_idx[sim_games_r$home_team]
    away_idx <- team_idx[sim_games_r$away_team]

    home_torp <- torp_vec[home_idx]
    away_torp <- torp_vec[away_idx]

    # Compute estimate: TORP diff + residuals + noise + home advantage
    estimate <- SIM_HOME_ADVANTAGE +
      (home_torp - away_torp) +
      (resid_vec[home_idx] - resid_vec[away_idx]) +
      # Independent per-team noise; effective margin SD = injury_sd * sqrt(2)
      stats::rnorm(n_games, 0, injury_sd) - stats::rnorm(n_games, 0, injury_sd)

    data.table::set(sim_games_r, j = "estimate", value = estimate)
    data.table::set(sim_games_r, j = "wp",
                    value = 1 / (10^(-estimate / SIM_WP_SCALING_FACTOR) + 1))

    # Simulate result for unplayed games
    na_mask <- is.na(sim_games_r$result)
    if (any(na_mask)) {
      na_idx <- which(na_mask)
      new_result <- as.integer(round(
        stats::rnorm(length(na_idx), estimate[na_idx],
                     SIM_NOISE_SD + abs(estimate[na_idx]) / 3)
      ))
      data.table::set(sim_games_r, i = na_idx, j = "result", value = new_result)
    }

    # Generate individual scores from margin + total
    result_vec <- sim_games_r$result
    base_total <- if (has_xtotal) {
      xt <- sim_games_r$pred_xtotal
      ifelse(is.na(xt), SIM_AVG_TOTAL, xt)
    } else {
      rep(SIM_AVG_TOTAL, n_games)
    }
    total <- pmax(stats::rnorm(n_games, base_total, SIM_TOTAL_SD), SIM_MIN_TOTAL)
    data.table::set(sim_games_r, j = "home_score",
                    value = as.integer(pmax(round((total + result_vec) / 2), 0)))
    data.table::set(sim_games_r, j = "away_score",
                    value = as.integer(pmax(round((total - result_vec) / 2), 0)))

    # Update team ratings via direct vector assignment
    shifts <- SIM_RATING_SHIFT * (result_vec - estimate)
    torp_vec[home_idx] <- torp_vec[home_idx] - shifts
    torp_vec[away_idx] <- torp_vec[away_idx] + shifts

    # Mean reversion: pull ratings toward league average each round
    league_mean <- mean(torp_vec)
    torp_vec <- torp_vec + SIM_MEAN_REVERSION * (league_mean - torp_vec)

    # Store round metadata
    data.table::set(sim_games_r, j = "torp_home_round", value = home_torp)
    data.table::set(sim_games_r, j = "torp_away_round", value = away_torp)
    data.table::set(sim_games_r, j = "outcome",
                    value = data.table::fifelse(
                      result_vec > 0L, 1, data.table::fifelse(
                        result_vec < 0L, 0, 0.5)))

    simmed_games_list[[i]] <- sim_games_r
  }

  # Combine the list of rounds into a single data frame of simulated games
  simmed_games <- data.table::rbindlist(simmed_games_list)

  if (return_teams) {
    # Write final torp back to data.table for finals sim
    sim_teams_dt[, torp := torp_vec]
    return(list(games = simmed_games, teams = sim_teams_dt))
  }
  return(simmed_games)
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
  # Copy teams (modified via rating shifts); sim_games is a pre-split slice
  sim_teams <- data.table::copy(sim_teams)

  sim_teams[, roundnum := round_num]
  sim_games[, roundnum := round_num]

  # Join team ratings + residuals in 2 joins (home & away) instead of 4
  has_residual <- "residual" %in% names(sim_teams)
  if (has_residual) {
    sim_games[sim_teams, c("home_torp", "home_residual") := .(i.torp, i.residual),
              on = .(roundnum, home_team = team)]
    sim_games[sim_teams, c("away_torp", "away_residual") := .(i.torp, i.residual),
              on = .(roundnum, away_team = team)]
  } else {
    sim_games[sim_teams, home_torp := i.torp, on = .(roundnum, home_team = team)]
    sim_games[sim_teams, away_torp := i.torp, on = .(roundnum, away_team = team)]
    sim_games[, `:=`(home_residual = 0, away_residual = 0)]
  }

  # Compute estimate in one pass: TORP diff + noise + home advantage + residuals
  n_games <- nrow(sim_games)
  sim_games[, estimate := SIM_HOME_ADVANTAGE +
    (home_torp - away_torp) +
    (home_residual - away_residual) +
    stats::rnorm(n_games, 0, injury_sd) - stats::rnorm(n_games, 0, injury_sd)]
  sim_games[, wp := 1 / (10^(-estimate / SIM_WP_SCALING_FACTOR) + 1)]

  sim_games[is.na(result), result := as.integer(
    round(stats::rnorm(.N, estimate, SIM_NOISE_SD + (abs(estimate) / 3)))
  )]

  # Generate individual scores from margin + total
  has_xtotal <- "pred_xtotal" %in% names(sim_games)
  sim_games[!is.na(result), c("home_score", "away_score") := {
    base_total <- if (has_xtotal) {
      data.table::fifelse(is.na(pred_xtotal), SIM_AVG_TOTAL, pred_xtotal)
    } else {
      SIM_AVG_TOTAL
    }
    total <- pmax(stats::rnorm(.N, base_total, SIM_TOTAL_SD), 40)
    hs <- as.integer(pmax(round((total + result) / 2), 0))
    as_val <- as.integer(pmax(round((total - result) / 2), 0))
    list(hs, as_val)
  }]

  # Compute torp_shift and outcome
  sim_games[, torp_shift := SIM_RATING_SHIFT * (result - estimate)]
  sim_games[, outcome := data.table::fcase(
    is.na(result), NA_real_,
    result > 0, 1,
    result < 0, 0,
    default = 0.5
  )]

  # Update team ratings: direct vector assignment avoids join overhead
  home_idx <- match(sim_games$home_team, sim_teams$team)
  away_idx <- match(sim_games$away_team, sim_teams$team)
  shifts <- sim_games$torp_shift
  data.table::set(sim_teams, i = home_idx, j = "torp",
                  value = sim_teams$torp[home_idx] - shifts)
  data.table::set(sim_teams, i = away_idx, j = "torp",
                  value = sim_teams$torp[away_idx] + shifts)

  # Mean reversion: pull ratings toward league average each round
  league_mean <- mean(sim_teams$torp)
  sim_teams[, torp := torp + SIM_MEAN_REVERSION * (league_mean - torp)]

  sim_games[, `:=`(torp_home_round = home_torp, torp_away_round = away_torp)]
  sim_games[, c("torp_shift", "home_torp", "away_torp",
                "home_residual", "away_residual") := NULL]

  list(sim_teams = sim_teams, sim_games = sim_games)
}
