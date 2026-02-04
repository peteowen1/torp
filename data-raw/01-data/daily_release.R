# Daily Data Release Script for TORP Package
#
# This script is optimized for daily automated runs via GitHub Actions.
# Key features:
# - Only updates current season data (skips historical)
# - Detects if new games have been played before processing
# - Creates/updates aggregated seasonal files for faster bulk loads
# - Modular functions for each data type
# - Comprehensive logging and error handling
#
# Usage:
#   Rscript data-raw/01-data/daily_release.R
#   Or: source("data-raw/01-data/daily_release.R"); run_daily_release()

library(dplyr)
library(glue)
library(cli)
library(piggyback)

# Load the torp package
devtools::load_all()

# =============================================================================
# Configuration
# =============================================================================

#' Get TORP data repository name
get_torpdata_repo <- function() {
 "peteowen1/torpdata"
}

# =============================================================================
# Helper Functions
# =============================================================================

#' Check if New Games Have Been Played
#'
#' Compares current fixtures against the last completed round data.
#' Returns TRUE if there are new completed games since the last release.
#'
#' @return Logical indicating if new games are available
has_new_games <- function() {
  tryCatch({
    current_season <- get_afl_season()
    current_round <- get_afl_week()

    # Get fixtures for current season
    fixtures <- fitzRoy::fetch_fixture_afl(current_season, comp = "AFLM")

    if (nrow(fixtures) == 0) {
      cli::cli_inform("No fixtures found for season {current_season}")
      return(FALSE)
    }

    # Check if any games have been completed in the current round
    time_aest <- lubridate::with_tz(Sys.time(), tzone = "Australia/Brisbane")
    completed_games <- fixtures %>%
      dplyr::filter(
        .data$round.roundNumber == current_round,
        .data$utcStartTime < time_aest
      )

    if (nrow(completed_games) > 0) {
      cli::cli_inform("Found {nrow(completed_games)} completed game(s) in round {current_round}")
      return(TRUE)
    }

    cli::cli_inform("No completed games found in round {current_round}")
    return(FALSE)
  }, error = function(e) {
    cli::cli_warn("Error checking for new games: {conditionMessage(e)}")
    # Default to TRUE to ensure data gets updated if check fails
    return(TRUE)
  })
}

#' Get Maximum Round for a Season
#'
#' Returns the maximum round number to process for a given season.
#'
#' @param season Season year
#' @return Integer round number
get_max_round <- function(season) {
  current_season <- get_afl_season()
  if (season == current_season) {
    return(get_afl_week())
  }
  # Historical seasons have up to round 28 (including finals)
  return(28)
}

#' Get Starting Round for a Season
#'
#' Returns the starting round number for a season (0 for 2024+, 1 for earlier).
#'
#' @param season Season year
#' @return Integer round number
get_start_round <- function(season) {
  if (season >= 2024) {
    return(0)
  }
  return(1)
}

# =============================================================================
# Per-Round Data Functions
# =============================================================================

#' Update Chains Data for a Round
#'
#' Fetches and uploads chains data for a specific season and round.
#'
#' @param season Season year
#' @param round Round number
#' @return Invisible NULL
update_round_chains <- function(season, round) {
  cli::cli_progress_step("Fetching chains for {season} round {round}")

  chains <- tryCatch({
    get_week_chains(season, round)
  }, error = function(e) {
    cli::cli_warn("Failed to fetch chains for {season} R{round}: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(chains) || nrow(chains) == 0) {
    cli::cli_inform("No chains data for {season} round {round}")
    return(invisible(NULL))
  }

  round_02d <- sprintf("%02d", round)
  file_name <- glue::glue("chains_data_{season}_{round_02d}")
  save_to_release(df = chains, file_name = file_name, release_tag = "chains-data")

  cli::cli_inform("Saved chains: {file_name} ({nrow(chains)} rows)")
  invisible(NULL)
}

#' Update PBP Data for a Round
#'
#' Fetches chains, processes into PBP, and uploads for a specific season and round.
#'
#' @param season Season year
#' @param round Round number
#' @return Invisible NULL
update_round_pbp <- function(season, round) {
  cli::cli_progress_step("Processing PBP for {season} round {round}")

  chains <- tryCatch({
    get_week_chains(season, round)
  }, error = function(e) {
    cli::cli_warn("Failed to fetch chains for PBP {season} R{round}: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(chains) || nrow(chains) == 0) {
    cli::cli_inform("No chains data for PBP {season} round {round}")
    return(invisible(NULL))
  }

  # Process chains into PBP
  pbp <- tryCatch({
    chains %>%
      clean_pbp() %>%
      clean_model_data_epv() %>%
      clean_shots_data() %>%
      add_shot_vars() %>%
      add_epv_vars() %>%
      clean_model_data_wp() %>%
      add_wp_vars()
  }, error = function(e) {
    cli::cli_warn("Failed to process PBP for {season} R{round}: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(pbp) || nrow(pbp) == 0) {
    return(invisible(NULL))
  }

  round_02d <- sprintf("%02d", round)
  file_name <- glue::glue("pbp_data_{season}_{round_02d}")
  save_to_release(df = pbp, file_name = file_name, release_tag = "pbp-data")

  cli::cli_inform("Saved PBP: {file_name} ({nrow(pbp)} rows)")
  invisible(NULL)
}

# =============================================================================
# Aggregated File Functions
# =============================================================================

#' Update Aggregated Chains File
#'
#' Combines all per-round chains files for a season into a single file.
#'
#' @param season Season year
#' @return Invisible NULL
update_aggregated_chains <- function(season) {
  cli::cli_h2("Building aggregated chains for {season}")

  max_round <- get_max_round(season)
  start_round <- get_start_round(season)

  # Load all rounds for the season
  all_data <- lapply(start_round:max_round, function(round) {
    tryCatch({
      round_02d <- sprintf("%02d", round)
      file_name <- glue::glue("chains_data_{season}_{round_02d}")
      file_reader(file_name, "chains-data")
    }, error = function(e) {
      NULL
    })
  })

  # Combine non-null results
  combined <- data.table::rbindlist(
    Filter(Negate(is.null), all_data),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(combined) == 0) {
    cli::cli_warn("No chains data found for {season}")
    return(invisible(NULL))
  }

  file_name <- glue::glue("chains_data_{season}_all")
  save_to_release(df = combined, file_name = file_name, release_tag = "chains-data")

  cli::cli_alert_success("Saved aggregated chains: {file_name} ({nrow(combined)} rows)")
  invisible(NULL)
}

#' Update Aggregated PBP File
#'
#' Combines all per-round PBP files for a season into a single file.
#'
#' @param season Season year
#' @return Invisible NULL
update_aggregated_pbp <- function(season) {
  cli::cli_h2("Building aggregated PBP for {season}")

  max_round <- get_max_round(season)
  start_round <- get_start_round(season)

  # Load all rounds for the season
  all_data <- lapply(start_round:max_round, function(round) {
    tryCatch({
      round_02d <- sprintf("%02d", round)
      file_name <- glue::glue("pbp_data_{season}_{round_02d}")
      file_reader(file_name, "pbp-data")
    }, error = function(e) {
      NULL
    })
  })

  # Combine non-null results
  combined <- data.table::rbindlist(
    Filter(Negate(is.null), all_data),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(combined) == 0) {
    cli::cli_warn("No PBP data found for {season}")
    return(invisible(NULL))
  }

  file_name <- glue::glue("pbp_data_{season}_all")
  save_to_release(df = combined, file_name = file_name, release_tag = "pbp-data")

  cli::cli_alert_success("Saved aggregated PBP: {file_name} ({nrow(combined)} rows)")
  invisible(NULL)
}

# =============================================================================
# Season-Level Data Functions
# =============================================================================

#' Update XG Data
#'
#' @param season Season year
#' @return Invisible NULL
update_xg_data <- function(season) {
  cli::cli_progress_step("Updating xG data for {season}")

  xg_df <- tryCatch({
    match_xgs(season, TRUE)
  }, error = function(e) {
    cli::cli_warn("Failed to generate xG data: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(xg_df) || nrow(xg_df) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("xg_data_{season}")
  save_to_release(df = xg_df, file_name = file_name, release_tag = "xg-data")

  cli::cli_inform("Saved xG: {file_name} ({nrow(xg_df)} rows)")
  invisible(NULL)
}

#' Update Fixtures Data
#'
#' @param season Season year
#' @return Invisible NULL
update_fixtures <- function(season) {
  cli::cli_progress_step("Updating fixtures for {season}")

  fixtures <- tryCatch({
    fitzRoy::fetch_fixture_afl(season, comp = "AFLM")
  }, error = function(e) {
    cli::cli_warn("Failed to fetch fixtures: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(fixtures) || nrow(fixtures) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("fixtures_{season}")
  save_to_release(df = fixtures, file_name = file_name, release_tag = "fixtures-data")

  cli::cli_inform("Saved fixtures: {file_name} ({nrow(fixtures)} rows)")
  invisible(NULL)
}

#' Update Results Data
#'
#' @param season Season year
#' @return Invisible NULL
update_results <- function(season) {
  cli::cli_progress_step("Updating results for {season}")

  results <- tryCatch({
    fitzRoy::fetch_results_afl(season, comp = "AFLM")
  }, error = function(e) {
    cli::cli_warn("Failed to fetch results: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(results) || nrow(results) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("results_{season}")
  save_to_release(df = results, file_name = file_name, release_tag = "results-data")

  cli::cli_inform("Saved results: {file_name} ({nrow(results)} rows)")
  invisible(NULL)
}

#' Update Player Stats Data
#'
#' @param season Season year
#' @return Invisible NULL
update_player_stats <- function(season) {
  cli::cli_progress_step("Updating player stats for {season}")

  player_stats <- tryCatch({
    fitzRoy::fetch_player_stats_afl(season) %>%
      janitor::remove_constant() %>%
      janitor::clean_names()
  }, error = function(e) {
    cli::cli_warn("Failed to fetch player stats: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(player_stats) || nrow(player_stats) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_stats_{season}")
  save_to_release(df = player_stats, file_name = file_name, release_tag = "player_stats-data")

  cli::cli_inform("Saved player stats: {file_name} ({nrow(player_stats)} rows)")
  invisible(NULL)
}

#' Update Teams/Lineups Data
#'
#' @param season Season year
#' @return Invisible NULL
update_teams <- function(season) {
  cli::cli_progress_step("Updating teams/lineups for {season}")

  teams <- tryCatch({
    fitzRoy::fetch_lineup(season, comp = "AFLM") %>%
      dplyr::mutate(
        season = as.numeric(substr(providerId, 5, 8)),
        row_id = paste0(providerId, teamId, player.playerId)
      )
  }, error = function(e) {
    cli::cli_warn("Failed to fetch teams: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(teams) || nrow(teams) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("teams_{season}")
  save_to_release(df = teams, file_name = file_name, release_tag = "teams-data")

  cli::cli_inform("Saved teams: {file_name} ({nrow(teams)} rows)")
  invisible(NULL)
}

#' Update Player Details Data
#'
#' @param season Season year
#' @return Invisible NULL
update_player_details <- function(season) {
  cli::cli_progress_step("Updating player details for {season}")

  player_details <- tryCatch({
    fitzRoy::fetch_player_details_afl(season = season, comp = "AFLM") %>%
      dplyr::mutate(
        player_name = paste(firstName, surname),
        age = lubridate::decimal_date(lubridate::as_date(glue::glue("{season}-07-01"))) -
          lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
        row_id = paste(providerId, season)
      )
  }, error = function(e) {
    cli::cli_warn("Failed to fetch player details: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(player_details) || nrow(player_details) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_details_{season}")
  save_to_release(df = player_details, file_name = file_name, release_tag = "player_details-data")

  cli::cli_inform("Saved player details: {file_name} ({nrow(player_details)} rows)")
  invisible(NULL)
}

# =============================================================================
# Main Entry Point
# =============================================================================

#' Run Daily Data Release
#'
#' Main entry point for daily automated data release.
#' Only processes current season data to minimize runtime.
#'
#' @param force Logical. If TRUE, skip the new games check and force update.
#' @param include_aggregates Logical. If TRUE (default), rebuild aggregated files.
#' @return Invisible logical indicating success
#' @export
run_daily_release <- function(force = FALSE, include_aggregates = TRUE) {
  cli::cli_h1("Daily Data Release")

  tictoc::tic("total")

  current_season <- get_afl_season()
  current_round <- get_afl_week()

  cli::cli_inform("Season: {current_season}, Round: {current_round}")

  # Skip if no new games (unless forced)
  if (!force && !has_new_games()) {
    cli::cli_alert_info("No new games detected - skipping release")
    return(invisible(FALSE))
  }

  # -------------------------------------------------------------------------
  # 1. Update per-round data (chains + pbp) for current round
  # -------------------------------------------------------------------------
  cli::cli_h2("Updating round {current_round} data")
  tictoc::tic("round_data")

  update_round_chains(current_season, current_round)
  update_round_pbp(current_season, current_round)

  tictoc::toc(log = TRUE)

  # -------------------------------------------------------------------------
  # 2. Rebuild aggregated seasonal files
  # -------------------------------------------------------------------------
  if (include_aggregates) {
    tictoc::tic("aggregates")

    update_aggregated_chains(current_season)
    update_aggregated_pbp(current_season)

    tictoc::toc(log = TRUE)
  }

  # -------------------------------------------------------------------------
  # 3. Update season-level data files
  # -------------------------------------------------------------------------
  cli::cli_h2("Updating seasonal data")
  tictoc::tic("seasonal_data")

  update_xg_data(current_season)
  update_fixtures(current_season)
  update_results(current_season)
  update_player_stats(current_season)
  update_teams(current_season)
  update_player_details(current_season)

  tictoc::toc(log = TRUE)

  # -------------------------------------------------------------------------
  # Summary
  # -------------------------------------------------------------------------
  tictoc::toc(log = TRUE)

  cli::cli_alert_success("Daily release complete!")

  # Print timing summary
  cli::cli_h3("Timing Summary")
  timings <- tictoc::tic.log(format = TRUE)
  for (t in timings) {
    cli::cli_inform(t)
  }
  tictoc::tic.clearlog()

  invisible(TRUE)
}

# =============================================================================
# Execute if run as script
# =============================================================================

if (sys.nframe() == 0) {
  # Running as a script
  args <- commandArgs(trailingOnly = TRUE)
  force_flag <- "--force" %in% args

  run_daily_release(force = force_flag)
}
