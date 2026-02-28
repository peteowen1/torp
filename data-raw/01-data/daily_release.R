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

# Setup ----

library(dplyr)
library(glue)
library(cli)
library(piggyback)

devtools::load_all()

# Configuration ----

#' Get TORP data repository name
get_torpdata_repo <- function() {
 "peteowen1/torpdata"
}

# Helper Functions ----

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
    # Default to FALSE to avoid releasing stale/partial data when API is down
    return(FALSE)
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

# Seasonal Data Update Functions (chains + PBP) ----
# All data stored as _all files (one per season). New rounds are merged into
# the existing _all file and re-uploaded.

#' Update Chains Data for Current Season
#'
#' Fetches new round chains, merges into existing _all file, and re-uploads.
#'
#' @param season Season year
#' @param round Round number
#' @return Invisible NULL
update_season_chains <- function(season, round) {
  cli::cli_h2("Updating chains_data_{season}_all with round {round}")

  # Fetch new round data from fitzRoy
  new_chains <- tryCatch({
    get_week_chains(season, round)
  }, error = function(e) {
    cli::cli_warn("Failed to fetch chains for {season} R{round}: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(new_chains) || nrow(new_chains) == 0) {
    cli::cli_inform("No chains data for {season} round {round}")
    return(invisible(NULL))
  }

  cli::cli_inform("Fetched {nrow(new_chains)} new chain rows for round {round}")

  # Load existing _all file
  existing <- tryCatch({
    file_reader(glue::glue("chains_data_{season}_all"), "chains-data")
  }, error = function(e) {
    cli::cli_inform("No existing _all file for {season} - creating fresh")
    NULL
  })

  if (!is.null(existing) && nrow(existing) > 0) {
    # Remove stale data for this round (handles re-runs)
    # Chains data uses camelCase from AFL API (roundNumber or round.roundNumber),
    # not snake_case round_number (which only exists after clean_pbp/torp_clean_names)
    existing <- data.table::as.data.table(existing)
    round_col <- if ("round_number" %in% names(existing)) {
      "round_number"
    } else if ("roundNumber" %in% names(existing)) {
      "roundNumber"
    } else if ("round.roundNumber" %in% names(existing)) {
      "round.roundNumber"
    } else {
      NULL
    }
    if (!is.null(round_col)) {
      existing <- existing[existing[[round_col]] != round, ]
    } else {
      cli::cli_warn("No round column found in existing chains data - cannot de-duplicate round {round}")
    }
    cli::cli_inform("Existing data: {nrow(existing)} rows (after removing round {round})")
  }

  # Combine
  combined <- data.table::rbindlist(
    list(existing, new_chains),
    use.names = TRUE, fill = TRUE
  )

  file_name <- glue::glue("chains_data_{season}_all")
  save_to_release(df = combined, file_name = file_name, release_tag = "chains-data")

  cli::cli_alert_success("Saved {file_name} ({nrow(combined)} rows)")
  invisible(NULL)
}

#' Update PBP Data for Current Season
#'
#' Fetches new round chains, processes into PBP, merges into existing _all file.
#'
#' @param season Season year
#' @param round Round number
#' @return Invisible NULL
update_season_pbp <- function(season, round) {
  cli::cli_h2("Updating pbp_data_{season}_all with round {round}")

  # Fetch new round chains and process into PBP
  new_pbp <- tryCatch({
    chains <- get_week_chains(season, round)
    if (is.null(chains) || nrow(chains) == 0) return(NULL)
    chains |>
      clean_pbp() |>
      clean_model_data_epv() |>
      clean_shots_data() |>
      add_shot_vars() |>
      add_epv_vars() |>
      clean_model_data_wp() |>
      add_wp_vars()
  }, error = function(e) {
    cli::cli_warn("Failed to process PBP for {season} R{round}: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(new_pbp) || nrow(new_pbp) == 0) {
    cli::cli_inform("No PBP data for {season} round {round}")
    return(invisible(NULL))
  }

  cli::cli_inform("Processed {nrow(new_pbp)} new PBP rows for round {round}")

  # Load existing _all file
  existing <- tryCatch({
    file_reader(glue::glue("pbp_data_{season}_all"), "pbp-data")
  }, error = function(e) {
    cli::cli_inform("No existing _all file for {season} - creating fresh")
    NULL
  })

  if (!is.null(existing) && nrow(existing) > 0) {
    existing <- data.table::as.data.table(existing)
    existing <- existing[round_number != round]
    cli::cli_inform("Existing data: {nrow(existing)} rows (after removing round {round})")
  }

  # Combine
  combined <- data.table::rbindlist(
    list(existing, new_pbp),
    use.names = TRUE, fill = TRUE
  )

  file_name <- glue::glue("pbp_data_{season}_all")
  save_to_release(df = combined, file_name = file_name, release_tag = "pbp-data")

  cli::cli_alert_success("Saved {file_name} ({nrow(combined)} rows)")
  invisible(NULL)
}

# Season-Level Data Functions ----

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

  # Drop list-columns (nested structs from API) that arrow can't serialize
  list_cols <- names(fixtures)[vapply(fixtures, is.list, logical(1))]
  if (length(list_cols) > 0) {
    fixtures <- fixtures[, !names(fixtures) %in% list_cols, drop = FALSE]
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
    fitzRoy::fetch_player_stats_afl(season) |>
      dplyr::select(where(~ dplyr::n_distinct(.) > 1)) |>
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

#' Update Player Game Data
#'
#' Rebuilds processed player game data from PBP, player stats, and teams data.
#'
#' @param season Season year
#' @return Invisible NULL
update_player_game_data <- function(season) {
  cli::cli_progress_step("Updating player game data for {season}")

  pgd <- tryCatch({
    pbp <- load_pbp(season, rounds = TRUE)
    pstats <- load_player_stats(season)
    teams_data <- load_teams(season)
    create_player_game_data(pbp, pstats, teams_data)
  }, error = function(e) {
    cli::cli_warn("Failed to create player game data: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(pgd) || nrow(pgd) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_game_{season}")
  save_to_release(df = pgd, file_name = file_name, release_tag = "player_game-data")

  cli::cli_inform("Saved player game data: {file_name} ({nrow(pgd)} rows)")
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

# Derived Data Functions ----
# These run after upstream data is updated, computing derived datasets
# from already-released data (PBP, player_game_data, etc.)

#' Update Player Game Ratings
#'
#' Computes per-game TORP ratings for a season and releases to torpdata.
#'
#' @param season Season year
#' @return Invisible NULL
update_player_game_ratings <- function(season) {
  cli::cli_progress_step("Updating player game ratings for {season}")

  ratings <- tryCatch({
    pgd <- load_player_game_data(season)
    start_round <- get_start_round(season)
    max_round <- get_max_round(season)
    player_game_ratings(season, start_round:max_round, player_game_data = pgd)
  }, error = function(e) {
    cli::cli_warn("Failed to compute player game ratings: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(ratings) || nrow(ratings) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_game_ratings_{season}")
  save_to_release(df = ratings, file_name = file_name, release_tag = "player_game_ratings-data")

  cli::cli_inform("Saved player game ratings: {file_name} ({nrow(ratings)} rows)")
  invisible(NULL)
}

#' Update Player Season Ratings
#'
#' Computes season-total TORP ratings for a season and releases to torpdata.
#'
#' @param season Season year
#' @return Invisible NULL
update_player_season_ratings <- function(season) {
  cli::cli_progress_step("Updating player season ratings for {season}")

  ratings <- tryCatch({
    max_round <- get_max_round(season)
    start_round <- get_start_round(season)
    player_season_ratings(season, start_round:max_round)
  }, error = function(e) {
    cli::cli_warn("Failed to compute player season ratings: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(ratings) || nrow(ratings) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_season_ratings_{season}")
  save_to_release(df = ratings, file_name = file_name, release_tag = "player_season_ratings-data")

  cli::cli_inform("Saved player season ratings: {file_name} ({nrow(ratings)} rows)")
  invisible(NULL)
}

#' Update EP/WP Chart Data
#'
#' Selects charting-relevant columns from PBP and releases as a lightweight file.
#'
#' @param season Season year
#' @return Invisible NULL
update_ep_wp_chart <- function(season) {
  cli::cli_progress_step("Updating EP/WP chart data for {season}")

  chart_data <- tryCatch({
    pbp <- load_pbp(season, rounds = TRUE)
    if (nrow(pbp) == 0) return(NULL)

    chart_cols <- c(
      "match_id", "season", "round_number", "period", "period_seconds",
      "total_seconds", "display_order",
      "home_team_team_name", "away_team_team_name", "team", "home",
      "pos_team_points", "opp_team_points", "points_diff",
      "home_points", "away_points",
      "exp_pts", "delta_epv", "wp", "wpa",
      "description", "player_name", "play_type",
      "shot_row", "points_shot"
    )

    # Only keep columns that exist in the data
    available_cols <- intersect(chart_cols, names(pbp))
    missing_cols <- setdiff(chart_cols, names(pbp))
    if (length(missing_cols) > 0) {
      cli::cli_warn("EP/WP chart missing {length(missing_cols)} expected column{?s}: {.val {missing_cols}}")
    }
    pbp[, available_cols, drop = FALSE]
  }, error = function(e) {
    cli::cli_warn("Failed to create EP/WP chart data: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(chart_data) || nrow(chart_data) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("ep_wp_chart_{season}_all")
  save_to_release(df = chart_data, file_name = file_name, release_tag = "ep_wp_chart-data")

  cli::cli_inform("Saved EP/WP chart: {file_name} ({nrow(chart_data)} rows, {ncol(chart_data)} cols)")
  invisible(NULL)
}

# Main Entry Point ----

#' Run Daily Data Release
#'
#' Main entry point for daily automated data release.
#' Only processes current season data to minimize runtime.
#'
#' @param force Logical. If TRUE, skip the new games check and force update.
#' @return Invisible logical indicating success
#' @export
run_daily_release <- function(force = FALSE) {
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
  # 1. Update seasonal _all files (chains + pbp) with current round
  # -------------------------------------------------------------------------
  cli::cli_h2("Updating season files with round {current_round}")
  tictoc::tic("season_data")

  update_season_chains(current_season, current_round)
  update_season_pbp(current_season, current_round)

  tictoc::toc(log = TRUE)

  # -------------------------------------------------------------------------
  # 3. Update season-level data files
  # -------------------------------------------------------------------------
  cli::cli_h2("Updating seasonal data")
  tictoc::tic("seasonal_data")

  seasonal_failures <- character()
  seasonal_fns <- list(
    xg_data = update_xg_data,
    fixtures = update_fixtures,
    results = update_results,
    player_stats = update_player_stats,
    teams = update_teams,
    player_details = update_player_details,
    player_game_data = update_player_game_data
  )
  for (nm in names(seasonal_fns)) {
    tryCatch(seasonal_fns[[nm]](current_season), error = function(e) {
      seasonal_failures <<- c(seasonal_failures, nm)
      cli::cli_warn("Failed: {nm}: {conditionMessage(e)}")
    })
  }

  tictoc::toc(log = TRUE)

  # -------------------------------------------------------------------------
  # 4. Update derived data (depends on upstream data being current)
  # -------------------------------------------------------------------------
  cli::cli_h2("Updating derived data")
  tictoc::tic("derived_data")

  derived_failures <- character()
  tryCatch(update_player_game_ratings(current_season), error = function(e) {
    derived_failures <<- c(derived_failures, "player_game_ratings")
    cli::cli_warn("Failed: player_game_ratings: {conditionMessage(e)}")
  })
  tryCatch(update_player_season_ratings(current_season), error = function(e) {
    derived_failures <<- c(derived_failures, "player_season_ratings")
    cli::cli_warn("Failed: player_season_ratings: {conditionMessage(e)}")
  })
  tryCatch(update_ep_wp_chart(current_season), error = function(e) {
    derived_failures <<- c(derived_failures, "ep_wp_chart")
    cli::cli_warn("Failed: ep_wp_chart: {conditionMessage(e)}")
  })

  tictoc::toc(log = TRUE)

  # -------------------------------------------------------------------------
  # Summary
  # -------------------------------------------------------------------------
  tictoc::toc(log = TRUE)

  if (length(derived_failures) > 0) {
    cli::cli_alert_warning("Daily release complete with {length(derived_failures)} derived data failure{?s}: {paste(derived_failures, collapse = ', ')}")
  } else {
    cli::cli_alert_success("Daily release complete!")
  }

  # Print timing summary
  cli::cli_h3("Timing Summary")
  timings <- tictoc::tic.log(format = TRUE)
  for (t in timings) {
    cli::cli_inform(t)
  }
  tictoc::tic.clearlog()

  invisible(TRUE)
}

# Execute if run as script ----

if (sys.nframe() == 0) {
  # Running as a script
  args <- commandArgs(trailingOnly = TRUE)
  force_flag <- "--force" %in% args

  run_daily_release(force = force_flag)
}
