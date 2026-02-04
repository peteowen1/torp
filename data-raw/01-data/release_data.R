# Legacy Data Release Script
#
# NOTE: For automated daily releases, use daily_release.R instead.
# This script is kept for manual/ad-hoc releases and historical reference.
#
# For daily automated releases: data-raw/01-data/daily_release.R
# For creating historical aggregates: data-raw/01-data/create_aggregated_files.R

library(dplyr)
library(stringr)
library(httr)
library(piggyback)
library(mgcv)

tictoc::tic('total')

devtools::load_all()

# Get chains data  -------------------------------------------------------------
tictoc::tic('chains')

get_chains_data <- function(season, round) {
  # Sys.setenv(TZ = "Australia/Melbourne")
  # scrape_date <- Sys.Date()
  round_02d <- sprintf("%02d", round)

  file_name <- glue::glue("chains_data_{season}_{round_02d}")

  chains <- get_week_chains(season, round)

  # return(futures)
  save_to_release(df = chains, file_name = file_name, release_tag = "chains-data")
}

#' Create aggregated seasonal chains file
#'
#' Combines all per-round chains files for a season into a single file
#' for faster bulk downloads.
#'
#' @param season Season year to aggregate
create_aggregated_chains <- function(season) {
  cli::cli_inform("Creating aggregated chains file for {season}...")

  # Determine rounds to include
  if (season == get_afl_season()) {
    max_round <- get_afl_week()
  } else {
    max_round <- 28
  }

  # Load all rounds for the season
  all_chains <- purrr::map(0:max_round, function(round) {
    tryCatch({
      round_02d <- sprintf("%02d", round)
      file_name <- glue::glue("chains_data_{season}_{round_02d}")
      file_reader(file_name, "chains-data")
    }, error = function(e) {
      NULL
    })
  })

  # Combine and save
  combined <- data.table::rbindlist(
    purrr::compact(all_chains),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(combined) > 0) {
    file_name <- glue::glue("chains_data_{season}_all")
    save_to_release(df = combined, file_name = file_name, release_tag = "chains-data")
    cli::cli_inform("Saved aggregated chains file: {file_name} ({nrow(combined)} rows)")
  }
}

# extract and save
# purrr::walk(1:27,~get_chains_data(2021,.))
# purrr::walk(1:27,~get_chains_data(2022,.))
# purrr::walk(1:28,~get_chains_data(2023,.))
# purrr::walk(0:28,~get_chains_data(2024,.))
# purrr::walk(0:28,~get_chains_data(2025,.))
get_chains_data(2025, get_afl_week())

# Create aggregated file for current season after uploading latest round
create_aggregated_chains(2025)

tictoc::toc(log= TRUE)

# Get pbp data  -------------------------------------------------------------
tictoc::tic('pbp')
get_pbp_data <- function(season, round) {
  chains <- get_week_chains(season, round)

  ### clean chains (1.5 secs per round)
  pbp <- clean_pbp(chains)

  ### filter and prep data for epv model (7.5 secs per round)
  model_data_epv <- clean_model_data_epv(pbp)

  ### add epv and wp variables  (0.5 secs per round)
  model_data_wp <- model_data_epv %>%
    clean_shots_data() %>%
    add_shot_vars() %>%
    add_epv_vars() %>%
    clean_model_data_wp() %>%
    add_wp_vars()


  round_02d <- sprintf("%02d", round)

  file_name <- glue::glue("pbp_data_{season}_{round_02d}")

  # return(futures)
  save_to_release(df = model_data_wp, file_name = file_name, release_tag = "pbp-data")
}

#' Create aggregated seasonal pbp file
#'
#' Combines all per-round pbp files for a season into a single file
#' for faster bulk downloads.
#'
#' @param season Season year to aggregate
create_aggregated_pbp <- function(season) {
  cli::cli_inform("Creating aggregated pbp file for {season}...")

  # Determine rounds to include
  if (season == get_afl_season()) {
    max_round <- get_afl_week()
  } else {
    max_round <- 28
  }

  # Load all rounds for the season
  all_pbp <- purrr::map(0:max_round, function(round) {
    tryCatch({
      round_02d <- sprintf("%02d", round)
      file_name <- glue::glue("pbp_data_{season}_{round_02d}")
      file_reader(file_name, "pbp-data")
    }, error = function(e) {
      NULL
    })
  })

  # Combine and save
  combined <- data.table::rbindlist(
    purrr::compact(all_pbp),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(combined) > 0) {
    file_name <- glue::glue("pbp_data_{season}_all")
    save_to_release(df = combined, file_name = file_name, release_tag = "pbp-data")
    cli::cli_inform("Saved aggregated pbp file: {file_name} ({nrow(combined)} rows)")
  }
}

# extract and save
# purrr::walk(1:27,~get_pbp_data(2021,.))
# purrr::walk(1:27,~get_pbp_data(2022,.))
# purrr::walk(1:28,~get_pbp_data(2023,.))
# purrr::walk(0:28,~get_pbp_data(2024,.))
# purrr::walk(0:get_afl_week(),~get_pbp_data(2025,.))
get_pbp_data(2025, get_afl_week())

# Create aggregated file for current season after uploading latest round
create_aggregated_pbp(2025)

tictoc::toc(log= TRUE)

# Get xg data  -------------------------------------------------------------
tictoc::tic('xg')
get_xg_data <- function(season) {
  xg_df <- match_xgs(season, TRUE)

  # round_02d <- sprintf("%02d", round)

  file_name <- glue::glue("xg_data_{season}")

  # return(futures)
  save_to_release(df = xg_df, file_name = file_name, release_tag = "xg-data")
}

purrr::walk(2025:get_afl_season(),~get_xg_data(.))
# purrr::walk(2025:get_afl_season(), ~ get_xg_data(.))

tictoc::toc(log= TRUE)

# Get player stats data  -------------------------------------------------------------
tictoc::tic('player stats')
get_player_stats <- function(season) {
  player_stats <- fitzRoy::fetch_player_stats_afl(season) %>%
    janitor::remove_constant() %>%
    janitor::clean_names()

  file_name <- glue::glue("player_stats_{season}")

  # return(futures)
  save_to_release(df = player_stats, file_name = file_name, release_tag = "player_stats-data")
}

purrr::walk(2025:get_afl_season(), ~ get_player_stats(.))

tictoc::toc(log= TRUE)

# Get fixtures data  -------------------------------------------------------------
tictoc::tic('fixtures')
get_fixtures <- function(season) {
  ### update fixtures file (17 secs)
  fixtures <- fitzRoy::fetch_fixture_afl(season, comp = "AFLM")

  file_name <- glue::glue("fixtures_{season}")

  # return(futures)
  save_to_release(df = fixtures, file_name = file_name, release_tag = "fixtures-data")
}

purrr::walk(2025:lubridate::year(Sys.Date()), ~ get_fixtures(.))

tictoc::toc(log= TRUE)

# Get team lineups data  -------------------------------------------------------------
tictoc::tic('lineups')
get_teams <- function(season) {
  ### update teams file (90 secs per season)
  teams <- fitzRoy::fetch_lineup(season, comp = "AFLM") %>%
    dplyr::mutate(
      season = as.numeric(substr(providerId, 5, 8)),
      row_id = paste0(providerId, teamId, player.playerId)
    ) # %>% dplyr::filter(!is.na(player.playerId))

  file_name <- glue::glue("teams_{season}")

  # return(futures)
  save_to_release(df = teams, file_name = file_name, release_tag = "teams-data")
}

purrr::walk(2025:get_afl_season(), ~ get_teams(.))

tictoc::toc(log= TRUE)

# Get results data  -------------------------------------------------------------
tictoc::tic('results')
get_results <- function(season) {
  ##### update results file (5 secs per season)
  results <- fitzRoy::fetch_results_afl(season, comp = "AFLM")

  file_name <- glue::glue("results_{season}")

  # return(futures)
  save_to_release(df = results, file_name = file_name, release_tag = "results-data")
}

purrr::walk(2025:get_afl_season(), ~ get_results(.))

tictoc::toc(log= TRUE)

# Get player details data  -------------------------------------------------------------
tictoc::tic('player details')
get_player_details <- function(season) {
  ##### update players file (20 secs per season)
  player_details <-
    fitzRoy::fetch_player_details_afl(season = season, comp = "AFLM") %>%
    dplyr::mutate(
      player_name = paste(firstName, surname),
      age = lubridate::decimal_date(lubridate::as_date(glue::glue("{season}-07-01"))) -
        lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
      row_id = paste(providerId, season)
    )


  file_name <- glue::glue("player_details_{season}")

  # return(futures)
  save_to_release(df = player_details, file_name = file_name, release_tag = "player_details-data")
}

purrr::walk(2025:get_afl_season(), ~ get_player_details(.))

tictoc::toc(log= TRUE)

tictoc::toc(log= TRUE)

# print a nicely formatted table of all timings
tictoc::tic.log(format = TRUE) %>% unlist()

