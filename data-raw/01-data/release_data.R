# Legacy Data Release Script
#
# NOTE: For automated daily releases, use daily_release.R instead.
# This script is kept for manual/ad-hoc releases and historical reference.
#
# For daily automated releases: data-raw/01-data/daily_release.R
# For creating historical aggregates: data-raw/01-data/create_aggregated_files.R

# Setup ----

library(dplyr)
library(stringr)
library(httr)
library(piggyback)
library(mgcv)

tictoc::tic('total')

devtools::load_all()

# Chains Data ----
tictoc::tic('chains')

#' Build full season chains _all file
#'
#' Fetches all rounds for a season from fitzRoy and saves as _all file.
#'
#' @param season Season year
#' @param rounds Rounds to include (default: all)
release_chains_season <- function(season, rounds = NULL) {
  if (is.null(rounds)) {
    start <- if (season >= 2024) 0 else 1
    end <- if (season == get_afl_season()) get_afl_week() else 28
    rounds <- start:end
  }

  all_chains <- purrr::map(rounds, function(round) {
    tryCatch(get_week_chains(season, round), error = function(e) NULL)
  })

  combined <- data.table::rbindlist(purrr::compact(all_chains), use.names = TRUE, fill = TRUE)

  if (nrow(combined) > 0) {
    file_name <- glue::glue("chains_data_{season}_all")
    save_to_release(df = combined, file_name = file_name, release_tag = "chains-data")
    cli::cli_inform("Saved {file_name} ({nrow(combined)} rows)")
  }
}

# Release current season chains
release_chains_season(get_afl_season())
# Historical: release_chains_season(2021) etc.

tictoc::toc(log = TRUE)

# PBP Data ----
tictoc::tic('pbp')

#' Build full season PBP _all file
#'
#' Fetches fresh chains from fitzRoy, processes through full pipeline, saves as _all file.
#' NOTE: Fetches directly from fitzRoy rather than torpdata to avoid stale data
#' from piggyback cache delays.
#'
#' @param season Season year
release_pbp_season <- function(season) {
  start <- if (season >= 2024) 0 else 1
  end <- if (season == get_afl_season()) get_afl_week() else 28
  rounds <- start:end

  all_chains <- purrr::map(rounds, function(round) {
    tryCatch(get_week_chains(season, round), error = function(e) NULL)
  })

  chains <- data.table::rbindlist(purrr::compact(all_chains), use.names = TRUE, fill = TRUE)

  if (nrow(chains) == 0) {
    cli::cli_warn("No chains data fetched for {season} - skipping PBP release")
    return(invisible(NULL))
  }

  model_data_wp <- chains |>
    clean_pbp() |>
    clean_model_data_epv() |>
    clean_shots_data() |>
    add_shot_vars() |>
    add_epv_vars() |>
    clean_model_data_wp() |>
    add_wp_vars()

  file_name <- glue::glue("pbp_data_{season}_all")
  save_to_release(df = model_data_wp, file_name = file_name, release_tag = "pbp-data")
  cli::cli_inform("Saved {file_name} ({nrow(model_data_wp)} rows)")
}

# Release current season PBP
release_pbp_season(get_afl_season())
# Historical: release_pbp_season(2021) etc.

tictoc::toc(log = TRUE)

# XG Data ----
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

# Player Stats Data ----
tictoc::tic('player stats')
get_player_stats <- function(season) {
  player_stats <- fitzRoy::fetch_player_stats_afl(season) |>
    janitor::remove_constant() |>
    janitor::clean_names()

  file_name <- glue::glue("player_stats_{season}")

  # return(futures)
  save_to_release(df = player_stats, file_name = file_name, release_tag = "player_stats-data")
}

purrr::walk(2025:get_afl_season(), ~ get_player_stats(.))

tictoc::toc(log= TRUE)

# Player Game Data ----
tictoc::tic('player game data')
get_player_game_data <- function(season) {
  pbp <- load_pbp(season, rounds = TRUE)
  pstats <- load_player_stats(season)
  teams <- load_teams(season)
  pgd <- create_player_game_data(pbp, pstats, teams)

  file_name <- glue::glue("player_game_{season}")

  save_to_release(df = pgd, file_name = file_name, release_tag = "player_game-data")
}

purrr::walk(2021:get_afl_season(), ~ get_player_game_data(.))

tictoc::toc(log= TRUE)

# Fixtures Data ----
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

# Team Lineups Data ----
tictoc::tic('lineups')
get_teams <- function(season) {
  ### update teams file (90 secs per season)
  teams <- fitzRoy::fetch_lineup(season, comp = "AFLM") |>
    dplyr::mutate(
      season = as.numeric(substr(providerId, 5, 8)),
      row_id = paste0(providerId, teamId, player.playerId)
    ) # |> dplyr::filter(!is.na(player.playerId))

  file_name <- glue::glue("teams_{season}")

  # return(futures)
  save_to_release(df = teams, file_name = file_name, release_tag = "teams-data")
}

purrr::walk(2025:get_afl_season(), ~ get_teams(.))

tictoc::toc(log= TRUE)

# Results Data ----
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

# Player Details Data ----
tictoc::tic('player details')
get_player_details <- function(season) {
  ##### update players file (20 secs per season)
  player_details <-
    fitzRoy::fetch_player_details_afl(season = season, comp = "AFLM") |>
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

# Timing Summary ----
tictoc::tic.log(format = TRUE) |> unlist()

