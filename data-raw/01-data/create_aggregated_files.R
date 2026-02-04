# Create Historical Aggregated Files for torpdata
#
# One-time script to create aggregated files for all historical seasons.
# These aggregated files (e.g., chains_data_2024_all.parquet) significantly
# speed up bulk data loading (174 requests -> 6 requests).
#
# Run this once after implementing the daily release pipeline, then the
# daily_release.R script will keep current season's aggregated file updated.
#
# Usage:
#   Rscript data-raw/01-data/create_aggregated_files.R
#   Rscript data-raw/01-data/create_aggregated_files.R 2021 2023  # specific range
#   Rscript data-raw/01-data/create_aggregated_files.R --chains-only
#   Rscript data-raw/01-data/create_aggregated_files.R --pbp-only

library(dplyr)
library(data.table)
library(piggyback)
library(cli)
library(glue)

devtools::load_all()

# =============================================================================
# Configuration
# =============================================================================

SEASONS_TO_AGGREGATE <- 2021:2025
DATA_TYPES <- c("chains", "pbp")

#' Get starting round for a season
#'
#' @param season Season year
#' @return Integer round number
get_start_round <- function(season) {
  if (season >= 2024) return(0)
  return(1)
}

#' Get maximum round for a season
#'
#' @param season Season year
#' @return Integer round number
get_max_round <- function(season) {
  current_season <- get_afl_season()
  if (season == current_season) {
    return(get_afl_week())
  }
  # Historical seasons
  if (season == 2021 || season == 2022) return(27)
  return(28)
}

# =============================================================================
# Core Functions
# =============================================================================

#' Create aggregated file for a specific data type and season
#'
#' Downloads all per-round files for a season and combines them into
#' a single aggregated file for faster bulk loading.
#'
#' @param data_type Either "chains" or "pbp"
#' @param season Season year
#' @param force If TRUE, recreate even if aggregated file exists
#' @return Invisible logical indicating success
create_aggregated_file <- function(data_type, season, force = FALSE) {
  cli::cli_h2("Creating aggregated {data_type} file for {season}")

  # Set up file naming based on data type
  if (data_type == "chains") {
    file_prefix <- "chains_data"
    release_tag <- "chains-data"
  } else if (data_type == "pbp") {
    file_prefix <- "pbp_data"
    release_tag <- "pbp-data"
  } else {
    cli::cli_abort("Unknown data type: {data_type}")
  }

  start_round <- get_start_round(season)
  max_round <- get_max_round(season)

  cli::cli_inform("Loading rounds {start_round} to {max_round}...")

  # Load all rounds with progress indicator
  all_data <- list()
  rounds_loaded <- 0

  for (round in start_round:max_round) {
    result <- tryCatch({
      round_02d <- sprintf("%02d", round)
      file_name <- glue::glue("{file_prefix}_{season}_{round_02d}")
      file_reader(file_name, release_tag)
    }, error = function(e) {
      NULL
    })

    if (!is.null(result) && nrow(result) > 0) {
      all_data[[length(all_data) + 1]] <- result
      rounds_loaded <- rounds_loaded + 1
      cli::cli_inform("  Round {round}: {nrow(result)} rows")
    } else {
      cli::cli_warn("  Round {round}: not found")
    }
  }

  if (length(all_data) == 0) {
    cli::cli_warn("No data found for {data_type} {season}")
    return(invisible(FALSE))
  }

  # Combine all data
  combined <- data.table::rbindlist(all_data, use.names = TRUE, fill = TRUE)
  size_mb <- round(object.size(combined) / 1024 / 1024, 1)

  cli::cli_inform("Combined {rounds_loaded} rounds: {nrow(combined)} rows ({size_mb} MB)")

  # Save aggregated file
  agg_file_name <- glue::glue("{file_prefix}_{season}_all")
  save_to_release(df = combined, file_name = agg_file_name, release_tag = release_tag)
  cli::cli_alert_success("Saved {agg_file_name}.parquet")

  return(invisible(TRUE))
}

#' Create all historical aggregates
#'
#' Main function to create aggregated files for specified seasons and data types.
#'
#' @param seasons Vector of seasons to process
#' @param data_types Character vector of data types ("chains", "pbp", or both)
#' @return Invisible NULL
create_all_historical_aggregates <- function(
    seasons = SEASONS_TO_AGGREGATE,
    data_types = DATA_TYPES
) {
  cli::cli_h1("Creating Historical Aggregated Files")

  tictoc::tic("total")

  cli::cli_inform("Seasons: {paste(seasons, collapse = ', ')}")
  cli::cli_inform("Data types: {paste(data_types, collapse = ', ')}")

  for (data_type in data_types) {
    cli::cli_h1("{toupper(data_type)} Data")

    for (season in seasons) {
      tictoc::tic(glue::glue("{data_type}_{season}"))

      tryCatch({
        create_aggregated_file(data_type, season)
      }, error = function(e) {
        cli::cli_alert_danger("Failed for {data_type} {season}: {conditionMessage(e)}")
      })

      tictoc::toc(log = TRUE)
    }
  }

  tictoc::toc(log = TRUE)

  # Print summary
  cli::cli_h1("Timing Summary")
  tictoc::tic.log(format = TRUE) %>% unlist() %>% cat(sep = "\n")
  tictoc::tic.clearlog()

  invisible(NULL)
}

# =============================================================================
# Execute if run as script
# =============================================================================

if (sys.nframe() == 0) {
  # Running as a script - parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)

  # Default values
  seasons <- SEASONS_TO_AGGREGATE
  data_types <- DATA_TYPES

  # Parse season range if provided (two numeric arguments)
  numeric_args <- suppressWarnings(as.integer(args[!grepl("^--", args)]))
  numeric_args <- numeric_args[!is.na(numeric_args)]

  if (length(numeric_args) >= 2) {
    seasons <- numeric_args[1]:numeric_args[2]
  } else if (length(numeric_args) == 1) {
    seasons <- numeric_args[1]
  }

  # Parse data type flags
  if ("--chains-only" %in% args) {
    data_types <- "chains"
  } else if ("--pbp-only" %in% args) {
    data_types <- "pbp"
  }

  create_all_historical_aggregates(seasons = seasons, data_types = data_types)
}
