# Migration Script: Convert RDS files to Parquet on torpdata
#
# This script downloads existing RDS files from torpdata and re-uploads
# them as parquet files.
#
# Usage:
#   Rscript data-raw/debug/migrate_rds_to_parquet.R
#
# Note: This script uses the OLD format (RDS) to read and NEW format (parquet) to write.

library(dplyr)
library(glue)
library(cli)
library(piggyback)
library(arrow)

devtools::load_all()

# =============================================================================
# Configuration
# =============================================================================

TORPDATA_REPO <- "peteowen1/torpdata"
BASE_URL <- glue::glue("https://github.com/{TORPDATA_REPO}/releases/download")

# Seasons to migrate
SEASONS <- 2021:2025

# =============================================================================
# Helper Functions
# =============================================================================

#' Read RDS from URL (old format) - downloads to temp file first
read_rds_from_url <- function(url) {
  temp_file <- tempfile(fileext = ".rds")
  tryCatch({
    cli::cli_inform("  Reading: {basename(url)}")
    # Download to temp file first (avoids connection issues)
    download.file(url, temp_file, mode = "wb", quiet = TRUE)
    result <- readRDS(temp_file)
    unlink(temp_file)
    result
  }, error = function(e) {
    unlink(temp_file)
    cli::cli_warn("  Failed to read {basename(url)}: {conditionMessage(e)}")
    NULL
  })
}

#' Save as parquet to release (new format)
save_parquet_to_release <- function(df, file_name, release_tag) {
  if (is.null(df) || nrow(df) == 0) {
    cli::cli_warn("  Skipping {file_name} - no data")
    return(invisible(FALSE))
  }

  temp_dir <- tempdir(check = TRUE)
  parquet_name <- paste0(file_name, ".parquet")
  arrow::write_parquet(df, file.path(temp_dir, parquet_name))

  piggyback::pb_upload(
    file.path(temp_dir, parquet_name),
    repo = TORPDATA_REPO,
    tag = release_tag
  )

  cli::cli_inform("  Uploaded: {parquet_name}")
  invisible(TRUE)
}

#' Get starting round for a season
get_start_round <- function(season) {
  if (season >= 2024) return(0)
  return(1)
}

#' Get max round for a season
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
# Migration Functions
# =============================================================================

#' Migrate per-round data (chains, pbp)
migrate_per_round_data <- function(data_type, file_prefix, release_tag, seasons = SEASONS) {
  cli::cli_h1("Migrating {data_type} (per-round files)")

  file_counter <- 0

  for (season in seasons) {
    cli::cli_h2("Season {season}")

    start_round <- get_start_round(season)
    max_round <- get_max_round(season)

    for (round in start_round:max_round) {
      round_02d <- sprintf("%02d", round)
      file_name <- glue::glue("{file_prefix}_{season}_{round_02d}")
      url <- glue::glue("{BASE_URL}/{release_tag}/{file_name}.rds")

      df <- read_rds_from_url(url)
      if (!is.null(df)) {
        save_parquet_to_release(df, file_name, release_tag)
      }

      # Periodic cleanup every 20 files
      file_counter <- file_counter + 1
      if (file_counter %% 20 == 0) {
        gc()
      }
    }

    # Also migrate aggregated file
    agg_file_name <- glue::glue("{file_prefix}_{season}_all")
    agg_url <- glue::glue("{BASE_URL}/{release_tag}/{agg_file_name}.rds")
    agg_df <- read_rds_from_url(agg_url)
    if (!is.null(agg_df)) {
      save_parquet_to_release(agg_df, agg_file_name, release_tag)
    }

    # Cleanup after each season
    gc()
  }
}

#' Migrate per-season data (fixtures, results, etc.)
migrate_per_season_data <- function(data_type, file_prefix, release_tag, seasons = SEASONS) {
  cli::cli_h1("Migrating {data_type} (per-season files)")

  for (season in seasons) {
    file_name <- glue::glue("{file_prefix}_{season}")
    url <- glue::glue("{BASE_URL}/{release_tag}/{file_name}.rds")

    df <- read_rds_from_url(url)
    if (!is.null(df)) {
      save_parquet_to_release(df, file_name, release_tag)
    }
  }
}

#' Migrate fixtures (includes historical from 2018)
migrate_fixtures_data <- function() {
  cli::cli_h1("Migrating fixtures (2018-present)")

  for (season in 2018:get_afl_season()) {
    file_name <- glue::glue("fixtures_{season}")
    url <- glue::glue("{BASE_URL}/fixtures-data/{file_name}.rds")

    df <- read_rds_from_url(url)
    if (!is.null(df)) {
      save_parquet_to_release(df, file_name, "fixtures-data")
    }
    gc()
  }
}

# =============================================================================
# Main Migration
# =============================================================================

run_migration <- function(
    migrate_chains = TRUE,
    migrate_pbp = TRUE,
    migrate_fixtures = TRUE,
    migrate_results = TRUE,
    migrate_player_stats = TRUE,
    migrate_teams = TRUE,
    migrate_player_details = TRUE,
    migrate_xg = TRUE,
    migrate_predictions = TRUE,
    start_season = NULL
) {
  cli::cli_h1("Starting RDS to Parquet Migration")
  cli::cli_inform("Repository: {TORPDATA_REPO}")
  cli::cli_inform("Seasons: {paste(SEASONS, collapse = ', ')}")

  tictoc::tic("total")

  seasons_to_use <- if (!is.null(start_season)) {
    SEASONS[SEASONS >= start_season]
  } else {
    SEASONS
  }

  # 1. Chains data (per-round + aggregated)
  if (migrate_chains) {
    tictoc::tic("chains")
    migrate_per_round_data("chains", "chains_data", "chains-data", seasons_to_use)
    tictoc::toc(log = TRUE)
  }

  # 2. PBP data (per-round + aggregated)
  if (migrate_pbp) {
    tictoc::tic("pbp")
    migrate_per_round_data("pbp", "pbp_data", "pbp-data", seasons_to_use)
    tictoc::toc(log = TRUE)
  }

  # 3. Fixtures (2018-present)
  if (migrate_fixtures) {
    tictoc::tic("fixtures")
    migrate_fixtures_data()
    tictoc::toc(log = TRUE)
  }

  # 4. Results
  if (migrate_results) {
    tictoc::tic("results")
    migrate_per_season_data("results", "results", "results-data", seasons_to_use)
    tictoc::toc(log = TRUE)
  }

  # 5. Player stats
  if (migrate_player_stats) {
    tictoc::tic("player_stats")
    migrate_per_season_data("player_stats", "player_stats", "player_stats-data", seasons_to_use)
    tictoc::toc(log = TRUE)
  }

  # 6. Teams/lineups
  if (migrate_teams) {
    tictoc::tic("teams")
    migrate_per_season_data("teams", "teams", "teams-data", seasons_to_use)
    tictoc::toc(log = TRUE)
  }

  # 7. Player details
  if (migrate_player_details) {
    tictoc::tic("player_details")
    migrate_per_season_data("player_details", "player_details", "player_details-data", seasons_to_use)
    tictoc::toc(log = TRUE)
  }

  # 8. XG data
  if (migrate_xg) {
    tictoc::tic("xg")
    migrate_per_season_data("xg", "xg_data", "xg-data", seasons_to_use)
    tictoc::toc(log = TRUE)
  }

  # 9. Predictions
  if (migrate_predictions) {
    tictoc::tic("predictions")
    migrate_per_season_data("predictions", "predictions", "predictions", seasons_to_use)
    tictoc::toc(log = TRUE)
  }

  tictoc::toc(log = TRUE)

  # Print timing summary
  cli::cli_h1("Migration Complete!")
  cli::cli_h2("Timing Summary")
  tictoc::tic.log(format = TRUE) %>% unlist() %>% cat(sep = "\n")
  tictoc::tic.clearlog()
}

# =============================================================================
# Execute
# =============================================================================

if (sys.nframe() == 0) {
  run_migration()
}
