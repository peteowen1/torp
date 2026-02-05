# Batch migration script - runs one data type at a time
# Usage: Rscript migrate_batch.R <data_type> [start_season]
# data_type: chains, pbp, fixtures, results, player_stats, teams, player_details, xg, predictions

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript migrate_batch.R <data_type> [start_season]")
}

data_type <- args[1]
start_season <- if (length(args) >= 2) as.integer(args[2]) else 2021

library(dplyr)
library(glue)
library(cli)
library(piggyback)
library(arrow)
devtools::load_all()

TORPDATA_REPO <- "peteowen1/torpdata"
BASE_URL <- glue::glue("https://github.com/{TORPDATA_REPO}/releases/download")

read_rds_from_url <- function(url) {
  temp_file <- tempfile(fileext = ".rds")
  tryCatch({
    cli::cli_inform("  Reading: {basename(url)}")
    download.file(url, temp_file, mode = "wb", quiet = TRUE)
    result <- readRDS(temp_file)
    unlink(temp_file)
    result
  }, error = function(e) {
    unlink(temp_file)
    cli::cli_warn("  Failed: {conditionMessage(e)}")
    NULL
  })
}

save_parquet_to_release <- function(df, file_name, release_tag) {
  if (is.null(df) || nrow(df) == 0) return(invisible(FALSE))
  temp_dir <- tempdir(check = TRUE)
  parquet_name <- paste0(file_name, ".parquet")
  arrow::write_parquet(df, file.path(temp_dir, parquet_name))
  piggyback::pb_upload(file.path(temp_dir, parquet_name), repo = TORPDATA_REPO, tag = release_tag)
  cli::cli_inform("  Uploaded: {parquet_name}")
  invisible(TRUE)
}

get_start_round <- function(season) { if (season >= 2024) 0 else 1 }
get_max_round <- function(season) {
  if (season == get_afl_season()) return(get_afl_week())
  if (season %in% c(2021, 2022)) return(27)
  28
}

migrate_per_round <- function(file_prefix, release_tag, seasons) {
  for (season in seasons) {
    cli::cli_h2("Season {season}")
    for (round in get_start_round(season):get_max_round(season)) {
      file_name <- glue::glue("{file_prefix}_{season}_{sprintf('%02d', round)}")
      url <- glue::glue("{BASE_URL}/{release_tag}/{file_name}.rds")
      df <- read_rds_from_url(url)
      if (!is.null(df)) save_parquet_to_release(df, file_name, release_tag)
      gc()
    }
    # Aggregated
    agg_name <- glue::glue("{file_prefix}_{season}_all")
    agg_url <- glue::glue("{BASE_URL}/{release_tag}/{agg_name}.rds")
    agg_df <- read_rds_from_url(agg_url)
    if (!is.null(agg_df)) save_parquet_to_release(agg_df, agg_name, release_tag)
    gc()
  }
}

migrate_per_season <- function(file_prefix, release_tag, seasons) {
  for (season in seasons) {
    file_name <- glue::glue("{file_prefix}_{season}")
    url <- glue::glue("{BASE_URL}/{release_tag}/{file_name}.rds")
    df <- read_rds_from_url(url)
    if (!is.null(df)) save_parquet_to_release(df, file_name, release_tag)
    gc()
  }
}

seasons <- start_season:2025
cli::cli_h1("Migrating {data_type} (seasons {start_season}-2025)")

switch(data_type,
  "chains" = migrate_per_round("chains_data", "chains-data", seasons),
  "pbp" = migrate_per_round("pbp_data", "pbp-data", seasons),
  "fixtures" = {
    for (season in 2018:get_afl_season()) {
      file_name <- glue::glue("fixtures_{season}")
      url <- glue::glue("{BASE_URL}/fixtures-data/{file_name}.rds")
      df <- read_rds_from_url(url)
      if (!is.null(df)) save_parquet_to_release(df, file_name, "fixtures-data")
      gc()
    }
  },
  "results" = migrate_per_season("results", "results-data", seasons),
  "player_stats" = migrate_per_season("player_stats", "player_stats-data", seasons),
  "teams" = migrate_per_season("teams", "teams-data", seasons),
  "player_details" = migrate_per_season("player_details", "player_details-data", seasons),
  "xg" = migrate_per_season("xg_data", "xg-data", seasons),
  "predictions" = migrate_per_season("predictions", "predictions", seasons),
  stop("Unknown data type: ", data_type)
)

cli::cli_alert_success("{data_type} migration complete!")
