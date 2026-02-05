# Special migration for fixtures - handles nested list columns
# Usage: Rscript migrate_fixtures.R <year>

args <- commandArgs(trailingOnly = TRUE)
year <- if (length(args) >= 1) as.integer(args[1]) else 2025

library(arrow)
library(piggyback)
library(dplyr)

TORPDATA_REPO <- "peteowen1/torpdata"
BASE_URL <- "https://github.com/peteowen1/torpdata/releases/download"

file_name <- paste0("fixtures_", year)
url <- paste0(BASE_URL, "/fixtures-data/", file_name, ".rds")
temp_rds <- tempfile(fileext = ".rds")
temp_parquet <- tempfile(fileext = ".parquet")

tryCatch({
  cat("Downloading:", file_name, "\n")
  download.file(url, temp_rds, mode = "wb", quiet = TRUE)

  cat("Reading RDS...\n")
  df <- readRDS(temp_rds)
  unlink(temp_rds)

  cat("Checking for list columns...\n")
  list_cols <- names(df)[sapply(df, is.list)]
  if (length(list_cols) > 0) {
    cat("Found list columns:", paste(list_cols, collapse = ", "), "\n")
    cat("Removing list columns for parquet compatibility...\n")
    df <- df %>% select(-all_of(list_cols))
  }

  cat("Writing parquet (", ncol(df), "columns )...\n")
  arrow::write_parquet(df, temp_parquet)

  cat("Uploading...\n")
  piggyback::pb_upload(temp_parquet, repo = TORPDATA_REPO, tag = "fixtures-data",
                       name = paste0(file_name, ".parquet"))

  cat("Done:", file_name, ".parquet\n")
}, error = function(e) {
  cat("Error:", conditionMessage(e), "\n")
})

unlink(temp_rds)
unlink(temp_parquet)
