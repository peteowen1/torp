# Single file migration - avoids long-running sessions
# Usage: Rscript migrate_single.R <release_tag> <file_name>
# Example: Rscript migrate_single.R fixtures-data fixtures_2021

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript migrate_single.R <release_tag> <file_name>")

release_tag <- args[1]
file_name <- args[2]

library(arrow)
library(piggyback)

TORPDATA_REPO <- "peteowen1/torpdata"
BASE_URL <- "https://github.com/peteowen1/torpdata/releases/download"

url <- paste0(BASE_URL, "/", release_tag, "/", file_name, ".rds")
temp_rds <- tempfile(fileext = ".rds")
temp_parquet <- tempfile(fileext = ".parquet")

tryCatch({
  cat("Downloading:", file_name, "\n")
  download.file(url, temp_rds, mode = "wb", quiet = TRUE)

  cat("Reading RDS...\n")
  df <- readRDS(temp_rds)
  unlink(temp_rds)

  cat("Writing parquet...\n")
  arrow::write_parquet(df, temp_parquet)

  cat("Uploading...\n")
  piggyback::pb_upload(temp_parquet, repo = TORPDATA_REPO, tag = release_tag,
                       name = paste0(file_name, ".parquet"))

  cat("Done:", file_name, ".parquet\n")
}, error = function(e) {
  cat("Error:", conditionMessage(e), "\n")
})

unlink(temp_rds)
unlink(temp_parquet)
