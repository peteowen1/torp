# Test script for parallel player ratings functionality
library(devtools)
library(fitzRoy)
library(tidyverse)
devtools::load_all()

cat("Testing parallel player ratings functionality...\n")

# Source the build-player-ratings.R file to get the updated get_torp_df function
source("data-raw/build-player-ratings.R")

# Test the parallel implementation
cat("\n=== Testing get_torp_df with parallel processing ===\n")
start_time <- Sys.time()

tryCatch({
  # Test with a small set of rounds to verify it works
  cat("Testing get_torp_df(2024, 1:3)...\n")
  test_torp_df <- get_torp_df(2024, 1:3)
  
  end_time <- Sys.time()
  processing_time <- as.numeric(end_time - start_time)
  
  cat(sprintf("Success! Processed %d rounds in %.2f seconds\n", length(1:3), processing_time))
  cat(sprintf("Generated %d player rating records\n", nrow(test_torp_df)))
  cat(sprintf("Columns: %s\n", paste(colnames(test_torp_df), collapse = ", ")))
  
  # Verify row_id column was created correctly
  if ("row_id" %in% colnames(test_torp_df)) {
    cat("✓ row_id column created successfully\n")
    cat(sprintf("Sample row_ids: %s\n", paste(head(test_torp_df$row_id, 3), collapse = ", ")))
  } else {
    cat("✗ row_id column missing!\n")
  }
  
  # Check for expected columns
  expected_cols <- c("player_id", "season", "round", "torp")
  missing_cols <- setdiff(expected_cols, colnames(test_torp_df))
  if (length(missing_cols) == 0) {
    cat("✓ All expected columns present\n")
  } else {
    cat(sprintf("✗ Missing columns: %s\n", paste(missing_cols, collapse = ", ")))
  }
  
}, error = function(e) {
  cat(sprintf("Error occurred: %s\n", conditionMessage(e)))
  cat("Full error:\n")
  print(e)
})

cat("\nTest complete!\n")