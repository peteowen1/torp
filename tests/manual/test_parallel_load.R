# Test script for parallel load_chains() and load_pbp() functionality
library(devtools)
load_all()

cat("Testing parallel processing in load_chains() and load_pbp()...\n")

# Test 1: load_chains() with multiple seasons/rounds (should use parallel processing)
cat("\n=== Test 1: load_chains() with multiple seasons ===\n")
start_time <- Sys.time()
chains_data <- load_chains(seasons = 2021:2022, rounds = 1:3)
end_time <- Sys.time()
load_time <- as.numeric(end_time - start_time)
cat(sprintf("load_chains(2021:2022, 1:3) took %.2f seconds, loaded %d rows\n", load_time, nrow(chains_data)))

# Test 2: load_pbp() with multiple seasons/rounds (should use parallel processing)
cat("\n=== Test 2: load_pbp() with multiple seasons ===\n")
start_time <- Sys.time()
pbp_data <- load_pbp(seasons = 2021:2022, rounds = 1:2)
end_time <- Sys.time()
load_time <- as.numeric(end_time - start_time)
cat(sprintf("load_pbp(2021:2022, 1:2) took %.2f seconds, loaded %d rows\n", load_time, nrow(pbp_data)))

# Test 3: Single URL case (should not use parallel processing)
cat("\n=== Test 3: Single season/round (sequential) ===\n")
start_time <- Sys.time()
chains_single <- load_chains(seasons = 2023, rounds = 1)
end_time <- Sys.time()
load_time <- as.numeric(end_time - start_time)
cat(sprintf("load_chains(2023, 1) took %.2f seconds, loaded %d rows\n", load_time, nrow(chains_single)))

# Test 4: Verify data integrity
cat("\n=== Test 4: Data integrity checks ===\n")
cat(sprintf("Chains data columns: %d\n", ncol(chains_data)))
cat(sprintf("PBP data columns: %d\n", ncol(pbp_data)))
cat(sprintf("Chains data unique seasons: %s\n", paste(unique(chains_data$season), collapse = ", ")))
cat(sprintf("PBP data unique seasons: %s\n", paste(unique(pbp_data$season), collapse = ", ")))

# Test 5: Error handling
cat("\n=== Test 5: Error handling ===\n")
tryCatch({
  bad_data <- load_chains(seasons = 1900)  # Should fail gracefully
  cat("Unexpected: Bad data request succeeded\n")
}, error = function(e) {
  cat(sprintf("Expected error caught: %s\n", conditionMessage(e)))
})

cat("\nTesting complete!\n")