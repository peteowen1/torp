# Manual test for fixture caching functionality
library(devtools)
load_all()

cat("Testing fixture caching...\n")

# Clear any existing cache
clear_fixture_cache()
cat("Cache cleared\n")

# Test 1: First load should be cache miss
cat("\n=== Test 1: First load (should be cache miss) ===\n")
start_time <- Sys.time()
fixtures1 <- load_fixtures(seasons = 2023, verbose = TRUE)
end_time <- Sys.time()
first_load_time <- as.numeric(end_time - start_time)
cat(sprintf("First load took %.2f seconds, loaded %d rows\n", first_load_time, nrow(fixtures1)))

# Test 2: Second load should be cache hit
cat("\n=== Test 2: Second load (should be cache hit) ===\n")
start_time <- Sys.time()
fixtures2 <- load_fixtures(seasons = 2023, verbose = TRUE)
end_time <- Sys.time()
second_load_time <- as.numeric(end_time - start_time)
cat(sprintf("Second load took %.2f seconds, loaded %d rows\n", second_load_time, nrow(fixtures2)))

# Verify data is identical
cat(sprintf("Data identical: %s\n", identical(fixtures1, fixtures2)))
cat(sprintf("Speed improvement: %.1fx faster\n", first_load_time / second_load_time))

# Test 3: Check cache info
cat("\n=== Test 3: Cache information ===\n")
cache_info <- get_cache_info()
print(cache_info)

# Test 4: Test load_fixtures(TRUE) - the common problematic case
cat("\n=== Test 4: Testing load_fixtures(all=TRUE) ===\n")
start_time <- Sys.time()
fixtures_all1 <- load_fixtures(all = TRUE, verbose = TRUE)
end_time <- Sys.time()
all_first_time <- as.numeric(end_time - start_time)
cat(sprintf("First all=TRUE load took %.2f seconds, loaded %d rows\n", all_first_time, nrow(fixtures_all1)))

start_time <- Sys.time()
fixtures_all2 <- load_fixtures(all = TRUE, verbose = TRUE)
end_time <- Sys.time()
all_second_time <- as.numeric(end_time - start_time)
cat(sprintf("Second all=TRUE load took %.2f seconds, loaded %d rows\n", all_second_time, nrow(fixtures_all2)))

cat(sprintf("All=TRUE speed improvement: %.1fx faster\n", all_first_time / all_second_time))

# Test 5: Simulate the get_afl_week() scenario (calls load_fixtures(TRUE) twice)
cat("\n=== Test 5: Simulating get_afl_week() scenario ===\n")
clear_fixture_cache()

start_time <- Sys.time()
# This simulates what get_afl_week() does
past_fixtures <- load_fixtures(TRUE, verbose = TRUE)
future_fixtures <- load_fixtures(TRUE, verbose = TRUE)  # This should be a cache hit
end_time <- Sys.time()
total_time <- as.numeric(end_time - start_time)

cat(sprintf("Double load_fixtures(TRUE) took %.2f seconds total\n", total_time))
cat("Second call should have been a cache hit (see messages above)\n")

cat("\n=== Final cache status ===\n")
final_cache_info <- get_cache_info()
print(final_cache_info)

cat("\nTesting complete!\n")