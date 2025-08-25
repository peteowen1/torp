# Quick test for the parallel processing fix
library(devtools)
load_all()

cat("Testing the parallel processing fix...\n")

# Test with a simple load that should use parallel processing
tryCatch({
  cat("Testing load_teams(TRUE) - this should trigger parallel loading...\n")
  teams <- load_teams(TRUE)
  cat(sprintf("Success! Loaded %d rows of team data\n", nrow(teams)))
  
  cat("Testing load_chains() with multiple seasons/rounds...\n")
  chains_data <- load_chains(seasons = 2023:2024, rounds = 1:2)
  cat(sprintf("Success! Loaded %d rows of chains data\n", nrow(chains_data)))
  
  cat("Testing load_pbp() with multiple seasons/rounds...\n")
  pbp_data <- load_pbp(seasons = 2023:2024, rounds = 1:2)
  cat(sprintf("Success! Loaded %d rows of PBP data\n", nrow(pbp_data)))
  
}, error = function(e) {
  cat(sprintf("Error occurred: %s\n", conditionMessage(e)))
  cat("Full error:\n")
  print(e)
  traceback()
})

cat("Test complete!\n")