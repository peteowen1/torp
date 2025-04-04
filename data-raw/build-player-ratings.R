library(devtools)
library(fitzRoy)
library(tidyverse)
library(furrr)
devtools::load_all()

# Define a helper function to calculate torp ratings for a given year and round range
get_torp_df <- function(year, rounds) {
  torp_df <- purrr::map(rounds, ~ torp_ratings(year, .x), .progress = TRUE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(row_id = paste0(player_id, season, sprintf("%02d", round)))

  return(torp_df)
}

# Set up parallel processing
plan(multisession, workers = parallelly::availableCores() - 2)

tictoc::tic() # Start timing

# Calculate torp ratings for each year in parallel
# torp_df_21 <- get_torp_df(2021, 1:27)
# torp_df_22 <- get_torp_df(2022, 1:27)
# torp_df_23 <- get_torp_df(2023, 1:28)
# torp_df_24 <- get_torp_df(2024, 0:28)
torp_df_25 <- get_torp_df(2025, 0:get_afl_week("next"))

torp_df_total <- torp_df_total %>% rows_upsert(torp_df_25, by = "row_id")

# Combine all years into a single data frame
# torp_df_total <- bind_rows(torp_df_21, torp_df_22, torp_df_23, torp_df_24)

tictoc::toc() # Stop timing

# Save the final combined data frame
use_data(torp_df_total, overwrite = TRUE)

# GIT PUSH AFTER CHANGE
