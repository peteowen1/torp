library(dplyr)
library(stringr)
library(httr)
library(piggyback)

devtools::load_all()
# Save to and read from releases -------------------------------------------

save_to_release <- function(df, file_name, release_tag) {

  temp_dir <- tempdir(check = TRUE)
  .f_name <- paste0(file_name,".rds")
  saveRDS(df, file.path(temp_dir, .f_name))

  piggyback::pb_upload(file.path(temp_dir, .f_name),
                       repo = "peteowen1/torpdata",
                       tag = release_tag
  )

}


file_reader <- function(file_name, release_tag) {
  f_name <- paste0(file_name, ".rds")
  piggyback::pb_download(f_name,
                         repo = "peteowen1/torpdata",
                         tag = release_tag,
                         dest = tempdir())
  temp_dir <- tempdir(check = TRUE)

  readRDS(file.path(temp_dir, f_name))
}


# Get chains data  -------------------------------------------------------------

get_chains_data <- function(season,round) {

  # Sys.setenv(TZ = "Australia/Melbourne")
  # scrape_date <- Sys.Date()
  round_02d <- sprintf("%02d", round)

  file_name <- glue::glue("chain_data_{season}_{round_02d}")

  chains <- get_week_chains(season, round)

  # return(futures)
  save_to_release(df= chains, file_name= file_name, release_tag= "chain-data")

}

# extract and save
# purrr::walk(1:27,~get_chains_data(2021,.))
# purrr::walk(1:27,~get_chains_data(2022,.))
# purrr::walk(1:get_afl_week(),~get_chains_data(2023,.))
get_chains_data(2023,get_afl_week())

# Get pbp data  -------------------------------------------------------------
get_pbp_data <- function(season,round) {

  chains <- get_week_chains(season, round)

  ### clean chains (1.5 secs per round)
  pbp <- clean_pbp(chains)

  ### filter and prep data for epv model (7.5 secs per round)
  model_data_epv <- clean_model_data_epv(pbp)

  ### add epv and wp variables  (0.5 secs per round)
  model_data_wp <- model_data_epv %>%
    clean_shots_data() %>%
    add_shot_vars() %>%
    add_epv_vars() %>%
    clean_model_data_wp() %>%
    add_wp_vars()


  round_02d <- sprintf("%02d", round)

  file_name <- glue::glue("pbp_data_{season}_{round_02d}")

  # return(futures)
  save_to_release(df= model_data_wp, file_name= file_name, release_tag= "pbp-data")

}

# extract and save
# purrr::walk(1:27,~get_pbp_data(2021,.))
# purrr::walk(1:27,~get_pbp_data(2022,.))
# purrr::walk(1:get_afl_week(),~get_pbp_data(2023,.))
get_pbp_data(2023,get_afl_week())
