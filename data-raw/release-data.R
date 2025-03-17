library(dplyr)
library(stringr)
library(httr)
library(piggyback)
library(mgcv)

tictoc::tic()

devtools::load_all()
# Save to and read from releases -------------------------------------------

save_to_release <- function(df, file_name, release_tag) {
  temp_dir <- tempdir(check = TRUE)
  .f_name <- paste0(file_name, ".rds")
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
    dest = tempdir()
  )
  temp_dir <- tempdir(check = TRUE)

  readRDS(file.path(temp_dir, f_name))
}


# Get chains data  -------------------------------------------------------------

get_chains_data <- function(season, round) {
  # Sys.setenv(TZ = "Australia/Melbourne")
  # scrape_date <- Sys.Date()
  round_02d <- sprintf("%02d", round)

  file_name <- glue::glue("chains_data_{season}_{round_02d}")

  chains <- get_week_chains(season, round)

  # return(futures)
  save_to_release(df = chains, file_name = file_name, release_tag = "chains-data")
}

# extract and save
# purrr::walk(1:27,~get_chains_data(2021,.))
# purrr::walk(1:27,~get_chains_data(2022,.))
# purrr::walk(1:28,~get_chains_data(2023,.))
# purrr::walk(0:28,~get_chains_data(2024,.))
# purrr::walk(0:28,~get_chains_data(2025,.))
get_chains_data(2025, get_afl_week())

# Get pbp data  -------------------------------------------------------------
get_pbp_data <- function(season, round) {
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
  save_to_release(df = model_data_wp, file_name = file_name, release_tag = "pbp-data")
}

# extract and save
# purrr::walk(1:27,~get_pbp_data(2021,.))
# purrr::walk(1:27,~get_pbp_data(2022,.))
# purrr::walk(1:28,~get_pbp_data(2023,.))
# purrr::walk(0:28,~get_pbp_data(2024,.))
# purrr::walk(0:28,~get_pbp_data(2025,.))
get_pbp_data(2025, get_afl_week())

# Get xg data  -------------------------------------------------------------
get_xg_data <- function(season) {

  xg_df <- match_xgs(season, TRUE)

  # round_02d <- sprintf("%02d", round)

  file_name <- glue::glue("xg_data_{season}")

  # return(futures)
  save_to_release(df = xg_df, file_name = file_name, release_tag = "xg-data")
}

purrr::walk(2025:get_afl_season(),~get_xg_data(.))

# Get player stats data  -------------------------------------------------------------
get_player_stats <- function(season) {

  player_stats <- fitzRoy::fetch_player_stats_afl(season) %>%
    janitor::remove_constant() %>%
    janitor::clean_names()

  file_name <- glue::glue("player_stats_{season}")

  # return(futures)
  save_to_release(df = player_stats, file_name = file_name, release_tag = "player_stats-data")
}

purrr::walk(2025:get_afl_season(),~get_player_stats(.))

# Get fixtures data  -------------------------------------------------------------
get_fixtures <- function(season) {

  ### update fixtures file (17 secs)
  fixtures <- fitzRoy::fetch_fixture_afl(season, comp = "AFLM")

  file_name <- glue::glue("fixtures_{season}")

  # return(futures)
  save_to_release(df = fixtures, file_name = file_name, release_tag = "fixtures-data")
}

purrr::walk(2025:lubridate::year(Sys.Date()),~get_fixtures(.))

# Get team lineups data  -------------------------------------------------------------
get_teams <- function(season) {

  ### update teams file (90 secs per season)
  teams <- fitzRoy::fetch_lineup_afl(season, comp = "AFLM") %>%
    dplyr::mutate(
      season = as.numeric(substr(providerId, 5, 8)),
      row_id = paste0(providerId, teamId, player.playerId)
    ) # %>% dplyr::filter(!is.na(player.playerId))

  file_name <- glue::glue("teams_{season}")

  # return(futures)
  save_to_release(df = teams, file_name = file_name, release_tag = "teams-data")
}

purrr::walk(2025:get_afl_season(),~get_teams(.))

# Get results data  -------------------------------------------------------------
get_results <- function(season) {

  ##### update results file (5 secs per season)
  results <- fitzRoy::fetch_results_afl(season, comp = "AFLM")

  file_name <- glue::glue("results_{season}")

  # return(futures)
  save_to_release(df = results, file_name = file_name, release_tag = "results-data")
}

purrr::walk(2025:get_afl_season(),~get_results(.))

# Get player details data  -------------------------------------------------------------
get_player_details <- function(season) {

  ##### update players file (20 secs per season)
  player_details <-
    fitzRoy::fetch_player_details_afl(season = season, comp = "AFLM") %>%
    dplyr::mutate(
      player_name = paste(firstName,surname),
      age = lubridate::decimal_date(lubridate::as_date(glue::glue('{season}-07-01')))-
        lubridate::decimal_date(lubridate::as_date(dateOfBirth)),
      row_id = paste(providerId,season)
    )


  file_name <- glue::glue("player_details_{season}")

  # return(futures)
  save_to_release(df = player_details, file_name = file_name, release_tag = "player_details-data")
}

purrr::walk(2025:get_afl_season(),~get_player_details(.))

tictoc::toc()

