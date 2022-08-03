# devtools::install_github("jimmyday12/fitzRoy")
library(dplyr)
library(mgcv)
source('./R/scraper_functions.R')
#devtools::load_all()

get_week_chains <- function(season,roundnum) {
  #cache_message()

  load <- try(get_match_chains(season, round = roundnum), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to match chains from {.val {season}} round {.val {roundnum}}")
    return(data.table::data.table())
  }

  load <- load %>% janitor::clean_names()
  if ("team_team_name" %in% colnames(load)) {
    load <-
      load %>%
      dplyr::rename(
        team = team_team_name,
        home_team = home_team_team_name,
        away_team = away_team_team_name,
        home_team_score = home_team_score_total_score,
        away_team_score = away_team_score_total_score,
        home_team_direction = home_team_direction_qtr1
      )
  }
  return(load)
}

## create function for writing chains
write_chains <- function(season,roundnum){
  chains_round <- get_week_chains(season, roundnum)
  if(nrow(chains_round)>0){
  saveRDS(chains_round, file = paste0("./data/chains_",season,"_",sprintf("%02d",roundnum),".rds"))
  }
}

### map through 2021
furrr::future_map(1:27,~write_chains(2021,.))

### map through 2022
furrr::future_map(1:27,~write_chains(2022,.))

