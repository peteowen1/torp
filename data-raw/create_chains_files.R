# devtools::install_github("jimmyday12/fitzRoy")
library(dplyr)
library(mgcv)
source('./R/scraper_functions.R')

get_week_chains <- function(season,roundnum) {
  #cache_message()

  load <- try(get_match_chains(season, round = roundnum), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to match chains from {.val {season}} round {.val {roundnum}}")
    return(data.table::data.table())
  }

  data.table::setDT(load)
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
furrr::future_map(1:30,~write_chains(2021,.))

### map through 2022
furrr::future_map(1:30,~write_chains(2022,.))

