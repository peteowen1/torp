# devtools::install_github("jimmyday12/fitzRoy")
library(dplyr)
library(mgcv)
source('./R/scraper_functions.R')

## create function for writing chains
write_chains <- function(season,roundnum){
  chains_round <- get_match_chains(season, round = roundnum)
  saveRDS(chains_round, file = paste0("./data/chains_",season,"_",sprintf("%02d",roundnum),".rds"))
}

### map through 2021
purrr::map(1:30,~write_chains(2021,.))

### map through 2022
purrr::map(1:30,~write_chains(2022,.))

