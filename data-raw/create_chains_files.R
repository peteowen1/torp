# devtools::install_github("jimmyday12/fitzRoy")
library(dplyr)
library(mgcv)
source('./R/scraper_functions.R')
#devtools::load_all()

## create function for writing chains
write_chains <- function(season,roundnum){
  chains_round <- get_week_chains(season, roundnum)
  if(nrow(chains_round)>0){
  saveRDS(chains_round, file = paste0("./data/chains_",season,"_",sprintf("%02d",roundnum),".rds"))
  }
}

# ### map through 2021
# furrr::future_map(1:27,~write_chains(2021,.))

### map through 2022
furrr::future_map(24:27,~write_chains(2022,.))

