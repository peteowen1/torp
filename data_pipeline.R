### set variables
# season <- 2022
# round <- 27

###
devtools::load_all()

load_week_model_data <- function(season, round) {
  ### get week chains (0.5 secs per game)
  chains <- load_week_chain_data(season, round)

  ### clean chains (1.5 secs per game)
  pbp <- clean_pbp(chains)

  ### filter and prep data for epv model (7.5 secs per game)
  model_data_epv <- clean_model_data_epv(pbp)

  ### add epv and wp variables  (0.5 secs per game)
  model_data_wp <- model_data_epv %>%
    add_epv_vars() %>%
    clean_model_data_wp() %>%
    add_wp_vars()

  saveRDS(model_data_wp, glue::glue("./pbp-data/pbp_data_{season}_{round}.rds"))

  return(model_data_wp)
}

load_week_chain_data <- function(season, round) {
  chains <- get_week_chains(season, round)

  return(chains)
}

walk(1:27, ~ load_week_chain_data(2022, .))

# walk(1:27, ~ load_week_model_data(2022, .))
