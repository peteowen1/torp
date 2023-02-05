### set variables
season <- 2022
round <- 27

###
devtools::load_all()

### get week chains
chains <- get_week_chains(season,round)

### clean chains
pbp <- clean_pbp(chains)

### filter and then add epv and wp variables
model_data <- clean_model_data_epv(pbp) %>% add_epv_vars() %>% clean_model_data_wp() %>% add_wp_vars()
