######################## takes 3 mins to build model, 1 min for rest
# library(devtools)
library('tidyverse')
library('zoo')
library('tidymodels')
library('janitor')
library('lubridate')
# devtools::load_all()
source('./R/scraper_functions.R')
source("./R/load_chains.R")
source("./R/helper_functions.R")
source("./R/clean_model_data.R")
source("./R/add_model_variables.R")

chains <- load_chains(2021:lubridate::year(Sys.Date())) #%>%
  # bind_rows(get_week_chains(2022, 26)) %>%
  # bind_rows(get_week_chains(2022, 25))

pbp <- clean_pbp(chains)

model_data_epv <- clean_model_data_epv(pbp)
#######################################
##################
nrounds <- 87
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    tree_method = "hist",
    # early_stopping_rounds = 10,
    num_class = 5,
    eta = 0.15, # 0.0685
    gamma = 0,
    subsample = 0.85,
    # monotone_constraints = "(-1,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1)",
    # colsample_bylevel=0.8,
    colsample_bytree = 0.85,
    max_depth = 6,
    min_child_weight = 25
  )

###
full_train <- xgboost::xgb.DMatrix(stats::model.matrix(~ . + 0,
                                                data = model_data_epv %>% select_epv_model_vars()),
                                                label = model_data_epv$label_ep
)

##################################################### UNCOMMENT TO REBUILD THE MODEL
set.seed(1234)
ep_model <- xgboost::xgboost(
  params = params, data = full_train, nrounds = nrounds, print_every_n = 10
)

#saveRDS(ep_model, "./data/ep_model.rds")
usethis::use_data(ep_model,overwrite = TRUE)
###########
### TESTING
#######
df <- load_chains(2022, 23) %>% filter(match_id == "CD_M20220142309") %>%
  clean_pbp() %>%
  clean_model_data_epv() %>%
  add_epv_vars() %>%
  select(
    rn = display_order, chain = chain_number, period, secs = period_seconds, x, y, desc = description, jumper = jumper_number,
    player_id, player_name, team, team_id_mdl,
    lead_player, lead_team, delta_epv, pos_team, exp_pts,lead_points, opp_goal, opp_behind, behind, goal, no_score, player_position,
    goal_x,play_type,phase_of_play,lead_desc, points_shot, kick_points
  )

df2 <- load_chains(2022, 23) %>% filter(match_id == "CD_M20220142309") %>%
  clean_pbp() %>%
  # clean_model_data() %>%
  # add_epv_vars() %>%
  select(
    rn = display_order, chain = chain_number, period, secs = period_seconds, x, y, desc = description, jumper = jumper_number,
    player_id, player_name, team, team_id_mdl,
    #, lead_team, delta_epv, pos_team, exp_pts, Opp_Goal, Opp_Behind, Behind, Goal, No_Score, player_position,
    goal_x,play_type,phase_of_play,lead_desc_tot, points_shot,points_row_na,tot_goals,throw_in
  )
###################

