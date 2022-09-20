######################## takes 3 mins to build model, 1 min for rest
# library(devtools)
library('tidyverse')
library('zoo')
library('tidymodels')
library('janitor')
library('lubridate')
devtools::load_all()
# source("./R/load_chains.R")
# source("./R/helper_functions.R")
# source("./R/clean_model_data.R")
# source("./R/add_model_variables.R")

chains <- load_chains(2021:lubridate::year(Sys.Date())) %>%
  janitor::clean_names()

pbp <- clean_pbp(chains)

###########################
devtools::load_all()

model_data_wp <- readRDS("model_data_wp.rds")
model_data_wp <- model_data_epv %>% add_epv_vars() %>% clean_model_data_wp() %>% bind_rows(model_data_wp)
saveRDS(model_data_wp,"model_data_wp.rds")

#######################################
##################
nrounds = 75
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    tree_method = "hist",
    #early_stopping_rounds = 10,
    #num_class = 5,
    eta = 0.1, #0.0685
    gamma = 0,
    #subsample=0.9,
    #colsample_bylevel=0.8,
    #colsample_bytree=0.9,
    monotone_constraints = "(0,0,0,1,1,1,0,1,0,0,0,0,0,0,0)",
    max_depth = 6,
    min_child_weight = 1
  )

###
full_train <- xgboost::xgb.DMatrix(stats::model.matrix(~ . + 0,
                                                       data = model_data_wp %>% select_wp_model_vars()),
                                   label = model_data_wp$label_wp
)

##################################################### BUILD THE MODEL
set.seed(1234)
wp_model <- xgboost::xgboost(
  params = params, data = full_train, nrounds = nrounds, print_every_n = 10
)

#saveRDS(wp_model, "./data/wp_model.rds")
usethis::use_data(wp_model,overwrite = TRUE)
# #####
# library(mgcv)
# wp_model_gam <- mgcv::bam(label_wp ~
#                       ti(goal_x,y, bs=c("ts","ts"),k=4)
#                     + s(goal_x, bs="ts",k=5) + s(y, bs="ts",k=5)
#                     + ti(total_seconds,xpoints_diff, bs=c("ts","ts"),k=4) +
#                     + ti(total_seconds,pos_lead_prob, bs=c("ts","ts"),k=4)
#                     + s(xpoints_diff, bs="ts",k=5) + s(pos_lead_prob, bs="ts",k=5) + s(points_diff, bs="ts",k=5)
#                     + s(diff_time_ratio, bs="ts",k=5)
#                     + s(home, bs="re")
#                     + s(play_type, bs="re") +  + s(phase_of_play, bs="re")
#                     ,
#                     data = model_data_wp, family = "binomial"  , nthreads = 4 ,select = T, discrete = T)
#
# # summary(wp_model_gam)
# usethis::use_data(wp_model_gam,overwrite = TRUE)
#
# # plot(mgcViz::getViz(wp_model_gam))
# ModelMetrics::logLoss(model_data_wp$label_wp,predict.bam(wp_model_gam,model_data_wp,type="response"))
# ###########
# ### TESTING
# #######
df <- #load_chains(2021, 27) %>%
  #get_week_chains(2022,26) %>%
  model_data_wp %>% #select(-opp_goal,-opp_behind,-behind,-goal,-no_score)%>%
  filter(match_id == "CD_M20220142602") %>%
  # # janitor::clean_names() %>%
  # # clean_pbp() %>%
  # clean_model_data_epv() %>%
  # add_epv_vars() %>%
  # clean_model_data_wp() %>%
  # add_wp_vars() #%>%
  select(
    rn = display_order, chain = chain_number, period, secs = period_seconds, x,#x2,
    y, desc = description, jumper = jumper_number,
    player_id, player_name, team, team_id_mdl,
    lead_player, lead_team, delta_epv, pos_team,
    exp_pts,xpoints_diff,wp,wpa, opp_goal, opp_behind, behind, goal, no_score, player_position,
    goal_x,play_type,phase_of_play,
    kick_points,speed5,lag_goal_x5,throw_in,team_id
  )

#
#
# ###
# ###################
# #####################
# library(EIX)
# library(pdp)
# dataX_train <- # xgboost::xgb.DMatrix(
#     model.matrix(~ . + 0, data = model_data_wp %>%
#                    select_wp_model_vars() %>%
#                    slice_head(n = 5000))
#
# # xgboost::xgb.plot.shap(dataX_train, model = wp_model, top_n = 12, #target class 3 is goal
# #                          n_col = 3)#, pch = 16, pch_NA = 17)
#
# wp_model %>% # the %>% operator is read as "and then"
#   partial(pred.var = "diff_time_ratio", train = dataX_train) %>%
#   plotPartial(smooth = FALSE)
#
# # xgboost::xgb.importance(model = wp_model)
# # importance_ints <- EIX::importance(wp_model, model_data_wp %>%
# #                                      select_wp_model_vars() %>%
# #                                      slice_sample(n = 2000), option = "interactions") %>% view()
