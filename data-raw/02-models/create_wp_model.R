######################## takes 3 mins to build model, 1 min for rest
# library(devtools)
# library('tidyverse')
# library('zoo')
# library('tidymodels')
# library('janitor')
# library('lubridate')
devtools::load_all()

# tictoc::tic()
# chains <- load_chains(T,T) #%>% janitor::clean_names()
# tictoc::toc()
#
# tictoc::tic()
# pbp <- clean_pbp(chains)
# tictoc::toc()
#
# tictoc::tic()
# model_data_epv <- clean_model_data_epv(pbp)
# tictoc::toc()
###########################
# devtools::load_all()

# model_data_wp <- readRDS("model_data_wp.rds")
tictoc::tic()
model_data_wp <- model_data_epv %>%
  add_epv_vars() %>%
  clean_model_data_wp() # %>% bind_rows(model_data_wp)
tictoc::toc()
# saveRDS(model_data_wp,"./data/model_data_wp.rds")



#######################################
##################
nrounds <- 100
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    tree_method = "hist",
    # early_stopping_rounds = 10,
    # num_class = 5,
    eta = 0.1, # 0.0685
    gamma = 0,
    # subsample=0.9,
    # colsample_bylevel=0.8,
    # colsample_bytree=0.9,
    monotone_constraints = "(0,0,0,1,1,1,0,1,0,0,0,0,0,0,0)",
    max_depth = 6,
    min_child_weight = 1
  )


###
full_train <- xgboost::xgb.DMatrix(
  stats::model.matrix(~ . + 0,
    data = model_data_wp %>% select_wp_model_vars()
  ),
  label = model_data_wp$label_wp
)

##################################################### BUILD THE MODEL
set.seed(1234)
wp_model <- xgboost::xgboost(
  params = params, data = full_train, nrounds = nrounds, print_every_n = 10
)

###
# folds <- splitTools::create_folds(
#   y = model_data_wp$match_id,
#   k = 5,
#   type = "grouped",
#   invert = TRUE
# )
#
# wp_cv_model <- xgboost::xgb.cv(
#   data = full_train,
#   label = model_data_wp$label_wp,
#   params = params,
#   # this doesn't matter with early stopping in xgb.cv, just set a big number
#   # the actual optimal rounds will be found in this tuning process
#   nrounds = 1000,
#   # created above
#   folds = folds,
#   metrics = list("logloss"),
#   early_stopping_rounds = 50,
#   print_every_n = 50
# )
###

# saveRDS(wp_model, "./data/wp_model.rds")

usethis::use_data(wp_model, overwrite = TRUE)

#########################################################
# # #####
# library(mgcv)
# wp_model_gam <- mgcv::bam(label_wp ~
#                       ti(goal_x,y, bs=c("ts","ts"),k=4)
#                     + s(goal_x, bs="ts",k=5) + s(y, bs="ts",k=5)
#                     + ti(total_seconds,xpoints_diff, bs=c("ts","ts"),k=4) +
#                     + ti(total_seconds,pos_lead_prob, bs=c("ts","ts"),k=4)
#                     + s(xpoints_diff, bs="ts",k=5) + s(pos_lead_prob, bs="ts",k=5) + s(points_diff, bs="ts",k=5)
#                     + s(diff_time_ratio, bs="ts",k=5)
#                     + s(home, bs="re")
#                     + s(play_type, bs="re") + s(phase_of_play, bs="re")
#                     ,
#                     data = model_data_wp, family = "binomial",
#                     nthreads = 4, select = T, discrete = T)
#
# # summary(wp_model_gam)
# ### usethis::use_data(wp_model_gam,overwrite = TRUE)
#
# # plot(mgcViz::getViz(wp_model_gam))
# ModelMetrics::logLoss(model_data_wp$label_wp,predict.bam(wp_model_gam,model_data_wp,type="response"))
# ModelMetrics::logLoss(model_data_wp$label_wp,model_data_wp %>% add_wp_vars() %>% pull(wp))
# ModelMetrics::logLoss(model_data_wp$label_wp,rep(0.5,nrow(model_data_wp)))

# ###########
# ### TESTING
# #######
match_choice <- "CD_M20220142601"

df <- # load_chains(2021, 27) %>% # row 280 is messed up
  # get_week_chains(2022,26) %>%
  chains %>% # select(-opp_goal,-opp_behind,-behind,-goal,-no_score)%>%
  filter(matchId == match_choice) %>%
  # filter(season == 2022, roundNumber == 26) %>%
  clean_pbp() %>%
  clean_model_data_epv() %>%
  add_epv_vars() %>%
  clean_model_data_wp() %>%
  # # janitor::clean_names() %>%
  # # clean_pbp() %>%
  # clean_model_data_epv() %>%
  # add_epv_vars() %>%
  # clean_model_data_wp() %>%
  add_wp_vars() %>%
  select(
    match_id,
    rn = display_order, chain = chain_number, period, secs = period_seconds, x, # x2,
    y, desc = description, jumper = jumper_number,
    player_id, player_name, team, team_id_mdl,
    lead_player, lead_team, delta_epv, pos_team,
    exp_pts, xpoints_diff, wp, wpa,
    opp_goal, opp_behind, behind, goal, no_score, player_position,
    goal_x, play_type, phase_of_play,
    kick_points, speed5, lag_goal_x5, throw_in, team_id, total_seconds,
    home, scoring_team_id, home_points, away_points, pos_points, opp_points, points_diff,
    points_row, points_shot
  )
#
# pbps <- chains  %>% filter(matchId == match_choice) %>% clean_pbp()
# chain <- chains %>% filter(matchId == match_choice)

#
#
# ###
# ###################
#####################
# library(EIX)
# library(pdp)
# dataX_train <- # xgboost::xgb.DMatrix(
#     model.matrix(~ . + 0, data = model_data_wp %>%
#                    select_wp_model_vars() %>%
#                    slice_head(n = 5000))
#
# xgboost::xgb.plot.shap(dataX_train, model = wp_model, top_n = 12, #target class 3 is goal
#                          n_col = 3)#, pch = 16, pch_NA = 17)
#
# wp_model %>% # the %>% operator is read as "and then"
#   partial(pred.var = "diff_time_ratio", train = dataX_train) %>%
#   plotPartial(smooth = FALSE)
#
# # xgboost::xgb.importance(model = wp_model)
# # importance_ints <- EIX::importance(wp_model, model_data_wp %>%
# #                                      select_wp_model_vars() %>%
# #                                      slice_sample(n = 2000), option = "interactions") %>% view()
