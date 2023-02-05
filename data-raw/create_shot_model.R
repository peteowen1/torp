library(dplyr)
library(forcats)
library(mgcv)
source("./R/load_chains.R")
source("./R/helper_functions.R")
source("./R/match_xg_functions.R")

if (exists("chains") == FALSE) {
chains <- load_chains(2021:lubridate::year(Sys.Date()))
}

# chains <- bind_rows(chains_2021,chains_2022) %>% janitor::clean_names()

shots_prep <- chains %>% # filter(match_id == "CD_M20210142701") %>%
  clean_pbp() %>%
  clean_shots_data() %>%
  clean_model_data_epv()

shots <- shots_prep %>% filter(!is.na(points_shot))

################
shots$player_name_shot <- forcats::fct_lump_min(shots$player_name, 10)

shot_player_df <- tibble::tibble(player_name_shot = shots$player_name_shot %>% levels())
usethis::use_data(shot_player_df, overwrite = TRUE)

####################
# ###
# # shot result multi
# shot_result_mdl <-
#   gam(
#     list(
#       shot_result ~ s(goal_x, y, by = phase_of_play) + s(goal_x, y)
#         + s(goal_x, bs = "ts") + s(y, bs = "ts")
#         # + ti(lag_goal_x, y) + s(lag_goal_x, bs = "ts") + s(lag_y, bs = "ts")
#         # + s(play_type, bs = "re") + s(phase_of_play, bs = "re")
#         # + s(player_position_fac, bs = "re")
#       ,
#        ~ s(goal_x, y, by = phase_of_play) + s(goal_x, y)
#       + s(goal_x, bs = "ts") + s(y, bs = "ts")
#       # + ti(lag_goal_x, y) + s(lag_goal_x, bs = "ts") + s(lag_y, bs = "ts")
#       # + s(play_type, bs = "re") + s(phase_of_play, bs = "re")
#       # + s(player_position_fac, bs = "re")
#     ),
#       data = shots, family = multinom(K=2) #, nthreads = 4, select = T, discrete = T
#   )

###################
shot_result_mdl <-
  mgcv::bam(
    shot_result ~ ti(goal_x, y, by = phase_of_play) + ti(goal_x, y)
      + s(goal_x, bs = "ts") + s(y, bs = "ts")
      + ti(lag_goal_x, y) + s(lag_goal_x, bs = "ts") + s(lag_y, bs = "ts")
      + s(play_type, bs = "re") + s(phase_of_play, bs = "re")
      + s(player_position_fac, bs = "re")
      + s(player_name_shot, bs = "re"),
    data = shots, family = stats::binomial(), nthreads = 4,
    select = TRUE, discrete = TRUE, drop.unused.levels = FALSE
  )


# ModelMetrics::logLoss(shots$shot_result,predict.bam(shot_result_mdl,shots,type="response"))
### save data
# usethis::use_data(shot_result_mdl, overwrite = TRUE)
####
player_shot_score <- mixedup::extract_ranef(shot_result_mdl) %>%
  dplyr::filter(group_var == "player_name_shot") %>%
  dplyr::arrange(-value) # %>% tibble::view()

# usethis::use_data(player_shot_score, overwrite = TRUE)

# shots$pred <- predict.bam(shot_result_mdl,shots,type="response")
