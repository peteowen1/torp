library(dplyr)
library(forcats)
library(mgcv)
devtools::load_all()

chains <- load_chains(T, T)
shots_prep <- load_pbp(seasons = T, rounds = T)

###############################
shots_all <- shots_prep %>% dplyr::filter(!is.na(points_shot) | !is.na(shot_at_goal))
shots <- shots_all %>% dplyr::filter(!is.na(shot_at_goal),
                                     x > 0,
                                     goal_x < 65,
                                     abs_y < 45
                                     )

################
# shots$scored_shot <- ifelse(!is.na(shots$points_shot), 1, 0)
# shots$shot_cat <- ceiling(tidyr::replace_na(shots$points_shot,0)/3)+1

shots <- shots %>%
  mutate(scored_shot = ifelse(!is.na(shots$points_shot), 1, 0),
         shot_cat = case_when(is.na(points_shot) ~ 1,
                              points_shot == 1 ~ 2,
                              points_shot == 6 ~ 3,
                              )
         )

shots$player_id_shot <- forcats::fct_lump_min(shots$player_id, 10, other_level = "Other")
player_name_mapping <- shots %>%
  dplyr::group_by(player_id_shot = player_id) %>%
  dplyr::summarise(player_name_shot = dplyr::last(player_name))

shot_player_df <-
  tibble::tibble(player_id_shot = shots$player_id_shot %>% levels()) %>%
  dplyr::left_join(player_name_mapping)

usethis::use_data(shot_player_df, overwrite = TRUE)

####################
# ###
# # shot result multinomial
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

################################
shot_on_target_mdl <-
  mgcv::bam(
    scored_shot ~ ti(goal_x, y, by = phase_of_play) + ti(goal_x, y)
      + s(goal_x, bs = "ts") + s(y, bs = "ts")
      + ti(lag_goal_x, y) + s(lag_goal_x, bs = "ts") + s(lag_y, bs = "ts")
      + s(play_type, bs = "re") + s(phase_of_play, bs = "re")
      + s(player_position_fac, bs = "re")
      + s(player_id_shot, bs = "re"),
    data = shots, family = stats::binomial(), nthreads = 4,
    select = TRUE, discrete = TRUE, drop.unused.levels = FALSE
  )

# summary(shot_on_target_mdl)
# mixedup::extract_ranef(shot_on_target_mdl) %>% tibble::view()
# plot(mgcViz::getViz(shot_on_target_mdl))

### save data
usethis::use_data(shot_on_target_mdl, overwrite = TRUE)

################### shot result binomialS
shot_result_mdl <-
  mgcv::bam(
    shot_result ~ ti(goal_x, y, by = phase_of_play) + ti(goal_x, y)
      + s(goal_x, bs = "ts") + s(y, bs = "ts")
      + ti(lag_goal_x, y) + s(lag_goal_x, bs = "ts") + s(lag_y, bs = "ts")
      + s(play_type, bs = "re") + s(phase_of_play, bs = "re")
      + s(player_position_fac, bs = "re")
      + s(player_id_shot, bs = "re"),
    data = shots %>% dplyr::filter(!is.na(shot_result)),
    family = stats::binomial(),
    nthreads = 4,
    select = TRUE, discrete = TRUE, drop.unused.levels = FALSE
  )

# summary(shot_result_mdl)
# mixedup::extract_ranef(shot_result_mdl) %>% tibble::view()
# plot(mgcViz::getViz(shot_result_mdl))

# ModelMetrics::logLoss(shots$shot_result,predict.bam(shot_result_mdl,shots,type="response"))

#####################
shot_ocat_mdl <-
  mgcv::bam(
    shot_cat ~
      ti(goal_x, abs_y, by = phase_of_play, bs = "ts")
    + ti(goal_x, abs_y, bs = "ts")
    + s(goal_x, bs = "ts")
    + s(abs_y, bs = "ts")
    + ti(lag_goal_x, abs_y) + s(lag_goal_x, bs = "ts") + s(lag_y, bs = "ts")
    + s(play_type, bs = "re") + s(phase_of_play, bs = "re")
    + s(player_position_fac, bs = "re")
    + s(player_id_shot, bs = "re")
    ,
    data = shots,
    family = ocat(R = 3) ,#stats::binomial(),
    nthreads = 4,
    select = TRUE, discrete = TRUE, drop.unused.levels = FALSE
  )

### save data
usethis::use_data(shot_ocat_mdl, overwrite = TRUE)

####
player_shot_on_target_score <- mixedup::extract_ranef(shot_on_target_mdl) %>%
  dplyr::filter(group_var == "player_id_shot") %>%
  dplyr::left_join(shot_player_df, by = c("group" = "player_id_shot")) %>%
  dplyr::arrange(-value) # %>% tibble::view()

usethis::use_data(player_shot_on_target_score, overwrite = TRUE)

####
player_shot_result_score <- mixedup::extract_ranef(shot_result_mdl) %>%
  dplyr::filter(group_var == "player_id_shot") %>%
  dplyr::left_join(shot_player_df, by = c("group" = "player_id_shot")) %>%
  dplyr::arrange(-value) # %>% tibble::view()

usethis::use_data(player_shot_result_score, overwrite = TRUE)

####
player_shot_score <- mixedup::extract_ranef(shot_ocat_mdl) %>%
  dplyr::filter(group_var == "player_id_shot") %>%
  dplyr::left_join(shot_player_df, by = c("group" = "player_id_shot")) %>%
  dplyr::arrange(-value) # %>% tibble::view()

usethis::use_data(player_shot_score, overwrite = TRUE)

###
preds <- predict.bam(shot_ocat_mdl,shots,type = 'response')
colnames(preds) <- c('pred_no_score','pred_behind','pred_goal')
shots <- bind_cols(shots,preds)
