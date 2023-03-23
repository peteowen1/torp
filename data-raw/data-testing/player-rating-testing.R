###############
this_week <- plyr_ratings(2023,1) %>%
  dplyr::left_join(fitzRoy::fetch_player_details(),by = c("player.playerId" = "providerId"))

season_table <- plyr_gm_df %>%
  dplyr::filter(season==2023, round >= 1, round <= 28) %>%
  dplyr::group_by(plyr_nm,player_id,tm,pos) %>%
  dplyr::summarise(tot_points = sum(tot_p_adj), g = dplyr::n(),p_g = tot_points/g) %>%
  dplyr::left_join(fitzRoy::fetch_player_details(),by = c("player_id" = "providerId")) %>%
  #dplyr::filter(year(dateOfBirth)>=2001) %>%
  dplyr::arrange(-tot_points) #%>% view()

season_table %>% tibble::view()

# final_22 %>% filter(round.roundNumber==2) %>%
#   left_join(final_22 %>% filter(round.roundNumber==23),by = c('player.playerId'='player.playerId')) %>%
#   mutate(delta = bayes_g.y - bayes_g.x) %>%
#   relocate(delta,player_name.x,bayes_g.y,gms.x,gms.y) %>% view()



##############################
# tictoc::tic()
# results <- fetch_results(season = 2021, comp = "AFLM") %>%
#   bind_rows(fetch_results(season = 2022, comp = "AFLM")) %>%
#   mutate(season = as.numeric(substr(match.matchId, 5, 8)))
# tictoc::toc()
#
# final <- tibble()

### 2021 - OLD DONT UNCOMMENT
# for (i in 2:28) {
#   plyr_df <- plyr_ratings(afl_total,2021,i)
#   teams2 <- teams %>%
#     filter(season == 2021, round.roundNumber == i) %>%
#     left_join(plyr_df, by = c("player.playerId" = "player_id"))
#
#   final <- bind_rows(final, teams2)
# }
#
# saveRDS(final, "plyr_df_2021.rds")
# ############ 2021  UNCOMMENT THIS IF YOU CHANGE THE PLAYER_RATINGS FUNCTION AS ANY POINT!!!
# tictoc::tic()
# final_21 <- furrr::future_map_dfr(2:28, ~ plyr_ratings(plyr_gm_df, teams, 2021, .))
# tictoc::toc()
# saveRDS(final_21, "plyr_df_2021.rds")
# final_21 <- readRDS("plyr_df_2021.rds")
#
# ### 2022
# n <- max(teams %>% filter(season == lubridate::year(Sys.Date()), utcStartTime < Sys.time() + 350000) %>%
#            select(round.roundNumber)) + 1
#
# ### 2022
# tictoc::tic()
# final_22 <- furrr::future_map_dfr(1:n, ~ plyr_ratings(plyr_gm_df, teams, 2022, .))
# tictoc::toc()
# saveRDS(final_22, "plyr_df_2022.rds")
# final_22 <- readRDS("plyr_df_2022.rds")
#
# ### final
# final <- bind_rows(final_21, final_22)
#
# final$weight <- exp(as.numeric(-(Sys.Date() - as.Date(final$utcStartTime))) / 250)
# final$weight <- final$weight / mean(final$weight, na.rm = T)
# #############
#
# team_rt_df <-
#   teams %>%
#   left_join(final %>% filter(player_name != "NA NA"), by = c('providerId'='providerId',"player.playerId"="player.playerId")) %>%
#   filter((!is.na(bayes_g) | utcStartTime.x > Sys.time())) %>% #view()
#   ### filter(player_name != "NA NA" | is.na(player_name)) %>%
#   # filter(providerId > "CD_M20210140509") %>%
#   filter((position.x != "EMERG" & position.x != "SUB") | is.na(position.x)) %>%
#   mutate(
#     phase = case_when(
#       position.x %in% c("BPL", "BPR", "FB", "CHB", "HBFL", "HBFR") ~ "def",
#       position.x %in% c("C", "WL", "WR", "R", "RR", "RK") ~ "mid",
#       position.x %in% c("FPL", "FPR", "FF", "CHF", "HFFL", "HFFR") ~ "fwd",
#       position.x %in% c("INT", "SUB") ~ "int",
#       TRUE ~ "other",
#     ),
#     def = ifelse(phase == "def", bayes_g, NA),
#     mid = ifelse(phase == "mid", bayes_g, NA),
#     fwd = ifelse(phase == "fwd", bayes_g, NA),
#     int = ifelse(phase == "int", bayes_g, NA),
#     key_def = ifelse(posn == "KEY_DEFENDER", bayes_g, NA),
#     med_def = ifelse(posn == "MEDIUM_DEFENDER", bayes_g, NA),
#     midfield = ifelse(posn == "MIDFIELDER", bayes_g, NA),
#     mid_fwd = ifelse(posn == "MIDFIELDER_FORWARD", bayes_g, NA),
#     med_fwd = ifelse(posn == "MEDIUM_FORWARD", bayes_g, NA),
#     key_fwd = ifelse(posn == "KEY_FORWARD", bayes_g, NA),
#     rucks = ifelse(posn == "RUCK", bayes_g, NA),
#     other_pos = ifelse(is.na(posn), bayes_g, NA)
#   ) %>% # view()
#   #mutate(season = lubridate::year(utcStartTime)) %>%
#   group_by(providerId, teamName.x, season, round.roundNumber.x, teamType.x) %>%
#   summarise(
#     bayes_g = sum(bayes_g), # , na.rm = T),
#     bayes_recv_g = sum(bayes_recv_g), # , na.rm = T),
#     bayes_disp_g = sum(bayes_disp_g), # , na.rm = T),
#     def = sum(def, na.rm = T),
#     mid = sum(mid, na.rm = T),
#     fwd = sum(fwd, na.rm = T),
#     int = sum(int, na.rm = T),
#     key_def = mean(key_def, na.rm = T),
#     med_def = mean(med_def, na.rm = T),
#     midfield = mean(midfield, na.rm = T),
#     mid_fwd = mean(mid_fwd, na.rm = T),
#     med_fwd = mean(med_fwd, na.rm = T),
#     key_fwd = mean(key_fwd, na.rm = T),
#     rucks = mean(rucks, na.rm = T),
#     other_pos = mean(other_pos, na.rm = T),
#     count = n()
#   ) %>%
#   ungroup() %>%
#   group_by(teamName.x) %>%
#   fill(bayes_g, bayes_recv_g, bayes_disp_g) %>%
#   mutate(
#     def = ifelse(def == 0, lag(def), def),
#     mid = ifelse(mid == 0, lag(mid), mid),
#     fwd = ifelse(fwd == 0, lag(fwd), fwd),
#     int = ifelse(int == 0, lag(int), int)
#   ) %>%
#   fill(def, mid, fwd, int) %>%
#   ungroup()
#
# ####
# team_mdl_df <- team_rt_df %>% # filter(!is.na(bayes_g)) %>%
#   left_join(team_rt_df %>% mutate(typ2 = ifelse(teamType.x == "home", "away", "home")),
#             by = c("providerId" = "providerId", "teamType.x" = "typ2")
#   ) %>%
#   mutate(
#     bayes_g_diff = bayes_g.x - bayes_g.y,
#     bayes_g_ratio = log(bayes_g.x / bayes_g.y),
#     bayes_recv_g_diff = bayes_recv_g.x - bayes_recv_g.y,
#     bayes_disp_g_diff = bayes_disp_g.x - bayes_disp_g.y
#   ) %>%
#   # filter(teamType == "home") %>%
#   left_join(results %>%
#               select(
#                 match.matchId,
#                 homeTeamScore.matchScore.totalScore, homeTeamScore.matchScore.goals, homeTeamScore.matchScore.behinds,
#                 awayTeamScore.matchScore.totalScore, awayTeamScore.matchScore.goals, awayTeamScore.matchScore.behinds,
#                 match.utcStartTime
#               ),
#             by = c("providerId" = "match.matchId")
#   ) %>%
#   mutate(
#     home_shots = homeTeamScore.matchScore.goals + homeTeamScore.matchScore.behinds,
#     away_shots = awayTeamScore.matchScore.goals + awayTeamScore.matchScore.behinds,
#     score_diff = ifelse(teamType.x == "home",
#                         homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore,
#                         awayTeamScore.matchScore.totalScore - homeTeamScore.matchScore.totalScore
#     ),
#     shot_diff = ifelse(teamType.x == "home",
#                        home_shots - away_shots,
#                        away_shots - home_shots
#     ),
#     team_shots = ifelse(teamType.x == "home",
#                         home_shots,
#                         away_shots
#     ),
#     shot_conv = ifelse(teamType.x == "home",
#                        homeTeamScore.matchScore.goals / home_shots,
#                        awayTeamScore.matchScore.goals / away_shots
#     ),
#     win = ifelse(score_diff > 0, 1, ifelse(score_diff == 0, 0.5, 0)),
#     hoff_adef = fwd.x - def.y,
#     hmid_amid = mid.x - mid.y,
#     hdef_afwd = def.x - fwd.y,
#     hint_aint = pmax(pmin((int.x - int.y),10),-10),
#     team_type_fac = as.factor(teamType.x),
#     total_score = homeTeamScore.matchScore.totalScore + awayTeamScore.matchScore.totalScore,
#     total_shots = home_shots + away_shots,
#     team_name.x = as.factor(teamName.x.x),
#     team_name.y = as.factor(teamName.x.y),
#     weightz = exp(as.numeric(-(Sys.Date() - as.Date(match.utcStartTime))) / 250),
#     weightz = weightz / mean(weightz, na.rm = T)
#   )
#
#
# #### MODEL
# library(mgcViz)
#
# ###
# afl_shot_mdl <- bam(score_diff ~
#                       s(team_type_fac, bs = "re")
#                     # + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
#                     # + ti(bayes_g_diff, pred_totshots, bs = c("ts", "ts"), k = 5)
#                     # + s(pred_totshots, bs = "ts")
#                     + s(bayes_g_diff, bs = "ts")
#                     # + s(bayes_recv_g_diff, bs = "ts")
#                     # + s(bayes_disp_g_diff, bs = "ts")
#                     # + s(hoff_adef, bs = "ts") + s(hmid_amid, bs = "ts") + s(hdef_afwd, bs = "ts") + s(hint_aint, bs = "ts")
#                     + s(key_def.x, bs = "ts", k = 5) + s(med_def.x, bs = "ts", k = 5) + s(midfield.x, bs = "ts", k = 5) #+ s(mid_fwd.x, bs = "ts", k = 5)
#                     + s(med_fwd.x, bs = "ts", k = 5) + s(key_fwd.x, bs = "ts", k = 5) + s(rucks.x, bs = "ts", k = 5) #+ s(other_pos.x, bs = "ts", k = 5)
#                     + s(key_def.y, bs = "ts", k = 5) + s(med_def.y, bs = "ts", k = 5) + s(midfield.y, bs = "ts", k = 5) #+ s(mid_fwd.y, bs = "ts", k = 5)
#                     + s(med_fwd.y, bs = "ts", k = 5) + s(key_fwd.y, bs = "ts", k = 5) + s(rucks.y, bs = "ts", k = 5) #+ s(other_pos.y, bs = "ts", k = 5)
#                     # + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
#                     # + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
#                     ,
#                     data = team_mdl_df, #weights = weightz,
#                     family = gaussian(), nthreads = 4, select = T, discrete = T
# )
# team_mdl_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_mdl_df, type = "response")
#
# summary(afl_shot_mdl)
# summary(team_mdl_df$pred_shot_diff)
# # plot(mgcViz::getViz(afl_shot_mdl))
# # mixedup::extract_ranef(afl_shot_mdl) %>% view()
# # Deviance explained = 44.5%
#
# ###
