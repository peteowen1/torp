##### BASIC TESTING
##### add to match pred data frame
###
# where does pred_df come from?

pred_df <- readRDS("./data-raw/stat_pred_df.rds")

plyr_mdl_df <-
  pred_df %>%
  left_join(
    results %>%
      dplyr::select(
        match.matchId,
        homeTeamScore.matchScore.totalScore, homeTeamScore.matchScore.goals, homeTeamScore.matchScore.behinds,
        awayTeamScore.matchScore.totalScore, awayTeamScore.matchScore.goals, awayTeamScore.matchScore.behinds,
        match.utcStartTime
      ),
    by = c("provider_id" = "match.matchId")
  ) %>%
  dplyr::mutate(
    home_shots = homeTeamScore.matchScore.goals + homeTeamScore.matchScore.behinds,
    away_shots = awayTeamScore.matchScore.goals + awayTeamScore.matchScore.behinds,
    score_diff = ifelse(home_away == "home",
      homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore,
      awayTeamScore.matchScore.totalScore - homeTeamScore.matchScore.totalScore
    ),
    shot_diff = ifelse(home_away == "home",
      home_shots - away_shots,
      away_shots - home_shots
    ),
    team_shots = ifelse(home_away == "home",
      home_shots,
      away_shots
    ),
    shot_conv = ifelse(home_away == "home",
      homeTeamScore.matchScore.goals / home_shots,
      awayTeamScore.matchScore.goals / away_shots
    ),
    win = ifelse(score_diff > 0, 1, ifelse(score_diff == 0, 0.5, 0)),
    ####
    team_type_fac = as.factor(home_away),
    total_score = homeTeamScore.matchScore.totalScore + awayTeamScore.matchScore.totalScore,
    total_shots = home_shots + away_shots,
    team_name.x = as.factor(team_name),
    team_name.y = as.factor(opp_name),
    weightz = exp(as.numeric(-(Sys.Date() - as.Date(match.utcStartTime))) / decay),
    weightz = weightz / mean(weightz, na.rm = T),
    row_id = paste0(player_id, substr(provider_id, 5, 8), round),
    adj_score_diff = score_diff / 18
  ) %>%
  left_join(
    torp_df_total,
    by = c("row_id" = "row_id")
  ) %>%
  mutate(
    torp = replace_na(torp,0)
  ) %>%
  filter(!is.na(pred_goals))


#
# plyr_mdl_df %>%
#   group_by(player_position) %>%
#   select(starts_with('pred')) %>%
#   summarise(across(everything(), list(mean = ~mean(., na.rm=T), sd = ~sd(., na.rm=T)))) %>%
#   view()

##########################################
plyr_torp_diff_mdl <- mgcv::bam(
  score_diff ~
    team_type_fac
    + offset(torp)
    + scale(pred_goals) # , bs = "ts")
    + scale((pred_behinds)) # , bs = "ts")
    # + scale((pred_kicks)) # , bs = "ts")
    # + scale((pred_handballs)) # , bs = "ts") # +
    # #+ s((pred_disposals), bs = "ts") # +
    + scale((pred_marks)) # , bs = "ts")
    + scale((pred_bounces)) # , bs = "ts") # * -
    + scale((pred_tackles)) # , bs = "ts") # +
    + scale((pred_contested_possessions))#, bs = "ts") # ** +
    + scale((pred_uncontested_possessions))#, bs = "ts") # ** +
    #+Is((pred_total_possessions), bs = "ts")
    # + scale((pred_inside50s))#, bs = "ts") # *** +
    # + scale((pred_marks_inside50))#, bs = "ts")
    # + scale((pred_contested_marks))#, bs = "ts")
    + scale((pred_hitouts)) # , bs = "ts")
    # ### + s(scale(pred_one_percenters), bs='ts') # **
    # + scale((pred_clangers))#, bs = "ts")
    + scale((pred_frees_for)) # , bs = "ts") # ** -
    + scale((pred_frees_against)) # , bs = "ts") # * +
    + scale((pred_rebound50s)) # , bs = "ts")
    # #+ scale((pred_goal_assists))#, bs = "ts") # * +
    + scale((pred_turnovers)) # , bs = "ts") # * -
    # + scale((pred_intercepts))#, bs = "ts") # +
    # + scale((pred_tackles_inside50))#, bs = "ts")
    ### + s((pred_shots_at_goal), bs = "ts")
    # ### + s(scale(pred_score_involvements), bs='ts') # **
    # + s((pred_clearances_centre_clearances), bs = "ts") # * -
    # + s((pred_clearances_stoppage_clearances), bs = "ts")
    # + s((pred_clearances_total_clearances), bs = "ts")
    # + s((pred_extended_stats_effective_kicks), bs = "ts")
    # #+ s(scale(pred_extended_stats_kick_to_handball_ratio), bs='ts')
    # + s((pred_extended_stats_effective_disposals), bs = "ts")
    # + s((pred_extended_stats_marks_on_lead), bs = "ts")
    # + s((pred_extended_stats_intercept_marks), bs = "ts")
    # + s((pred_extended_stats_hitouts_to_advantage), bs = "ts") # +
    # + s((pred_extended_stats_ground_ball_gets), bs = "ts")
    # + s((pred_extended_stats_f50ground_ball_gets), bs = "ts")
    # ### + s(scale(pred_extended_stats_score_launches), bs='ts') # .
    # + s((pred_extended_stats_pressure_acts), bs = "ts")
    + scale((pred_extended_stats_def_half_pressure_acts)) # *** -
    # + s((pred_extended_stats_spoils), bs = "ts") # * +
    # + s((pred_extended_stats_ruck_contests), bs = "ts")
    # + s((pred_extended_stats_contest_def_one_on_ones), bs = "ts")
    # + s((pred_extended_stats_contest_def_losses), bs = "ts")
    # + s((pred_extended_stats_contest_off_one_on_ones), bs = "ts")
    # + s((pred_extended_stats_contest_off_wins), bs = "ts")
    # #### + s(scale(pred_extended_stats_centre_bounce_attendances), bs='ts')
    # #### + s(scale(pred_extended_stats_kickins), bs='ts')
    # #### + s(scale(pred_extended_stats_kickins_playon), bs='ts')
    # ############# BINOM
    # + s((pred_time_on_ground_percentage), bs = "ts")
    # + s((pred_disposal_efficiency), bs = "ts")
    # + s((pred_goal_accuracy), bs = "ts")
    # + s((pred_extended_stats_kick_efficiency), bs = "ts")
    # + s((pred_extended_stats_contested_possession_rate), bs = "ts")
    # + s((pred_extended_stats_hitout_win_percentage), bs = "ts")
    + scale(pred_extended_stats_hitout_to_advantage_rate)
  # + s((pred_extended_stats_contest_def_loss_percentage), bs = "ts")
  # + s((pred_extended_stats_contest_off_wins_percentage), bs = "ts")
  ,
  data = plyr_mdl_df %>% dplyr::filter(season > 2021),
  select = T, discrete = T, nthreads = 4,
  weights = time_on_ground_percentage,
  family = "gaussian"
)


summary(plyr_torp_diff_mdl)

###
# plot(mgcViz::getViz(plyr_torp_diff_mdl))
####
plyr_mdl_df %>%
  select(
    player_name.x,
    player_position,
    provider_id,
    team_name,
    pred_one_percenters,
    pred_extended_stats_spoils,
    pred_handballs, # good
    pred_extended_stats_effective_disposals, # nosey
    pred_bounces, # bad
    pred_extended_stats_def_half_pressure_acts, # bad
    pred_extended_stats_pressure_acts, # bad
    pred_clearances_centre_clearances, # badish
    pred_clearances_stoppage_clearances, # goodish
    pred_inside50s, # good
    pred_hitouts, # bad
    pred_disposal_efficiency, # good
    pred_extended_stats_hitout_to_advantage_rate, # good
  ) %>%
  view()
####################################################
###########################
# afl_torp_diff_mdl <- mgcv::bam(
#   score_diff ~
#     team_type_fac
#     + offset(torp_diff)
#     + s(scale(pred_goals), bs = "ts")
#     + s(scale(pred_behinds), bs = "ts")
#     + s(scale(pred_kicks), bs = "ts")
#     + s(scale(pred_handballs), bs = "ts") # +
#     + s(scale(pred_disposals), bs = "ts") # +
#     + s(scale(pred_marks), bs = "ts")
#     + s(scale(pred_bounces), bs = "ts") # * -
#     + s(scale(pred_tackles), bs = "ts") # +
#     + s(scale(pred_contested_possessions), bs = "ts") # ** +
#     + s(scale(pred_uncontested_possessions), bs = "ts") # ** +
#     + s(scale(pred_total_possessions), bs = "ts")
#     + s(scale(pred_inside50s), bs = "ts") # *** +
#     + s(scale(pred_marks_inside50), bs = "ts")
#     + s(scale(pred_contested_marks), bs = "ts")
#     + s(scale(pred_hitouts), bs = "ts")
#     ### + s(scale(pred_one_percenters), bs='ts') # **
#     + s(scale(pred_clangers), bs = "ts")
#     + s(scale(pred_frees_for), bs = "ts") # ** -
#     + s(scale(pred_frees_against), bs = "ts") # * +
#     + s(scale(pred_rebound50s), bs = "ts")
#     + s(scale(pred_goal_assists), bs = "ts") # * +
#     + s(scale(pred_turnovers), bs = "ts") # * -
#     + s(scale(pred_intercepts), bs = "ts") # +
#     + s(scale(pred_tackles_inside50), bs = "ts")
#     + s(scale(pred_shots_at_goal), bs = "ts")
#     ### + s(scale(pred_score_involvements), bs='ts') # **
#     + s(scale(pred_clearances_centre_clearances), bs = "ts") # * -
#     + s(scale(pred_clearances_stoppage_clearances), bs = "ts")
#     # + s(scale(pred_clearances_total_clearances), bs = "ts")
#     + s(scale(pred_extended_stats_effective_kicks), bs = "ts")
#     #+ s(scale(pred_extended_stats_kick_to_handball_ratio), bs='ts')
#     + s(scale(pred_extended_stats_effective_disposals), bs = "ts")
#     + s(scale(pred_extended_stats_marks_on_lead), bs = "ts")
#     + s(scale(pred_extended_stats_intercept_marks), bs = "ts")
#     + s(scale(pred_extended_stats_hitouts_to_advantage), bs = "ts") # +
#     + s(scale(pred_extended_stats_ground_ball_gets), bs = "ts")
#     + s(scale(pred_extended_stats_f50ground_ball_gets), bs = "ts")
#     ### + s(scale(pred_extended_stats_score_launches), bs='ts') # .
#     + s(scale(pred_extended_stats_pressure_acts), bs = "ts")
#     + s(scale(pred_extended_stats_def_half_pressure_acts), bs = "ts") # *** -
#     + s(scale(pred_extended_stats_spoils), bs = "ts") # * +
#     + s(scale(pred_extended_stats_ruck_contests), bs = "ts") #+
#     + s(scale(pred_extended_stats_contest_def_one_on_ones), bs = "ts")
#     + s(scale(pred_extended_stats_contest_def_losses), bs = "ts")
#     + s(scale(pred_extended_stats_contest_off_one_on_ones), bs = "ts")
#     + s(scale(pred_extended_stats_contest_off_wins), bs = "ts")
#     ### + s(scale(pred_extended_stats_centre_bounce_attendances), bs='ts')
#     ### + s(scale(pred_extended_stats_kickins), bs='ts')
#     ### + s(scale(pred_extended_stats_kickins_playon), bs='ts')
#     ############# BINOM
#     # + s(scale(pred_time_on_ground_percentage), bs = "ts")
#     + s(scale(pred_disposal_efficiency), bs = "ts")
#     + s(scale(pred_goal_accuracy), bs = "ts")
#     + s(scale(pred_extended_stats_kick_efficiency), bs = "ts")
#     + s(scale(pred_extended_stats_contested_possession_rate), bs = "ts")
#     + s(scale(pred_extended_stats_hitout_win_percentage), bs = "ts")
#     + s(scale(pred_extended_stats_hitout_to_advantage_rate), bs = "ts")
#     + s(scale(pred_extended_stats_contest_def_loss_percentage), bs = "ts")
#     + s(scale(pred_extended_stats_contest_off_wins_percentage), bs = "ts")
#   #   ### positions
#   # + I(CB.x)
#   # + I(BP.x)
#   # + I(HBF.x)
#   # + I(W.x)
#   # + I(MIDS.x)
#   # + I(RK.x)
#   # + I(HFF.x)
#   # + I(FP.x)
#   # + I(CF.x)
#   # + I(int.x)
#   # #+ def.y + mid.y + fwd.y #+ int.y
#   ,
#   data = team_mdl_df %>% dplyr::filter(count.x == count.y, season.x > 2021),
#   select = T, nthreads = 4
#   # ,discrete = T
#   , family = "gaussian"
# )
#
# summary(afl_torp_diff_mdl)
# # plot(mgcViz::getViz(afl_torp_diff_mdl))

#
afl_torp_offset_mdl <- mgcv::bam(
  score_diff ~
    team_type_fac
    + offset(torp_diff)
    + torp_recv_diff
    + torp_disp_diff
    + torp_spoil_diff
    + torp_hitout_diff,
  data = team_mdl_df %>% dplyr::filter(count.x == count.y, season.x > 2021),
  select = T, nthreads = 4
  # ,discrete = T
  , family = "gaussian"
)

summary(afl_torp_offset_mdl)


#######

afl_torp_posn_mdl <- mgcv::bam(
  score_diff ~
    team_type_fac
    + offset(torp_diff)
    ### positions
    + I(CB.x)
    + I(BP.x)
    + I(HBF.x)
    + I(W.x)
    + I(MIDS.x)
    + I(RK.x)
    + I(HFF.x)
    + I(FP.x)
    + I(CF.x)
    + I(int.x),
  data = team_mdl_df %>% dplyr::filter(count.x == count.y, season.x > 2021),
  select = T, nthreads = 4
  # ,discrete = T
  , family = "gaussian"
)

summary(afl_torp_posn_mdl)


# ####
# # plot(mgcViz::getViz(afl_torp_diff_mdl))
#
# team_mdl_df$pred_score_difference <- predict(afl_torp_diff_mdl, team_mdl_df, type = "response")
#
# #############
# pred_df$team_type_fac <- as.factor("home")
# pred_df$torp_diff <- 0
#
# pred_df$pred_val <- mgcv::predict.bam(afl_torp_diff_mdl, newdata = pred_df, type = "response")
#
#
# ###################################
# ##### BASIC TESTING
#
afl_torp_diff_basic <- mgcv::bam(
  score_diff ~
    team_type_fac
    + I(torp_diff)
    #   ### LINEUP DATA
    #     # + CB.x
    #     # + BP.x
    #     # + HBF.x
    #     # + W.x
    #     # + MIDS.x
    #     # + RK.x
    #     # + HFF.x
    #     # + FP.x
    #     # + CF.x
    #     # + int.x
    #   ### CHAMP DATA
    + key_def.x
    + med_def.x
    + midfield.x
    + mid_fwd.x
    + med_fwd.x
    + key_fwd.x
    + rucks.x
  #+ def.y + mid.y + fwd.y #+ int.y
  ,
  data = team_mdl_df %>% dplyr::filter(count.x == count.y, season.x > 2021),
  select = T, # discrete = T,
  nthreads = 4,
  family = "gaussian"
)
#
#
summary(afl_torp_diff_basic)
#
#
# ##### BASIC TESTING
# afl_torp_diff_win_mdl <- mgcv::bam(
#   win ~ team_type_fac
#     + torp_diff
#     + key_def.x + med_def.x + midfield.x + mid_fwd.x
#     + med_fwd.x + key_fwd.x + rucks.x
#     # + key_def.y + med_def.y + midfield.y + mid_fwd.y
#     # + med_fwd.y + key_fwd.y + rucks.y
#     # + int.x
#   # + int.y
#     # + pred_score_difference
#   ,
#   data = team_mdl_df %>% dplyr::filter(),
#   family = "binomial"
# )
#
# summary(afl_torp_diff_win_mdl)
#
# adf <- team_mdl_df %>%
#   dplyr::filter(
#     season.x == get_afl_season("current"),
#     round.roundNumber.x == get_afl_week("current"),
#     team_type_fac == "home"
#   ) %>%
#   dplyr::mutate(
#     pred_score_difference = predict(afl_torp_diff_mdl, .),
#     pred_win = predict(afl_torp_diff_win_mdl, ., type = "response")
#   )
#
# adf %>% dplyr::select(providerId, count.x, venue.name.x, team_name.x, torp.x, team_name.y, torp.y, torp_diff, int.x, int.y, pred_score_difference, pred_win)
# #################### Positional value testing
# ###
# # team_mdl_df[,13:18] <- scale(team_mdl_df[,13:18])
# # team_mdl_df[,62:67] <- scale(team_mdl_df[,62:67])
#
# afl_score_pos <- mgcv::bam(
#   score_diff ~
#     0 +
#     s(team_type_fac, bs = "re")
#     ### standard variables
#     # + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
#     # + s(torp_diff, bs = "ts", k = 5)
#     # + s(torp_recv_g_diff, bs = "ts", k = 5)
#     # + s(torp_disp_g_diff, bs = "ts", k = 5)
#     # # + s(hoff_adef,bs="ts") + s(hmid_amid,bs="ts")+ s(hdef_afwd,bs="ts")+ s(hint_aint,bs="ts")
#     ### positional phase
#     # + fwd.x + mid.x + def.x + int.x
#     # + fwd.y + mid.y + def.y + int.y
#     ### champion position
#     + key_def.x + med_def.x + midfield.x + mid_fwd.x + med_fwd.x + key_fwd.x + rucks.x
#     + key_def.y + med_def.y + midfield.y + mid_fwd.y + med_fwd.y + key_fwd.y + rucks.y
#     ### positional line
#     # + I(backs.x) + I(half_backs.x ) + I(midfielders.x )
#     # + I(followers.x ) + I(half_forwards.x ) + I(forwards.x )
#     # + I(backs.y) + I(half_backs.y ) + I(midfielders.y )
#     # + I(followers.y ) + I(half_forwards.y ) + I(forwards.y )
#     ### grouped position
#     # + I((FB.x))
#     # + I((CHB.x))
#     # + I((BPL.x + BPR.x))
#     # + I((HBFL.x + HBFR.x))
#     # + I((WL.x + WR.x))
#     # + I(C.x)
#     # + I(R.x) + I(RR.x) + I(RK.x)
#     # + I((HFFL.x + HFFR.x))
#     # + I((FPL.x + FPR.x))
#     # + I((CHF.x))
#     # + I((FF.x))
#     # + I((FB.y))
#     # + I((CHB.y))
#     # + I((BPL.y + BPR.y))
#     # + I((HBFL.y + HBFR.y))
#     # + I((WL.y + WR.y))
#     # + I(C.y)
#     # + I(R.y) + I(RR.y) + I(RK.y)
#     # + I((HFFL.y + HFFR.y))
#     # + I((FPL.y + FPR.y))
#     # + I((CHF.y))
#     # + I((FF.y))
#     ### individual position
#     # + I(BPL.x)+ I(BPR.x) + I(FB.x)
#     # + I(HBFL.x) + I(HBFR.x) + I(CHB.x)
#     # + I(WL.x) + I(WR.x) + I(C.x)
#     # + I(R.x) + I(RR.x) + I(RK.x)
#     # + I(HFFL.x) + I(HFFR.x)  + I(CHF.x)
#     # + I(FPL.x) + I(FPR.x) + I(FF.x)
#     # + I(BPL.y)+ I(BPR.y) + I(FB.y)
#     # + I(HBFL.y) + I(HBFR.y) + I(CHB.y)
#     # + I(WL.y) + I(WR.y) + I(C.y)
#     # + I(R.y) + I(RR.y) + I(RK.y)
#     # + I(HFFL.y) + I(HFFR.y)  + I(CHF.y)
#     # + I(FPL.y) + I(FPR.y) + I(FF.y)
#     ### interchange
#     + int.x + int.y,
#   data = team_mdl_df,
#   family = "gaussian", nthreads = 4, select = T, discrete = T
# )
#
# summary(afl_score_pos)
# summary(coef(afl_score_pos)[2:7])
# # plot(mgcViz::getViz(afl_score_pos))
#
# # ####
# # team_rt_df <- team_rt_df %>%
# #   dplyr::mutate(
# #     backs_adj = tidyr::replace_na(backs * backs_ind, 0),
# #     half_backs_adj = tidyr::replace_na(half_backs * half_backs_ind, 0),
# #     midfielders_adj = tidyr::replace_na(midfielders * midfielders_ind, 0),
# #     followers_adj = tidyr::replace_na(followers * followers_ind, 0),
# #     half_forwards_adj = tidyr::replace_na(half_forwards * half_forwards_ind, 0),
# #     forwards_adj = tidyr::replace_na(forwards * forwards_ind, 0),
# #     ### ind pos
# #     BPL2 = tidyr::replace_na(BPL^BP_ind, 0),
# #     BPR2 = tidyr::replace_na(BPR^BP_ind, 0),
# #     FB2 = tidyr::replace_na(FB^FB_ind, 0),
# #     HBFL2 = tidyr::replace_na(HBFL^HBF_ind, 0),
# #     HBFR2 = tidyr::replace_na(HBFR^HBF_ind, 0),
# #     CHB2 = tidyr::replace_na(CHB^CHB_ind, 0),
# #     WL2 = tidyr::replace_na(WL^W_ind, 0),
# #     WR2 = tidyr::replace_na(WR^W_ind, 0),
# #     C2 = tidyr::replace_na(C^C_ind, 0),
# #     R2 = tidyr::replace_na(R^R_ind, 0),
# #     RR2 = tidyr::replace_na(RR^RR_ind, 0),
# #     RK2 = tidyr::replace_na(RK^RK_ind, 0),
# #     HFFL2 = tidyr::replace_na(HFFL^HFF_ind, 0),
# #     HFFR2 = tidyr::replace_na(HFFR^HFF_ind, 0),
# #     CHF2 = tidyr::replace_na(CHF^CHF_ind, 0),
# #     FPL2 = tidyr::replace_na(FPL^FP_ind, 0),
# #     FPR2 = tidyr::replace_na(FPR^FP_ind, 0),
# #     FF2 = tidyr::replace_na(FF^FF_ind, 0)
# #   )
# #
# # tot_x <- tibble::tibble()
# #
# # for (i in 51:56) {
# #   x <- (dplyr::bind_cols(pos = colnames(team_rt_df[, i]), sdz = (sqrt(var(team_rt_df[, i] %>% dplyr::pull()))), avg = (mean(team_rt_df[, i] %>% dplyr::pull(), na.rm = T))))
# #   tot_x <- dplyr::bind_rows(tot_x, x)
# # }
#
# # ###
# # xg_df <- furrr::future_map_dfr(1:27,~match_xgs(2021,.))
# #
# # xg_df2 <- xg_df %>%
# #   mutate(score_diff = home_shots_score - away_shots_score,
# #          xscore_diff = home_xscore - away_xscore)
# #
# # ModelMetrics::mae(xg_df2$score_diff,xg_df2$xscore_diff)
