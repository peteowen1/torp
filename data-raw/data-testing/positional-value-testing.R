##### BASIC TESTING
<<<<<<< HEAD
afl_torp_diff_mdl <- stats::glm(
  score_diff ~
    team_type_fac
    + I(torp_diff)
   + backs.x + half_backs.x + midfielders.x + R.x + RK.x + RR.x + half_forwards.x + forwards.x + int.x
   + backs.y + half_backs.y + midfielders.y + R.y + RK.y + RR.y + half_forwards.y + forwards.y + int.y
  ,
  data = team_mdl_df %>% dplyr::filter(),
  family = "gaussian"
)
=======
afl_torp_diff_mdl <- mgcv::bam(score_diff ~
                                  team_type_fac
                               + I(torp_diff*1.1)
                               + CB.x
                               + BP.x
                               + HBF.x
                               + W.x
                               + MIDS.x
                               + RK.x
                               + HFF.x
                               + FP.x
                               + CF.x
                               + int.x
                         #+ def.y + mid.y + fwd.y #+ int.y
                         ,
                     data = team_mdl_df %>% dplyr::filter(),
                     family = "gaussian")
>>>>>>> 9d3782651a5c754bb6f224263eb91fb4882ea7f9

summary(afl_torp_diff_mdl)

team_mdl_df$pred_score_difference <- predict(afl_torp_diff_mdl, team_mdl_df, type = "response")

##### BASIC TESTING
afl_torp_diff_win_mdl <- mgcv::bam(
  win ~ team_type_fac
    + torp_diff
    + key_def.x + med_def.x + midfield.x + mid_fwd.x
    + med_fwd.x + key_fwd.x + rucks.x
    + key_def.y + med_def.y + midfield.y + mid_fwd.y
    + med_fwd.y + key_fwd.y + rucks.y
    + int.x + int.y
    + pred_score_difference,
  data = team_mdl_df %>% dplyr::filter(),
  family = "binomial"
)

summary(afl_torp_diff_win_mdl)

adf <- team_mdl_df %>%
  dplyr::filter(
    season.x == get_afl_season("current"),
    round.roundNumber.x == get_afl_week("current"),
    team_type_fac == "home"
  ) %>%
  dplyr::mutate(
    pred_score_difference = predict(afl_torp_diff_mdl, .),
    pred_win = predict(afl_torp_diff_win_mdl, ., type = "response")
  )

adf %>% dplyr::select(providerId, count.x, venue.name.x, team_name.x, torp.x, team_name.y, torp.y, torp_diff, int.x, int.y, pred_score_difference, pred_win)
#################### Positional value testing
###
# team_mdl_df[,13:18] <- scale(team_mdl_df[,13:18])
# team_mdl_df[,62:67] <- scale(team_mdl_df[,62:67])

afl_score_mdl <- mgcv::bam(
  score_diff ~
    0 +
    s(team_type_fac, bs = "re")
    ### standard variables
    # + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    # + s(torp_diff, bs = "ts", k = 5)
    # + s(torp_recv_g_diff, bs = "ts", k = 5)
    # + s(torp_disp_g_diff, bs = "ts", k = 5)
    # # + s(hoff_adef,bs="ts") + s(hmid_amid,bs="ts")+ s(hdef_afwd,bs="ts")+ s(hint_aint,bs="ts")
    ### positional phase
    # + fwd.x + mid.x + def.x + int.x
    # + fwd.y + mid.y + def.y + int.y
    ### champion position
    + key_def.x + med_def.x + midfield.x + mid_fwd.x + med_fwd.x + key_fwd.x + rucks.x
    + key_def.y + med_def.y + midfield.y + mid_fwd.y + med_fwd.y + key_fwd.y + rucks.y
    ### positional line
    # + I(backs.x) + I(half_backs.x ) + I(midfielders.x )
    # + I(followers.x ) + I(half_forwards.x ) + I(forwards.x )
    # + I(backs.y) + I(half_backs.y ) + I(midfielders.y )
    # + I(followers.y ) + I(half_forwards.y ) + I(forwards.y )
    ### grouped position
    # + I((FB.x))
    # + I((CHB.x))
    # + I((BPL.x + BPR.x))
    # + I((HBFL.x + HBFR.x))
    # + I((WL.x + WR.x))
    # + I(C.x)
    # + I(R.x) + I(RR.x) + I(RK.x)
    # + I((HFFL.x + HFFR.x))
    # + I((FPL.x + FPR.x))
    # + I((CHF.x))
    # + I((FF.x))
    # + I((FB.y))
    # + I((CHB.y))
    # + I((BPL.y + BPR.y))
    # + I((HBFL.y + HBFR.y))
    # + I((WL.y + WR.y))
    # + I(C.y)
    # + I(R.y) + I(RR.y) + I(RK.y)
    # + I((HFFL.y + HFFR.y))
    # + I((FPL.y + FPR.y))
    # + I((CHF.y))
    # + I((FF.y))
    ### individual position
    # + I(BPL.x)+ I(BPR.x) + I(FB.x)
    # + I(HBFL.x) + I(HBFR.x) + I(CHB.x)
    # + I(WL.x) + I(WR.x) + I(C.x)
    # + I(R.x) + I(RR.x) + I(RK.x)
    # + I(HFFL.x) + I(HFFR.x)  + I(CHF.x)
    # + I(FPL.x) + I(FPR.x) + I(FF.x)
    # + I(BPL.y)+ I(BPR.y) + I(FB.y)
    # + I(HBFL.y) + I(HBFR.y) + I(CHB.y)
    # + I(WL.y) + I(WR.y) + I(C.y)
    # + I(R.y) + I(RR.y) + I(RK.y)
    # + I(HFFL.y) + I(HFFR.y)  + I(CHF.y)
    # + I(FPL.y) + I(FPR.y) + I(FF.y)
    ### interchange
    + int.x + int.y,
  data = team_mdl_df,
  family = "gaussian", nthreads = 4, select = T, discrete = T
)

summary(afl_score_mdl)
summary(coef(afl_score_mdl)[2:7])
# plot(mgcViz::getViz(afl_score_mdl))

team_rt_df <- team_rt_df %>%
  dplyr::mutate(
    backs_adj = tidyr::replace_na(backs * backs_ind, 0),
    half_backs_adj = tidyr::replace_na(half_backs * half_backs_ind, 0),
    midfielders_adj = tidyr::replace_na(midfielders * midfielders_ind, 0),
    followers_adj = tidyr::replace_na(followers * followers_ind, 0),
    half_forwards_adj = tidyr::replace_na(half_forwards * half_forwards_ind, 0),
    forwards_adj = tidyr::replace_na(forwards * forwards_ind, 0),
    ### ind pos
    BPL2 = tidyr::replace_na(BPL^BP_ind, 0),
    BPR2 = tidyr::replace_na(BPR^BP_ind, 0),
    FB2 = tidyr::replace_na(FB^FB_ind, 0),
    HBFL2 = tidyr::replace_na(HBFL^HBF_ind, 0),
    HBFR2 = tidyr::replace_na(HBFR^HBF_ind, 0),
    CHB2 = tidyr::replace_na(CHB^CHB_ind, 0),
    WL2 = tidyr::replace_na(WL^W_ind, 0),
    WR2 = tidyr::replace_na(WR^W_ind, 0),
    C2 = tidyr::replace_na(C^C_ind, 0),
    R2 = tidyr::replace_na(R^R_ind, 0),
    RR2 = tidyr::replace_na(RR^RR_ind, 0),
    RK2 = tidyr::replace_na(RK^RK_ind, 0),
    HFFL2 = tidyr::replace_na(HFFL^HFF_ind, 0),
    HFFR2 = tidyr::replace_na(HFFR^HFF_ind, 0),
    CHF2 = tidyr::replace_na(CHF^CHF_ind, 0),
    FPL2 = tidyr::replace_na(FPL^FP_ind, 0),
    FPR2 = tidyr::replace_na(FPR^FP_ind, 0),
    FF2 = tidyr::replace_na(FF^FF_ind, 0)
  )

tot_x <- tibble::tibble()

for (i in 51:56) {
  x <- (dplyr::bind_cols(pos = colnames(team_rt_df[, i]), sdz = (sqrt(var(team_rt_df[, i] %>% dplyr::pull()))), avg = (mean(team_rt_df[, i] %>% dplyr::pull(), na.rm = T))))
  tot_x <- dplyr::bind_rows(tot_x, x)
}

# ###
# xg_df <- furrr::future_map_dfr(1:27,~match_xgs(2022,.))
#
# xg_df2 <- xg_df %>%
#   mutate(score_diff = home_shots_score - away_shots_score,
#          xscore_diff = home_xscore - away_xscore)
#
# ModelMetrics::mae(xg_df2$score_diff,xg_df2$xscore_diff)
