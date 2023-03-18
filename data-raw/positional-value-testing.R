#################### Positional value testing

BP_ind <- 1
FB_ind <- 1.4
HBF_ind <- 1.05
CHB_ind <- 1.4
W_ind <- 0.65
C_ind <- 0.95
R_ind <- 0.8
RR_ind <- 1
RK_ind <- 1
HFF_ind <- 0.85
CHF_ind <- 1
FP_ind <- 1.2
FF_ind <- 1
###
backs_ind <- 1.5
half_backs_ind <- 1.3
midfielders_ind <- 0.8
followers_ind <- 0.7
half_forwards_ind <- 1
forwards_ind <- 1.4

###
# team_mdl_df[,13:18] <- scale(team_mdl_df[,13:18])
# team_mdl_df[,62:67] <- scale(team_mdl_df[,62:67])

afl_score_mdl <- mgcv::bam(score_diff ~
                             s(team_type_fac, bs = "re")
                           # # + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
                           # + s(bayes_g_diff, bs = "ts", k = 5)
                           # + s(bayes_recv_g_diff, bs = "ts", k = 5)
                           # + s(bayes_disp_g_diff, bs = "ts", k = 5)
                           # # + s(hoff_adef,bs="ts") + s(hmid_amid,bs="ts")+ s(hdef_afwd,bs="ts")+ s(hint_aint,bs="ts")
                           ### positional phase
                           # + fwd.x + mid.x + def.x + int.x
                           # + fwd.y + mid.y + def.y + int.y
                           ### champion position
                           # + key_def.x + med_def.x + midfield.x + mid_fwd.x + med_fwd.x + key_fwd.x + rucks.x
                           # + key_def.y + med_def.y + midfield.y + mid_fwd.y + med_fwd.y + key_fwd.y + rucks.y
                           ### positional line
                           + I(backs.x) + I(half_backs.x ) + I(midfielders.x )
                           + I(followers.x ) + I(half_forwards.x ) + I(forwards.x )
                           + I(backs.y) + I(half_backs.y ) + I(midfielders.y )
                           + I(followers.y ) + I(half_forwards.y ) + I(forwards.y )
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
    backs_adj = tidyr::replace_na(backs*backs_ind , 0),
    half_backs_adj = tidyr::replace_na(half_backs*half_backs_ind , 0),
    midfielders_adj = tidyr::replace_na(midfielders*midfielders_ind , 0),
    followers_adj = tidyr::replace_na(followers*followers_ind , 0),
    half_forwards_adj = tidyr::replace_na(half_forwards*half_forwards_ind , 0),
    forwards_adj = tidyr::replace_na(forwards*forwards_ind , 0),
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
