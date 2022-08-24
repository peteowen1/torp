library(furrr)
### testing
set.seed(1234)

mdl_wk <- function(df, season, weeknum) {
  val <- paste0("CD_M", season, "014", sprintf("%02d", weeknum))
  team_func_df <- df %>% filter(providerId < val)
  if (!is.na(max(team_func_df$homeTeamScore.matchScore.totalScore))) {
    weightz_func <- exp(as.numeric(-(max(as.Date(team_func_df$match.utcStartTime)) - as.Date(team_func_df$match.utcStartTime))) / 250)
    weightz_func <- weightz_func / mean(weightz_func, na.rm = T)
    ###
    ###
    afl_totshots_mdl <- bam(total_shots ~
      s(team_type_fac, bs = "re")
      + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      # s(bayes_g_diff,bs="ts") + s(bayes_recv_g_diff,bs="ts") + s(bayes_disp_g_diff,bs="ts")
      + s(abs(bayes_g_diff), bs = "ts", k = 5)
    #+ s(abs(bayes_recv_g_diff), bs = "ts", k = 5)
    #+ s(abs(bayes_disp_g_diff), bs = "ts", k = 5)
    # + s(hoff_adef,bs="ts") + s(hmid_amid,bs="ts")+ s(hdef_afwd,bs="ts") + s(hint_aint,bs="ts")
    #+ s((hoff_adef), bs = "ts", k = 5) + s(abs(hmid_amid), bs = "ts", k = 5)
    #+ s((hdef_afwd), bs = "ts", k = 5) + s(abs(hint_aint), bs = "ts", k = 5)
    #+ s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
    #+ s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
    ,
    data = team_func_df, weights = weightz_func,
    family = quasipoisson(), nthreads = 4, select = T, discrete = T
    )
    team_func_df$pred_totshots <- predict(afl_totshots_mdl, newdata = team_func_df, type = "response")
    df$pred_totshots <- predict(afl_totshots_mdl, newdata = df, type = "response")

    # mixedup::extract_ranef(afl_totshots_mdl) %>% view()
    # plot(mgcViz::getViz(afl_totshots_mdl))
    ###
    afl_shot_mdl <- bam(shot_diff ~
      s(team_type_fac, bs = "re")
      + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + ti(bayes_g_diff, pred_totshots, bs = c("ts", "ts"), k = 4)
      + s(pred_totshots, bs = "ts", k = 5)
      + s(bayes_g_diff, bs = "ts", k = 5) #+ s(bayes_recv_g_diff, bs = "ts") + s(bayes_disp_g_diff, bs = "ts")
    #+ s(hoff_adef, bs = "ts", k = 5)
    #+ s(hmid_amid, bs = "ts", k = 5)
    #+ s(hdef_afwd, bs = "ts", k = 5) + s(hint_aint, bs = "ts", k = 5)
    #+ s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
    #+ s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
    ,
    data = team_func_df, weights = weightz_func,
    family = gaussian(), nthreads = 4, select = T, discrete = T
    )

    team_func_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_func_df, type = "response")
    df$pred_shot_diff <- predict(afl_shot_mdl, newdata = df, type = "response")

    # plot(mgcViz::getViz(afl_shot_mdl))
    # mixedup::extract_ranef(afl_shot_mdl) %>% view()

    ###
    afl_conv_mdl <- bam(shot_conv ~
      s(team_type_fac, bs = "re")
      #+ s(team_name.x, bs = "re")+ s(team_name.y, bs = "re")
      + ti(bayes_g_diff, pred_totshots, bs = c("ts", "ts"), k = 4)
      + s(pred_totshots, bs = "ts", k = 5)
      + s(pred_shot_diff, bs = "ts", k = 5)
      + s(bayes_g_diff, bs = "ts", k = 5) #+ s(bayes_recv_g_diff, bs = "ts") + s(bayes_disp_g_diff, bs = "ts")
      # + s(hoff_adef, bs = "ts", k = 5) + s(hmid_amid, bs = "ts", k = 5)
      # + s(hdef_afwd, bs = "ts", k = 5) + s(hint_aint, bs = "ts", k = 5)
      + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
      + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5),
    data = team_func_df, weights = team_shots * weightz_func,
    family = "binomial", nthreads = 4, select = T, discrete = T
    )

    team_func_df$pred_conv <- predict(afl_conv_mdl, newdata = team_func_df, type = "response")
    df$pred_conv <- predict(afl_conv_mdl, newdata = df, type = "response")

    # summary(afl_conv_mdl)
    # plot(mgcViz::getViz(afl_conv_mdl))
    ###

    afl_score_mdl <- bam(score_diff ~
      s(team_type_fac, bs = "re")
      #+ s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      #+ ti(pred_shot_diff, pred_totshots, bs = c("ts", "ts"), k = 5)
      + s(pred_totshots, bs = "ts", k = 5)
      + ti(pred_shot_diff, pred_conv, bs = c("ts", "ts"), k = 4)
      + s(pred_conv, bs = "ts", k = 5)
      + s(bayes_g_diff, bs = "ts", k = 5) + s(bayes_recv_g_diff, bs = "ts", k = 5) + s(bayes_disp_g_diff, bs = "ts", k = 5)
      + s(pred_shot_diff, bs = "ts", k = 5)
    # + s(hoff_adef,bs="ts") + s(hmid_amid,bs="ts")+ s(hdef_afwd,bs="ts")+ s(hint_aint,bs="ts")
    #+ s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
    #+ s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
    ,
    data = team_func_df, weights = weightz_func,
    family = "gaussian", nthreads = 4, select = T, discrete = T
    )
    # summary(afl_score_mdl)
    # plot(mgcViz::getViz(afl_score_mdl))

    team_func_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_func_df, type = "response")
    df$pred_score_diff <- predict(afl_score_mdl, newdata = df, type = "response")

    ###
    afl_win_mdl <-
      bam(win ~
        s(team_type_fac, bs = "re")
        # + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
        + ti(pred_totshots, pred_score_diff, bs = c("ts", "ts"), k = 4)
        # + s(pred_totshots, bs = "ts")
        + s(pred_score_diff, bs = "ts", k = 5) #+ s(pred_shot_diff,bs="ts")
      #+ s(bayes_g_diff,bs="ts")  + s(bayes_recv_g_diff,bs="ts") + s(bayes_disp_g_diff,bs="ts")
      #+ s(hoff_adef,bs="ts") + s(hmid_amid,bs="ts")+ s(hdef_afwd,bs="ts")+ s(hint_aint,bs="ts")
      # + s(fwd.x,bs="ts") + s(mid.x,bs="ts") + s(def.x,bs="ts") + s(int.x,bs="ts")
      # + s(fwd.y,bs="ts") + s(mid.y,bs="ts") + s(def.y,bs="ts") + s(int.y,bs="ts")#+ ti(pred_score_diff,pred_shot_diff),
      ,
      data = team_func_df, weights = weightz_func,
      family = "binomial", nthreads = 4, select = T, discrete = T
      )

    team_func_df$pred_win <- predict(afl_win_mdl, newdata = team_func_df, type = "response")
    df$pred_win <- predict(afl_win_mdl, newdata = df, type = "response")

    #########
    test_df <- df %>% filter(season.x == season, round.roundNumber.x.x == weeknum, team_type_fac == "home")
    test_df$max_wp <- pmax(test_df$pred_win, 1 - test_df$pred_win)
    test_df$bits <- ifelse(test_df$win == 1,
      1 + log2(test_df$pred_win),
      ifelse(test_df$win == 0,
        1 + log2(1 - test_df$pred_win),
        1 + 0.5 * log2(test_df$pred_win * (1 - test_df$pred_win))
      )
    )
    test_df$tips <- ifelse(round(test_df$pred_win) == test_df$win, 1, 0)
    test_df$mae <- abs(test_df$score_diff - test_df$pred_score_diff)

    # library(MLmetrics)
    # LogLoss(test_df$pred_win, test_df$win)
    # MAE(test_df$pred_score_diff, test_df$score_diff)
    # sum(test_df$bits)
    # mean(test_df$bits)
    # mean(test_df$tips)
    print(weeknum)

    return(test_df)
  }

  test_df <- df %>% filter(season.x == season, round.roundNumber.x.x == weeknum, team_type_fac == "home")
  print(weeknum)
  return(test_df)
}

#########################################################
season <- 2022
resultz <- furrr::future_map_dfr(1:30, ~ mdl_wk(team_mdl_df, season, .), .options = furrr_options(seed = TRUE))
rez <- resultz %>% filter(!is.na(bits))
####
library(MLmetrics)
LogLoss(rez$pred_win, rez$win)
MAE(rez$pred_score_diff, rez$score_diff)
sum(rez$bits)
mean(rez$bits)
sum(rez$tips)
mean(rez$tips)

### rez %>% select(30:38,41:55,57:59,61:62,bayes_g_diff,shot_diff,score_diff) %>% view()
### view(df %>% filter(providerId == "CD_M20220140306"))

#################### TESTING

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

afl_score_mdl <- bam(score_diff ~
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
  mutate(
    backs_adj = replace_na(backs*backs_ind , 0),
    half_backs_adj = replace_na(half_backs*half_backs_ind , 0),
    midfielders_adj = replace_na(midfielders*midfielders_ind , 0),
    followers_adj = replace_na(followers*followers_ind , 0),
    half_forwards_adj = replace_na(half_forwards*half_forwards_ind , 0),
    forwards_adj = replace_na(forwards*forwards_ind , 0),
    ### ind pos
    BPL2 = replace_na(BPL^BP_ind, 0),
    BPR2 = replace_na(BPR^BP_ind, 0),
    FB2 = replace_na(FB^FB_ind, 0),
    HBFL2 = replace_na(HBFL^HBF_ind, 0),
    HBFR2 = replace_na(HBFR^HBF_ind, 0),
    CHB2 = replace_na(CHB^CHB_ind, 0),
    WL2 = replace_na(WL^W_ind, 0),
    WR2 = replace_na(WR^W_ind, 0),
    C2 = replace_na(C^C_ind, 0),
    R2 = replace_na(R^R_ind, 0),
    RR2 = replace_na(RR^RR_ind, 0),
    RK2 = replace_na(RK^RK_ind, 0),
    HFFL2 = replace_na(HFFL^HFF_ind, 0),
    HFFR2 = replace_na(HFFR^HFF_ind, 0),
    CHF2 = replace_na(CHF^CHF_ind, 0),
    FPL2 = replace_na(FPL^FP_ind, 0),
    FPR2 = replace_na(FPR^FP_ind, 0),
    FF2 = replace_na(FF^FF_ind, 0)
  )

tot_x <- tibble()

for (i in 51:56) {
  x <- (bind_cols(pos = colnames(team_rt_df[, i]), sdz = (sqrt(var(team_rt_df[, i] %>% pull()))), avg = (mean(team_rt_df[, i] %>% pull(), na.rm = T))))
  tot_x <- bind_rows(tot_x, x)
}
