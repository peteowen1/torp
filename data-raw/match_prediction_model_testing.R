library(furrr)
### testing
set.seed(1234)

mdl_wk <- function(df, season, weeknum) {
  val <- paste0("CD_M", season, "014", sprintf("%02d", weeknum))
  team_func_df <- df %>% dplyr::filter(providerId < val)
  if (!is.na(max(team_func_df$homeTeamScore.matchScore.totalScore))) {
    weightz_func <- exp(as.numeric(-(max(as.Date(team_func_df$match.utcStartTime)) - as.Date(team_func_df$match.utcStartTime))) / 250)
    weightz_func <- weightz_func / mean(weightz_func, na.rm = T)
    ###
    ###
    afl_totshots_mdl <- mgcv::bam(total_shots ~
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
    afl_shot_mdl <- mgcv::bam(shot_diff ~
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
    afl_conv_mdl <- mgcv::bam(shot_conv ~
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

    afl_score_mdl <- mgcv::bam(score_diff ~
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
      mgcv::bam(win ~
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
    test_df <- df %>% dplyr::filter(season.x == season, round.roundNumber.x.x == weeknum) #, team_type_fac == "home")
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

  test_df <- df %>% dplyr::filter(season.x == season, round.roundNumber.x.x == weeknum) #, team_type_fac == "home")
  print(weeknum)
  return(test_df)
}

#########################################################
season <- 2022
resultz <- purrr::map_df(1:27, ~ mdl_wk(team_mdl_df, season, .), .options = furrr_options(seed = TRUE))
rez <- resultz %>% dplyr::filter(!is.na(bits))
####
# library(MLmetrics)
MLmetrics::LogLoss(rez$pred_win, rez$win)
MLmetrics::MAE(rez$pred_score_diff, rez$score_diff)
sum(rez$bits)
mean(rez$bits)
sum(rez$tips)
mean(rez$tips)

### rez %>% select(30:38,41:55,57:59,61:62,bayes_g_diff,shot_diff,score_diff) %>% view()
### view(df %>% filter(providerId == "CD_M20220140306"))

