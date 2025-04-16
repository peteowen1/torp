library(tidyverse)
#### library(furrr)
### testing
set.seed(1234)

mdl_wk <- function(df, season, weeknum) {
  val <- paste0("CD_M", season, "014", sprintf("%02d", weeknum))
  team_func_df <- df %>% dplyr::filter(providerId < val, providerId > "CD_M202101409")
  # team_func_df <- df %>% dplyr::filter(season.x != season | round.roundNumber.x < weeknum )
  if (!is.na(max(team_func_df$homeTeamScore.matchScore.totalScore))) {
    decay <- 1000
    # weightz_func <- exp(as.numeric(-(max(as.Date(team_func_df$match.utcStartTime)) - as.Date(team_func_df$match.utcStartTime))) / decay)
    # weightz_func <- weightz_func / mean(weightz_func, na.rm = T)
    # team_func_df$weightz = weightz_func
    team_func_df$weightz <- 1
    #### MODEL
    # library(mgcViz)
    # set.seed("1234")

    # ###
    # afl_totshots_mdl <- mgcv::bam(
    #   total_shots ~
    #     s(team_type_fac, bs = "re") +
    #     s(hoff_adef, bs='ts') + # = pmax(pmin((fwd.x - def.y), 20), -5),
    #     s(hmid_amid, bs='ts') + # = pmax(pmin((mid.x - mid.y), 12), -12),
    #     s(hdef_afwd, bs='ts') + # = pmax(pmin((def.x - fwd.y), 5), -20),
    #     s(hint_aint, bs='ts') + # = pmax(pmin((int.x - int.y), 10), -10),
    #     + s(def.x, bs='ts')
    #   + s(mid.x, bs='ts')
    #   + s(fwd.x, bs='ts')
    #   + s(int.x, bs='ts')
    #   + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    #   + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
    #   + s(abs(torp_diff), bs = "ts", k = 5)
    #
    #   ,
    #   data = team_func_df, weights = weightz,
    #   family = quasipoisson(), nthreads = 4, select = T, discrete = T
    #   , drop.unused.levels = FALSE
    # )
    #
    # df$pred_totshots <- predict(afl_totshots_mdl, newdata = df, type = "response")
    #
    # # summary(afl_totshots_mdl)
    # # Deviance explained =   22%
    #
    # ###
    # afl_shot_mdl <- mgcv::bam(
    #   shot_diff ~
    #     s(team_type_fac, bs = "re")
    #   + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    #   + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
    #   + ti(torp_diff, pred_totshots, bs = c("ts", "ts"), k = 4)
    #   + s(pred_totshots, bs = "ts", k = 5)
    #   + s(torp_diff, bs = "ts", k = 5),
    #   data = team_func_df, weights = weightz,
    #   family = gaussian(), nthreads = 4, select = T, discrete = T
    #   , drop.unused.levels = FALSE
    # )
    #
    # df$pred_shot_diff <- predict(afl_shot_mdl, newdata = df, type = "response")
    #
    # # summary(afl_shot_mdl)
    # # mixedup::extract_ranef(afl_shot_mdl) %>% tibble::view()
    # # plot(mgcViz::getViz(afl_shot_mdl))
    # # Deviance explained = 44.5%

    ###
    afl_total_xpoints_mdl_test <- mgcv::bam(
      total_xpoints_adj ~
        s(team_type_fac.x, bs = "re")
      + s(game_year_decimal.x, bs = "ts")
      + s(game_prop_through_year.x, bs = "cc")
      + s(game_prop_through_month.x, bs = "cc")
      + s(game_wday_fac.x, bs = "re")
      + s(game_prop_through_day.x, bs = "cc")
      + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
      + s(abs(torp_diff), bs = "ts", k = 5)
      + s(abs(torp_recv_diff), bs = "ts", k = 5)
      + s(abs(torp_disp_diff), bs = "ts", k = 5)
      + s(abs(torp_spoil_diff), bs = "ts", k = 5)
      + s(abs(torp_hitout_diff), bs = "ts", k = 5)
      + s(torp.x, bs = "ts", k = 5) + s(torp.y, bs = "ts", k = 5)
      # + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
      # + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
      + s(venue_fac, bs='re')
      + s(log_dist.x, bs = "ts", k = 5) + s(log_dist.y, bs = "ts", k = 5)
      + s(familiarity.x, bs = "ts", k = 5) + s(familiarity.y, bs = "ts", k = 5)
      + s(log_dist_diff, bs = "ts", k = 5)
      + s(familiarity_diff, bs = "ts", k = 5)
      + s(days_rest_diff_fac, bs = "re")
      ,
      data = team_func_df, weights = weightz,
      family = gaussian(), nthreads = 4, select = T, discrete = T,
      drop.unused.levels = FALSE
    )

    df$pred_tot_xscore <- predict(afl_total_xpoints_mdl_test, newdata = df, type = "response")

    # summary(afl_total_xpoints_mdl_test)
    # Deviance explained =   22%

    ###
    afl_xscore_diff_mdl_test <- mgcv::bam(
      xscore_diff ~
        s(team_type_fac, bs = "re")
      + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
      + ti(torp_diff, pred_tot_xscore, bs = c("ts", "ts"), k = 4)
      + s(pred_tot_xscore, bs = "ts", k = 5)
      + s(torp_diff, bs = "ts", k = 5)
      + s(torp_recv_diff, bs = "ts", k = 5)
      + s(torp_disp_diff, bs = "ts", k = 5)
      + s(torp_spoil_diff, bs = "ts", k = 5)
      + s(torp_hitout_diff, bs = "ts", k = 5)
      # + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
      # + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re")
      ,
      data = team_func_df, weights = weightz,
      family = gaussian(), nthreads = 4, select = T, discrete = T,
      drop.unused.levels = FALSE
    )

    df$pred_xscore_diff <- predict(afl_xscore_diff_mdl_test, newdata = df, type = "response")

    # summary(afl_xscore_diff_mdl_test)
    # mixedup::extract_ranef(afl_xscore_diff_mdl_test) %>% tibble::view()
    # plot(mgcViz::getViz(afl_xscore_diff_mdl_test))
    # Deviance explained = 44.5%


    ###
    afl_conv_mdl_test <- mgcv::bam(
      shot_conv_diff ~
        s(team_type_fac.x, bs = "re")
      + s(game_year_decimal.x, bs = "ts")
      + s(game_prop_through_year.x, bs = "cc")
      + s(game_prop_through_month.x, bs = "cc")
      + s(game_wday_fac.x, bs = "re")
      + s(game_prop_through_day.x, bs = "cc")
      + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
      + ti(torp_diff, pred_tot_xscore, bs = c("ts", "ts"), k = 4)
      + s(torp_diff, bs = "ts", k = 5)
      + s(torp_recv_diff, bs = "ts", k = 5)
      + s(torp_disp_diff, bs = "ts", k = 5)
      + s(torp_spoil_diff, bs = "ts", k = 5)
      + s(torp_hitout_diff, bs = "ts", k = 5)
      + s(pred_tot_xscore, bs = "ts", k = 5)
      + s(pred_xscore_diff, bs = "ts", k = 5)
      # + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
      # + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
      + s(venue_fac, bs='re')
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re")
      ,
      data = team_func_df, weights = shot_weightz,
      family = gaussian(), nthreads = 4, select = T, discrete = T,
      drop.unused.levels = FALSE
    )

    df$pred_conv_diff <- predict(afl_conv_mdl_test, newdata = df, type = "response")
    # summary(afl_conv_mdl_test)
    ###

    afl_score_mdl_test <- mgcv::bam(
      score_diff ~
        s(team_type_fac, bs = "re")
      + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
      + ti(pred_xscore_diff, pred_conv_diff, bs = 'ts', k = 5)
      + ti(pred_tot_xscore, pred_conv_diff, bs = 'ts', k = 5)
      # + s(pred_tot_xscore, bs = "ts", k = 5)
      + s(pred_conv_diff, bs = "ts", k = 5)
      + s(pred_xscore_diff)
      + s(torp_diff, bs = "ts", k = 5)
      + s(torp_recv_diff, bs = "ts", k = 5)
      + s(torp_disp_diff, bs = "ts", k = 5)
      + s(torp_spoil_diff, bs = "ts", k = 5)
      + s(torp_hitout_diff, bs = "ts", k = 5)
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re")
      ,
      data = team_func_df, weights = weightz,
      family = "gaussian", nthreads = 4, select = T, discrete = T,
      drop.unused.levels = FALSE
    )

    df$pred_score_diff <- predict(afl_score_mdl_test, newdata = df, type = "response")

    # summary(afl_score_mdl_test)
    # Deviance explained = 42.2%

    ###
    afl_win_mdl_test <-
      mgcv::bam(
        win ~
          # s(team_type_fac, bs = "re")
          +s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
        + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
        + ti(pred_tot_xscore, pred_score_diff, bs = c("ts", "ts"), k = 4)
        # + ti(pred_tot_xscore, pred_xscore_diff, bs = c("ts", "ts"), k = 4)
        + s(pred_score_diff, bs = "ts", k = 5)
        #+ s(pred_xscore_diff, bs = "ts", k = 5)
        + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re")
        ,
        data = team_func_df, weights = weightz,
        family = "binomial", nthreads = 4, select = T, discrete = T,
        drop.unused.levels = FALSE
      )

    # summary(afl_win_mdl_test)

    ###
    df$pred_win <- predict(afl_win_mdl_test, newdata = df, type = "response")

    #########
    test_df <- df %>% dplyr::filter(season.x == season, round.roundNumber.x == weeknum) # , team_type_fac == "home")
    test_df$max_wp <- pmax(test_df$pred_win, 1 - test_df$pred_win)
    test_df$bits <- ifelse(test_df$win == 1,
      1 + log2(test_df$pred_win),
      ifelse(test_df$win == 0,
        1 + log2(1 - test_df$pred_win),
        1 + 0.5 * log2(test_df$pred_win * (1 - test_df$pred_win))
      )
    )
    test_df$tips <- ifelse(round(test_df$pred_win) == test_df$win, 1,
      ifelse(test_df$win == 0.5, 1, 0)
    )
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

  test_df <- df %>% dplyr::filter(season.x == season, round.roundNumber.x == weeknum) # , team_type_fac == "home")
  # print(weeknum)
  return(test_df)
}

#########################################################
library(furrr) # 415 secs
library(purrr) # 1055 secs
plan("multisession", workers = (parallelly::availableCores() - 2))

tictoc::tic()
resultz <- bind_rows(
  # furrr::future_map(1:27, ~ mdl_wk(team_mdl_df, 2022, .), .progress=T, .options=furrr_options(seed = TRUE)),
  # furrr::future_map(1:28, ~ mdl_wk(team_mdl_df, 2023, .), .progress=T, .options=furrr_options(seed = TRUE)),
  furrr::future_map(0:28, ~ mdl_wk(team_mdl_df, 2024, .), .progress = T, .options = furrr_options(seed = TRUE)),
  furrr::future_map(0:get_afl_week("next"), ~ mdl_wk(team_mdl_df, 2025, .), .progress = T, .options = furrr_options(seed = TRUE))
)
tictoc::toc()

resultz <-
  resultz %>%
  mutate(tips = ifelse(win == 0.5, 1, tips))

saveRDS(resultz,'./shiny/resultz.rds')

# Get chains data  -------------------------------------------------------------
season <- get_afl_season()

get_predictions_data <- function(season) {
  # Sys.setenv(TZ = "Australia/Melbourne")
  file_name <- glue::glue("predictions_{season}")

  rdf  <- resultz %>% dplyr::filter(season.x %in% season)

  save_to_release(df = rdf, file_name = file_name, release_tag = "predictions")
}

# extract and save
get_predictions_data(season)

###
# targets (MAE - bits)
# 22: 25 - 40
# 23: 26 - 30
# 24: ???

rez <- resultz %>%
  dplyr::filter(!is.na(bits), team_type_fac == "home", season.x %in% season) %>%
  dplyr::select(providerId, season.x,
    round = round.roundNumber.x,
    team_name_adj.x, torp.x, team_name_adj.y, torp.y,
    torp_diff, pred_tot_xscore, pred_score_diff, pred_win, score_diff, win, bits, tips
  )

####
# library(MLmetrics)
MLmetrics::LogLoss(rez$pred_win, rez$win)
MLmetrics::MAE(rez$pred_score_diff, rez$score_diff)
sum(rez$bits)
sum(rez$tips)
mean(rez$bits)
mean(rez$tips)
nrow(rez)

