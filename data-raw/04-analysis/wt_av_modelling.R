library(purrr)
library(tidyverse)
library(fitzRoy)
devtools::load_all()

szns <- 2021:get_afl_season()

minmax <- function(x, na.rm = TRUE) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# fil_val <- 24
decay <- 365

tictoc::tic()

pl_details <- fetch_player_details_afl(get_afl_season())

# ps24 <- fetch_player_stats_afl(2024) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# ps23 <- fetch_player_stats_afl(2023) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# ps22 <- fetch_player_stats_afl(2022) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# ps21 <- fetch_player_stats_afl(2021) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# ps20 <- fetch_player_stats_afl(2020) %>%
#   janitor::remove_constant() %>%
#   janitor::clean_names()
# pstot <- bind_rows(ps20, ps21, ps22, ps23, ps24)

pstot <- load_player_stats(TRUE)

### columns 22 to 88

##############################
cols <- colnames(pstot)[18:79]
cols <- cols[!cols %in% c(
  "dream_team_points", "rating_points", "metres_gained", "last_updated",
  "extended_stats_centre_bounce_attendances", "extended_stats_kickins", "extended_stats_kickins_playon"
)]

cols_binom <- c(
  "time_on_ground_percentage", "disposal_efficiency", "goal_accuracy",
  "extended_stats_kick_efficiency", "extended_stats_contested_possession_rate", "extended_stats_hitout_win_percentage",
  "extended_stats_hitout_to_advantage_rate", "extended_stats_contest_def_loss_percentage", "extended_stats_contest_off_wins_percentage"
)
cols_pois <- setdiff(cols, cols_binom)

################
wav_data <- function(df, fil_val, model_col, decay = 365) {
  df$season_round <- paste0(substr(df$provider_id, 5, 8), substr(df$provider_id, 12, 13))

  df$opponent_name <- ifelse(df$team_status == "home", df$away_team_name, df$home_team_name)

  df_old <- df %>%
    filter(season_round < fil_val) %>%
    mutate(weight_gm = exp(as.numeric(-(max(as.Date(.data$utc_start_time)) - as.Date(.data$utc_start_time))) / decay))

  df_cur <- df %>% filter(season_round == fil_val)

  ###
  df_player <-
    df_old %>%
    group_by(
      player_id = player_player_player_player_id,
      # player_name = paste(player_player_player_given_name,player_player_player_surname)
    ) %>%
    summarise(
      wt_avg = round(sum(.data[[model_col]] * .data$weight_gm, na.rm = T) / sum(.data$weight_gm, na.rm = T), 3),
      wt_avg = replace_na(wt_avg, 0),
      log_wt_avg = log(wt_avg + 1),
      wt_gms = sum(.data$weight_gm, na.rm = T)
    ) %>%
    ungroup()

  ###
  df_team <-
    df_old %>%
    group_by(team_name) %>%
    summarise(
      wt_avg_team = round(sum(.data[[model_col]] * .data$weight_gm, na.rm = T) / sum(.data$weight_gm, na.rm = T), 3),
      wt_avg_team = replace_na(wt_avg_team, 0),
      log_wt_avg_team = log(wt_avg_team + 1),
      wt_gms_team = sum(.data$weight_gm, na.rm = T)
    ) %>%
    ungroup()

  ###
  df_opp <-
    df_old %>%
    group_by(opponent_name) %>%
    summarise(
      wt_avg_opp = round(sum(.data[[model_col]] * .data$weight_gm, na.rm = T) / sum(.data$weight_gm, na.rm = T), 3),
      wt_avg_opp = replace_na(wt_avg_opp, 0),
      log_wt_avg_opp = log(wt_avg_opp + 1),
      wt_gms_opp = sum(.data$weight_gm, na.rm = T)
    ) %>%
    ungroup()

  ###########
  df_tot <-
    df_player %>%
    right_join(df_cur, by = c("player_id" = "player_player_player_player_id")) %>%
    left_join(df_team, by = c("team_name" = "team_name")) %>%
    left_join(df_opp, by = c("opponent_name" = "opponent_name")) %>%
    mutate(
      round = round_round_number,
      season = as.factor(substr(provider_id, 5, 8)),
      venue = as.factor(venue_name),
      aest_start = with_tz(as_datetime(utc_start_time), "Australia/Brisbane"),
      date_numeric = as.numeric(aest_start),
      aest_hour = (hour(aest_start) * 60 + minute(aest_start)) / 60,
      aest_day = wday(aest_start, label = TRUE),
      position = as.factor(substr(player_player_position, 1, 2)),
      home_away = as.factor(team_status),
      player_name = paste(player_player_player_given_name, player_player_player_surname)
    ) %>%
    filter(
      position != "EM",
      !is.na(position)
    ) %>%
    relocate(any_of(model_col), position)

  return(df_tot)
}


############ POIS
stat_list <- list()

tictoc::tic()
for (i in cols_pois[1:length(cols_pois)]) { ############### DO 30 LATER!!!!!!!
  df_mdl <- purrr::map(
    paste0(rep(szns, each = 29), rep(sprintf("%02d", 0:28), times = length(szns))),
    ~ wav_data(pstot,
      fil_val = .,
      model_col = i
    )
  ) %>%
    purrr::list_rbind()


  mdl <- mgcv::bam(
    as.formula(paste0(
      i,
      " ~ ti(log_wt_avg,wt_gms, bs = 'ts') + s(log_wt_avg, bs='ts') + s(wt_gms, bs='ts')",
      "+ s(position,bs='re') + s(log_wt_avg_team, bs='ts') + s(log_wt_avg_opp, bs='ts') + s(home_away, bs='re')",
      "+ s(venue, bs='re') + s(round, bs='ts') + s(date_numeric, bs='ts', m=1) + s(aest_day, bs='re') + s(aest_hour, bs='ts')"
    )),
    data = df_mdl, family = poisson(),
    select = T, discrete = T, nthreads = 4,
  )

  model_preds <- tibble(
    player_id = df_mdl$player_id,
    player_name = df_mdl$player_name,
    player_position = df_mdl$player_player_position,
    provider_id = df_mdl$provider_id,
    round = df_mdl$round_round_number,
    home_away = df_mdl$home_away,
    team_name = df_mdl$team_name,
    opp_name = df_mdl$opponent_name,
    "{i}" := df_mdl[[i]],
    "pred_{i}" := mgcv::predict.bam(mdl, newdata = df_mdl, type = "response"),
    "wt_avg_{i}" := df_mdl$wt_avg,
    "wt_gms_{i}" := df_mdl$wt_gms,
    "wt_avg_team_{i}" := df_mdl$wt_avg_team,
    "wt_avg_opp_{i}" := df_mdl$wt_avg_opp
  )

  stat_list[[i]] <- model_preds

  saveRDS(mdl, glue::glue("./data-raw/stat-models/{i}.rds"))
  print(i)
}

tictoc::toc()

############ BINOM
# stat_list_binom <- list()

tictoc::tic()
for (i in cols_binom[1:length(cols_binom)]) {
  df_mdl <- purrr::map(
    paste0(rep(szns, each = 29), rep(sprintf("%02d", 0:28), times = length(szns))),
    ~ wav_data(
      pstot %>%
        mutate("{i}" := .data[[i]] / 100),
      fil_val = .,
      model_col = i
    )
  ) %>%
    purrr::list_rbind()


  mdl <- mgcv::bam(
    as.formula(paste0(
      i,
      " ~ ti(log_wt_avg,wt_gms, bs = 'ts') + s(log_wt_avg, bs='ts') + s(wt_gms, bs='ts')",
      "+ s(position,bs='re') + s(log_wt_avg_team, bs='ts', k=4) + s(log_wt_avg_opp, bs='ts', k=4) + s(home_away, bs='re')",
      "+ s(venue, bs='re') + s(round, bs='ts') + s(date_numeric, bs='ts', m=1) + s(aest_day, bs='re') + s(aest_hour, bs='ts')"
    )),
    data = df_mdl, family = binomial(),
    select = T, discrete = T, nthreads = 4,
  )

  model_preds <- tibble(
    player_id = df_mdl$player_id,
    player_name = df_mdl$player_name,
    player_position = df_mdl$player_player_position,
    provider_id = df_mdl$provider_id,
    round = df_mdl$round_round_number,
    home_away = df_mdl$home_away,
    team_name = df_mdl$team_name,
    opp_name = df_mdl$opponent_name,
    "{i}" := df_mdl[[i]],
    "pred_{i}" := mgcv::predict.bam(mdl, newdata = df_mdl, type = "response"),
    "wt_avg_{i}" := df_mdl$wt_avg,
    "wt_gms_{i}" := df_mdl$wt_gms,
    "wt_avg_team_{i}" := df_mdl$wt_avg_team,
    "wt_avg_opp_{i}" := df_mdl$wt_avg_opp
  )

  stat_list[[i]] <- model_preds

  saveRDS(mdl, glue::glue("./data-raw/stat-models/{i}.rds"))
  print(i)
}

tictoc::toc()

### combine
pred_df <- stat_list %>% reduce(left_join, by = c(
  "player_id", "player_name", "player_position",
  "provider_id", "round", "home_away", "team_name", "opp_name"
))
pred_df

arrow::write_parquet(pred_df, "./data-raw/outputs/stat_pred_df.parquet")
tictoc::toc()

#################################################################

pred_df <- arrow::read_parquet("./data-raw/outputs/stat_pred_df.parquet")

##############################
model_val <- "goals"
pred_model_val <- paste0("pred_", model_val)
wt_avg_model_val <- paste0("wt_avg_", model_val)

mdl <- readRDS(paste0("./data-raw/stat-models/", model_val, ".rds"))
summary(mdl)
mixedup::extract_random_effects(mdl) %>%
  arrange(-value) %>%
  View()
# plot(mgcViz::getViz(mdl))


############
stat_perf <- function(var) {
  model_val <- var
  pred_model_val <- paste0("pred_", model_val)
  wt_avg_model_val <- paste0("wt_avg_", model_val)
  sim_model_val <- paste0("sim_", model_val)

  test_df <- pred_df %>% filter(!is.na(.data[[pred_model_val]]))
  test_df <- test_df %>%
    mutate("sim_{model_val}" := rpois(nrow(test_df), .data[[pred_model_val]]))

  df <- tibble(
    var = paste(var),
    sim_rmse = ModelMetrics::rmse(test_df %>% pull(sim_model_val), test_df %>% pull(pred_model_val)),
    model_rmse = ModelMetrics::rmse(test_df %>% pull(model_val), test_df %>% pull(pred_model_val)),
    wt_avg_rmse = ModelMetrics::rmse(test_df %>% pull(model_val), test_df %>% pull(wt_avg_model_val)),
    naive_rmse = ModelMetrics::rmse(test_df %>% pull(model_val), rep(mean(test_df %>% pull(model_val)), nrow(test_df)))
  )

  return(df)
}

all_perf <- map(c(cols_pois, cols_binom), ~ stat_perf(.)) %>% list_rbind()

##### add to match pred data frame
team_preds <- pred_df %>%
  group_by(provider_id, team_name, opp_name) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) # %>% View()

team_mdl_df <- team_mdl_df %>%
  # mutate(team_name_adj = fitzRoy::replace_teams(team_name)) %>%
  left_join(team_preds %>% mutate(team_name_adj = fitzRoy::replace_teams(team_name)),
    by = c("providerId" = "provider_id", "team_name_adj.x" = "team_name")
  ) # %>% View()

###
colnames(team_mdl_df)[str_detect(colnames(team_mdl_df), "pred")]

###
pl_df_final <-
  pl_details %>%
  select(providerId, position) %>%
  left_join(pred_df %>% filter(substr(provider_id, 1, 13) == "CD_M202301423"),
    by = c("providerId" = "player_id")
  ) # %>%
# filter(position == "RUCK") %>%
# arrange(-pred_val) %>%
# relocate(pred_val) #%>%
# select(1:10) %>%
# group_by(position) %>%
# summarise(sqrt(var(pred_val, na.rm=T))) %>%
# View()

########
pl_df_final %>%
  group_by(position) %>%
  summarise(across(starts_with("pred"), ~ mean(.x, na.rm = T))) %>%
  view()


tst_df <- pred_df %>% filter(substr(provider_id,1,8)=='CD_M2025',round == 4) %>% select(1:50)
View(tst_df)
