library(furrr)
library(tidyverse)

# fil_val <- 24
decay <- 365

ps23 <- fetch_player_stats_afl(2023) %>% janitor::remove_constant() %>% janitor::clean_names()
ps22 <- fetch_player_stats_afl(2022) %>% janitor::remove_constant() %>% janitor::clean_names()
ps21 <- fetch_player_stats_afl(2021) %>% janitor::remove_constant() %>% janitor::clean_names()
ps20 <- fetch_player_stats_afl(2020) %>% janitor::remove_constant() %>% janitor::clean_names()
pstot <- bind_rows(ps20,ps21,ps22,ps23) ### columns 22 to 88

##############################
cols <- colnames(pstot)[18:79]
cols <- cols[!cols %in% c('dream_team_points','rating_points','metres_gained','last_updated')]

cols_binom <- c('time_on_ground_percentage','disposal_efficiency','goal_accuracy',
                'extended_stats_kick_efficiency','extended_stats_contested_possession_rate','extended_stats_hitout_win_percentage',
                'extended_stats_hitout_to_advantage_rate','extended_stats_contest_def_loss_percentage','extended_stats_contest_off_wins_percentage')
cols_pois <- setdiff(cols,cols_binom)


wav_data <- function(df,fil_val,model_col,decay = 150){

  df$season_round <- paste0(substr(df$provider_id,5,8),substr(df$provider_id,12,13))

  df_old <- df %>%
    filter(season_round < fil_val) %>%
    mutate(weight_gm = exp(as.numeric(-(max(as.Date(.data$utc_start_time)) - as.Date(.data$utc_start_time)))/ decay)
    )

  df_cur <- df %>% filter(season_round == fil_val)

  df_player <-
    df_old %>%
    group_by(player_id = player_player_player_player_id,
             #player_name = paste(player_player_player_given_name,player_player_player_surname)
             ) %>%
    summarise(wt_avg = round(sum(.data[[model_col]]*.data$weight_gm)/sum(.data$weight_gm),3),
              log_wt_avg = log(wt_avg+1),
              wt_gms = sum(.data$weight_gm)
    ) %>%
    ungroup()

  df_tot <-
    df_player %>%
    right_join(df_cur, by = c('player_id'='player_player_player_player_id')) %>%
    mutate(position = as.factor(substr(player_player_position,1,2))) %>%
    relocate(any_of(model_col),position)

  return(df_tot)
}

############
stat_list <- list()

tictoc::tic()
for (i in cols_pois) {

df_mdl <- furrr::future_map(paste0(2023,sprintf('%02d',1:24)),
               ~wav_data(pstot,
                         fil_val = .,
                         model_col = i )) %>%
                 purrr::list_rbind()


mdl <- mgcv::bam(as.formula(paste0(i,
                                   " ~ ti(log_wt_avg,wt_gms, bs = 'ts') + s(log_wt_avg, bs='ts') + s(wt_gms, bs='ts') + s(position,bs='re')"))
                 , data = df_mdl, family = poisson()
                 , select = T, discrete = T , nthreads = 4,
)

model_preds <- tibble(
                      player_id = df_mdl$player_id,
                      provider_id = df_mdl$provider_id,
                      team_id = df_mdl$team_id,
                      "{i}" := df_mdl[[i]],
                      "pred_{i}" := mgcv::predict.bam(mdl,newdata = df_mdl, type='response'),
                      "wt_avg_{i}" := df_mdl$wt_avg,
                      "wt_gms_{i}" := df_mdl$wt_gms,
                      )

stat_list[[i]] <- model_preds

saveRDS(mdl, glue::glue('./data-raw/stat-models/{i}.rds'))
print(i)
}
tictoc::toc()

pred_df <- stat_list %>% reduce(left_join, by= c('player_id','provider_id','team_id'))
pred_df

###############################
# plot(mgcViz::getViz(mdl))
# mixedup::extract_random_effects(mdl) %>% View()
model_val <- 'extended_stats_ruck_contests'
mdl <- readRDS(paste0('./data-raw/stat-models/',model_val,'.rds'))
summary(mdl)
mixedup::extract_random_effects(mdl) %>% arrange(-value)
plot(mgcViz::getViz(mdl))



