# ###########
library(fitzRoy)
library(tidyverse)
devtools::load_all()
skip_em <- "no"

teams <- load_teams(TRUE)

if (skip_em == "no") {
  chains <- load_chains(seasons = T, rounds = T)

  model_data_wp <- load_pbp(seasons = T, rounds = T) # (3 mins)

  pl_details <- load_player_details(get_afl_season())

  pstot <- load_player_stats(TRUE)
}

decay <- 500

######################################### REMEMBER TO UNCOMMENT ADD_WP_VARS ON ROW 30 AND 52
### player value rule of thumb:
### APY = torp * 175k + 100k
### so a replacement player is worth 100k, and someone with a torp of 7 is worth 7*175k+100k = 1.325m
### calculation is (teams cap ($14m) - team players (35) * min-salary (100k)) divided by mean "team torp rating" (60)
### (14m - 35*100k)/60 = 175,000
######
################### create player game df ()
tictoc::tic()

plyr_gm_df <-
  model_data_wp %>% # add_wp_vars() %>%
  ###### filter(match_id == 'CD_M20230142107') %>%
  dplyr::arrange(match_id, display_order) %>%
  dplyr::select(
    player_name, player_id, match_id, utc_start_time, home_away, away_team_team_name, home_team_team_name,
    delta_epv, team, player_position, round_week, pos_team, wpa
  ) %>%
  dplyr::mutate(
    weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay),
    opp_tm = ifelse(home_away == "Home", away_team_team_name, home_team_team_name)
  ) %>%
  dplyr::group_by(player_id, match_id) %>%
  dplyr::summarise(
    plyr_nm = max(player_name, na.rm = T),
    gms = dplyr::n_distinct(match_id),
    # wt_gms = sum(unique(weight_gm), na.rm = TRUE),
    utc_start_time = max(utc_start_time),
    weight_gm = max(weight_gm),
    disp_pts = sum(dplyr::if_else(pos_team == -1, delta_epv - 0.04, delta_epv + 0.08) / 2),
    disp_pts_wt = sum((dplyr::if_else(pos_team == -1, delta_epv - 0.04, delta_epv + 0.08) * max(weight_gm)) / 2),
    disp_wpa = sum((wpa) / 2),
    disp = floor(dplyr::n() / 2),
    tm = dplyr::last(team),
    opp = dplyr::last(opp_tm),
    pos = dplyr::last(player_position),
    round = as.numeric(dplyr::last(round_week)),
    season = dplyr::last(lubridate::year(utc_start_time))
  ) %>%
  dplyr::left_join(
    model_data_wp %>% # add_wp_vars() %>%
      # filter(match_id == 'CD_M20230142107') %>%
      dplyr::select(
        lead_player, lead_player_id, match_id, utc_start_time, home_away, away_team_team_name, home_team_team_name,
        delta_epv, team, player_position, round_week, pos_team, wpa
      ) %>%
      dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay)) %>%
      dplyr::group_by(lead_player, lead_player_id, match_id) %>%
      dplyr::summarise(
        recv_pts = sum(dplyr::if_else(pos_team == -1, (1.5 * delta_epv * pos_team) + 0.1, (1 * delta_epv * pos_team) + 0.05) / 2),
        recv_pts_wt = sum((dplyr::if_else(pos_team == -1, (1.5 * delta_epv * pos_team) + 0.1, ((1 * delta_epv * pos_team) + 0.05)) * max(weight_gm)) / 2),
        recv_wpa = sum((wpa) / 2),
        recvs = dplyr::n()
      ),
    by = c("player_id" = "lead_player_id", "match_id" = "match_id")
  ) %>%
  # ##### SPOILS
  # dplyr::left_join(chains %>%
  #             dplyr::filter(description == "Spoil") %>%
  #               dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utcStartTime)) - as.Date(utcStartTime))) / decay)) %>%
  #               dplyr::group_by(playerId, matchId) %>%
  #               #dplyr::mutate() %>%
  #               dplyr::summarise(spoils = dplyr::n(),
  #                         spoil_pts = spoils * 0.5,
  #                         spoil_pts_wt = spoil_pts * max(weight_gm)),
  #             by = c("player_id" = "playerId","match_id"="matchId")) %>%
  ##### HITOUTS + SPOILS
  dplyr::left_join(
    pstot %>%
      #   dplyr::select(
      #     extended_stats_spoils,extended_stats_pressure_acts,extended_stats_def_half_pressure_acts,
      #     hitouts,extended_stats_hitouts_to_advantage,extended_stats_ruck_contests
      # ) %>%
      dplyr::mutate(
        weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay),
        spoil_pts = extended_stats_spoils * 0.6 + tackles * 0.1 + extended_stats_pressure_acts * 0.1 - extended_stats_def_half_pressure_acts * 0.2, # HMMMMMMMMM
        spoil_pts_wt = spoil_pts * max(weight_gm),
        hitout_pts = hitouts * 0.15 + extended_stats_hitouts_to_advantage * 0.25 - extended_stats_ruck_contests * 0.06,
        hitout_pts_wt = hitout_pts * max(weight_gm)
      ) %>%
      dplyr::select(-utc_start_time),
    by = c("player_id" = "player_player_player_player_id", "match_id" = "provider_id")
  ) %>%
  dplyr::mutate(
    recv_pts = tidyr::replace_na(recv_pts, 0), # + 0.15 * extended_stats_effective_disposals - bounces * 0.5,
    recv_pts_wt = tidyr::replace_na(recv_pts_wt, 0),
    disp_pts = tidyr::replace_na(disp_pts, 0) - (bounces * 0.2),
    disp_pts_wt = tidyr::replace_na(disp_pts_wt, 0),
    spoil_pts = tidyr::replace_na(spoil_pts, 0),
    spoil_pts_wt = tidyr::replace_na(spoil_pts_wt, 0),
    hitout_pts = tidyr::replace_na(hitout_pts, 0),
    hitout_pts_wt = tidyr::replace_na(hitout_pts_wt, 0),
    tot_p = recv_pts + disp_pts + spoil_pts + hitout_pts,
    tot_p_wt = recv_pts_wt + disp_pts_wt + spoil_pts_wt + hitout_pts_wt,
    tot_wpa = recv_wpa + disp_wpa
  ) %>%
  dplyr::left_join(
    teams,
    by = c("match_id" = "providerId", "player_id" = "player.playerId")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(position) %>%
  dplyr::mutate(
    # tot_p_adj = tot_p - quantile(tot_p, 0.3, na.rm = T),
    recv_pts_adj = recv_pts - quantile(recv_pts, 0.4, na.rm = T),
    disp_pts_adj = disp_pts - quantile(disp_pts, 0.4, na.rm = T),
    spoil_pts_adj = spoil_pts - quantile(spoil_pts, 0.4, na.rm = T),
    hitout_pts_adj = hitout_pts - quantile(hitout_pts, 0.4, na.rm = T),
    tot_p_adj = recv_pts_adj + disp_pts_adj + spoil_pts_adj + hitout_pts_adj
  ) %>%
  ungroup() %>%
  mutate(season = season.x) %>%
  dplyr::relocate(tot_p_adj, disp, recv_pts_adj, disp_pts_adj, spoil_pts_adj, hitout_pts_adj) %>%
  dplyr::filter(!is.na(tm))

tictoc::toc()
###
usethis::use_data(plyr_gm_df, overwrite = TRUE)

### NEED TO GIT PUSH IF CHANGED THIS
