# Setup ----
# library(fitzRoy)  # replaced by internal functions
library(tidyverse)
devtools::load_all()
skip_em <- "no"

teams <- load_teams(TRUE)

chains <- load_chains(seasons = T, rounds = T)

model_data_wp <- load_pbp(seasons = T, rounds = T) # (3 mins)

pl_details <- load_player_details(get_afl_season())

pstot <- load_player_stats(TRUE)

decay <- 500

### REMEMBER TO UNCOMMENT ADD_WP_VARS ON ROW 30 AND 52
### player value rule of thumb:
### APY = epr * 175k + 100k
### so a replacement player is worth 100k, and someone with an epr of 7 is worth 7*175k+100k = 1.325m
### calculation is (teams cap ($14m) - team players (35) * min-salary (100k)) divided by mean "team epr rating" (60)
### (14m - 35*100k)/60 = 175,000

# Create Player Game DataFrame ----
tictoc::tic()

plyr_gm_df <-
  model_data_wp %>% # add_wp_vars() %>%
  ###### filter(match_id == 'CD_M20230142107') %>%
  dplyr::arrange(match_id, display_order) %>%
  dplyr::select(
    player_name, player_id, match_id, utc_start_time, home_away, away_team_name, home_team_name,
    delta_epv, team, player_position, round_week, pos_team, wpa
  ) %>%
  dplyr::mutate(
    weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay),
    opp_tm = ifelse(home_away == "Home", away_team_name, home_team_name)
  ) %>%
  dplyr::group_by(player_id, match_id) %>%
  dplyr::summarise(
    player_name = max(player_name, na.rm = T),
    gms = dplyr::n_distinct(match_id),
    # wt_gms = sum(unique(weight_gm), na.rm = TRUE),
    utc_start_time = max(utc_start_time),
    weight_gm = max(weight_gm),
    epv_disp = sum(dplyr::if_else(pos_team == -1, delta_epv - 0.04, delta_epv + 0.08) / 2),
    epv_disp_wt = sum((dplyr::if_else(pos_team == -1, delta_epv - 0.04, delta_epv + 0.08) * max(weight_gm)) / 2),
    disp_wpa = sum((wpa) / 2),
    disposals_pbp = floor(dplyr::n() / 2),
    team = dplyr::last(team),
    opponent = dplyr::last(opp_tm),
    position_group = dplyr::last(player_position),
    round = as.numeric(dplyr::last(round_week)),
    season = dplyr::last(lubridate::year(utc_start_time))
  ) %>%
  dplyr::left_join(
    model_data_wp %>% # add_wp_vars() %>%
      # filter(match_id == 'CD_M20230142107') %>%
      dplyr::select(
        lead_player, lead_player_id, match_id, utc_start_time, home_away, away_team_name, home_team_name,
        delta_epv, team, player_position, round_week, pos_team, wpa
      ) %>%
      dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay)) %>%
      dplyr::group_by(lead_player, lead_player_id, match_id) %>%
      dplyr::summarise(
        epv_recv = sum(dplyr::if_else(pos_team == -1, (1.5 * delta_epv * pos_team) + 0.1, (1 * delta_epv * pos_team) + 0.05) / 2),
        epv_recv_wt = sum((dplyr::if_else(pos_team == -1, (1.5 * delta_epv * pos_team) + 0.1, ((1 * delta_epv * pos_team) + 0.05)) * max(weight_gm)) / 2),
        recv_wpa = sum((wpa) / 2),
        receptions = dplyr::n()
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
  #                         epv_spoil = spoils * 0.5,
  #                         epv_spoil_wt = epv_spoil * max(weight_gm)),
  #             by = c("player_id" = "playerId","match_id"="matchId")) %>%
  ##### HITOUTS + SPOILS
  dplyr::left_join(
    pstot %>%
      #   dplyr::select(
      #     spoils,pressure_acts,def_half_pressure_acts,
      #     hitouts,hitouts_to_advantage,ruck_contests
      # ) %>%
      dplyr::mutate(
        weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay),
        epv_spoil = spoils * 0.6 + tackles * 0.1 + pressure_acts * 0.1 - def_half_pressure_acts * 0.2,
        epv_spoil_wt = epv_spoil * max(weight_gm),
        epv_hitout = hitouts * 0.15 + hitouts_to_advantage * 0.25 - ruck_contests * 0.06,
        epv_hitout_wt = epv_hitout * max(weight_gm)
      ) %>%
      dplyr::select(-utc_start_time),
    by = c("player_id" = "player_id", "match_id" = "match_id")
  ) %>%
  dplyr::mutate(
    epv_recv = tidyr::replace_na(epv_recv, 0), # + 0.15 * effective_disposals - bounces * 0.5,
    epv_recv_wt = tidyr::replace_na(epv_recv_wt, 0),
    epv_disp = tidyr::replace_na(epv_disp, 0) - (bounces * 0.2),
    epv_disp_wt = tidyr::replace_na(epv_disp_wt, 0),
    epv_spoil = tidyr::replace_na(epv_spoil, 0),
    epv_spoil_wt = tidyr::replace_na(epv_spoil_wt, 0),
    epv_hitout = tidyr::replace_na(epv_hitout, 0),
    epv_hitout_wt = tidyr::replace_na(epv_hitout_wt, 0),
    epv = epv_recv + epv_disp + epv_spoil + epv_hitout,
    epv_wt = epv_recv_wt + epv_disp_wt + epv_spoil_wt + epv_hitout_wt,
    tot_wpa = recv_wpa + disp_wpa
  ) %>%
  dplyr::left_join(
    teams,
    by = c("match_id", "player_id")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(position) %>%
  dplyr::mutate(
    # epv_adj = epv - quantile(epv, 0.3, na.rm = T),
    epv_recv_adj = epv_recv - quantile(epv_recv, 0.4, na.rm = T),
    epv_disp_adj = epv_disp - quantile(epv_disp, 0.4, na.rm = T),
    epv_spoil_adj = epv_spoil - quantile(epv_spoil, 0.4, na.rm = T),
    epv_hitout_adj = epv_hitout - quantile(epv_hitout, 0.4, na.rm = T),
    epv_adj = epv_recv_adj + epv_disp_adj + epv_spoil_adj + epv_hitout_adj
  ) %>%
  ungroup() %>%
  mutate(season = season.x) %>%
  dplyr::relocate(epv_adj, disposals_pbp, epv_recv_adj, epv_disp_adj, epv_spoil_adj, epv_hitout_adj) %>%
  dplyr::filter(!is.na(team))

tictoc::toc()

# Save Data ----
usethis::use_data(plyr_gm_df, overwrite = TRUE)

### NEED TO GIT PUSH IF CHANGED THIS
