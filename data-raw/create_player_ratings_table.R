###########
devtools::load_all()

teams <- torp::teams

chains <- load_chains(seasons = T, rounds = T)

model_data_wp <- load_pbp(seasons = T, rounds = T)

decay <- 500

######################################### REMEMBER TO UNCOMMENT ADD_WP_VARS ON ROW 30 AND 52
### player value rule of thumb:
### APY = torp * 135k + 100k
### so a replacement player is worth 100k, and someone with a torp of 7 is worth 7*150k+100k = 1.15m
### calculation is (teams cap ($13m) - team players (35) * min-salary (100k)) divided by mean "team torp rating" (70)
### (14m - 35*100k)/70 = 150,000
######
################### create player game df

plyr_gm_df <-
  model_data_wp %>% # add_wp_vars() %>%
  dplyr::arrange(match_id, display_order) %>%
  dplyr::select(
    player_name, player_id, match_id, utc_start_time, home_away, away_team_team_name, home_team_team_name,
    delta_epv, team, player_position, round_week, wpa
  ) %>%
  dplyr::mutate(
    weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay),
    opp_tm = ifelse(home_away == "Home", away_team_team_name, home_team_team_name)
  ) %>%
  dplyr::group_by(player_id, match_id) %>%
  dplyr::summarise(
    plyr_nm = max(player_name),
    gms = dplyr::n_distinct(match_id),
    # wt_gms = sum(unique(weight_gm), na.rm = TRUE),
    utc_start_time = max(utc_start_time),
    weight_gm = max(weight_gm),
    disp_pts = sum((delta_epv) / 2),
    disp_pts_wt = sum(delta_epv * max(weight_gm)) / 2,
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
      dplyr::select(
        lead_player, lead_player_id, match_id, utc_start_time, home_away, away_team_team_name, home_team_team_name,
        delta_epv, team, player_position, round_week, pos_team, wpa
      ) %>%
      dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay)) %>%
      dplyr::group_by(lead_player, lead_player_id, match_id) %>%
      dplyr::summarise(
        recv_pts = sum((dplyr::if_else(pos_team == -1, 1.5 * delta_epv * pos_team, delta_epv * pos_team)) / 2),
        recv_pts_wt = sum(delta_epv * pos_team * max(weight_gm)) / 2,
        recv_wpa = sum((wpa) / 2),
        recvs = dplyr::n()
      ),
    by = c("player_id" = "lead_player_id", "match_id" = "match_id")
  ) %>%
  dplyr::left_join(
    chains %>%
      dplyr::filter(description == "Spoil") %>%
      dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utcStartTime)) - as.Date(utcStartTime))) / decay)) %>%
      dplyr::group_by(playerId, matchId) %>%
      # dplyr::mutate() %>%
      dplyr::summarise(
        spoils = dplyr::n(),
        spoil_pts = spoils * 0.25,
        spoil_pts_wt = spoil_pts * max(weight_gm)
      ),
    by = c("player_id" = "playerId", "match_id" = "matchId")
  ) %>%
  dplyr::mutate(
    spoil_pts = tidyr::replace_na(spoil_pts, 0),
    spoil_pts_wt = tidyr::replace_na(spoil_pts_wt, 0),
    tot_p = recv_pts + disp_pts + spoil_pts,
    tot_p_wt = recv_pts_wt + disp_pts_wt + spoil_pts_wt,
    tot_wpa = recv_wpa + disp_wpa
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    tot_p_adj = tot_p - quantile(tot_p, 0.3, na.rm = T),
    recv_pts_adj = recv_pts - quantile(recv_pts, 0.3, na.rm = T),
    disp_pts_adj = disp_pts - quantile(disp_pts, 0.3, na.rm = T),
    spoil_pts_adj = spoil_pts - quantile(spoil_pts, 0.3, na.rm = T)
  ) %>%
  dplyr::relocate(tot_p_adj, disp) %>%
  dplyr::filter(!is.na(tm))

###
usethis::use_data(plyr_gm_df, overwrite = TRUE)
