###########
teams <-
  fitzRoy::fetch_lineup(season = 2021, comp = "AFLM") %>%
  dplyr::bind_rows(fitzRoy::fetch_lineup(season = 2022, comp = "AFLM")) %>%
  dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)))

######
# chains_test <- load_chains(2021:2022)
#
# chains_clean <- chains_test %>%
#   janitor::clean_names() %>%
#   clean_pbp()
#
# df_test <- chains_clean %>%
#   clean_model_data_epv() %>%
#   add_epv_vars()
#
# df_test <- df_test %>%
#   clean_model_data_wp() %>%
#   add_wp_vars()

################### create player game df
plyr_gm_df <-
  model_data_wp %>% add_wp_vars() %>%
  dplyr::select(player_name, player_id, match_id,utc_start_time,home_away,away_team,home_team,
                     delta_epv,team,player_position,round_week,wpa) %>%
  dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / 365),
                opp_tm = ifelse(home_away=="Home",away_team,home_team),) %>%
  dplyr::group_by(player_name, player_id, match_id) %>%
  dplyr::summarise(
    gms = n_distinct(match_id),
    #wt_gms = sum(unique(weight_gm), na.rm = TRUE),
    utc_start_time = max(utc_start_time),
    weight_gm = max(weight_gm),
    disp_pts = sum((delta_epv) / 2),
    disp_pts_wt = sum(delta_epv * max(weight_gm)) / 2,
    disp_wpa = sum((wpa) / 2),
    disp = n(),
    tm = last(team),
    opp = last(opp_tm),
    pos = last(player_position),
    round = last(round_week),
    season = last(lubridate::year(utc_start_time))
  ) %>%
  dplyr::left_join(
    model_data_wp %>% add_wp_vars() %>%
      dplyr::select(lead_player, lead_player_id, match_id,utc_start_time,home_away,away_team,home_team,
                       delta_epv,team,player_position,round_week,pos_team,wpa) %>%
      dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / 365)) %>%
      dplyr::group_by(lead_player, lead_player_id,match_id) %>%
      dplyr::summarise(
        recv_pts = sum((delta_epv * pos_team) / 2),
        recv_pts_wt = sum(delta_epv * pos_team * max(weight_gm)) / 2,
        recv_wpa = sum((wpa) / 2),
        recvs = n()
      ),
    by = c("player_name" = "lead_player", "player_id" = "lead_player_id","match_id"="match_id")
  ) %>%
  dplyr::left_join(pbp %>%
              dplyr::filter(description == "Spoil") %>%
                dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / 365)) %>%
                group_by(player_name, player_id, match_id) %>%
                summarise(spoils = n(),
                          spoil_pts = spoils * 0.15,
                          spoil_pts_wt = spoil_pts * max(weight_gm)),
              by = c("player_name" = "player_name", "player_id" = "player_id","match_id"="match_id")) %>%
  dplyr::mutate(spoil_pts = tidyr::replace_na(spoil_pts,0) ,
                spoil_pts_wt = tidyr::replace_na(spoil_pts_wt,0) ,
                tot_p = recv_pts + disp_pts + spoil_pts,
                tot_p_wt = recv_pts_wt + disp_pts_wt + spoil_pts_wt,
                tot_wpa = recv_wpa + disp_wpa) %>%
  relocate(tot_p,disp) %>% ungroup()


###### need to change 'max(as.Date(utc_start_time))' as it doesn't account for regression that should happen between seasons
###### as round 1 filters for all games before gwk1 and therefore no regression happens
plyr_ratings <- function(player_df, team_df, season_val, round_val) {
  gwk <- sprintf("%02d", round_val) # keep this in case you change to round -1 or round +1
  match_ref <- paste0("CD_M", season_val, "014", gwk)

  plyr_df <- player_df %>% ungroup() %>%
    dplyr::filter(match_id <= match_ref) %>%
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / 365)
    ) %>%
    group_by(player_name,player_id) %>%
    dplyr::summarise(
      gms = n_distinct(match_id),
      wt_gms = sum(unique(weight_gm), na.rm = TRUE),
      tot_p_sum = sum(tot_p * weight_gm),
      tot_wpa_sum = sum(tot_wpa * weight_gm),
      tot_p_g = sum(tot_p * weight_gm) / wt_gms,
      tot_wpa_g = sum(tot_wpa * weight_gm) / wt_gms,
      recv_g = sum(recv_pts * weight_gm) / wt_gms,
      disp_g = sum(disp_pts * weight_gm) / wt_gms,
      bayes_g = sum(tot_p * weight_gm) / (wt_gms + 3),
      bayes_recv_g = sum(recv_pts * weight_gm) / (wt_gms + 3),
      bayes_disp_g = sum(disp_pts * weight_gm) / (wt_gms + 3),
      posn = last(pos),
      round = max(round_val),
      season = max(season_val)
    ) # %>% view()

  # team_df <- plyr_df %>% group_by(team) %>%
  #   summarise(team_tot = sum(tot_p)) %>%
  #   left_join(afl_total %>% filter(round_number <= gwk) %>% group_by(team) %>% summarise(gms = n_distinct(match_id))) %>%
  #   mutate(tot_g = team_tot / gms ) %>% arrange(-tot_g)

  final_df <- team_df %>%
    dplyr::filter(season == season_val, round.roundNumber == round_val) %>%
    dplyr::right_join(plyr_df, by = c("player.playerId" = "player_id")) %>%
    ungroup()

  # final <- bind_rows(final, teams2)
  print(round_val)
  return(final_df)
}

###############
this_week <- plyr_ratings(plyr_gm_df,teams,2022,22)
