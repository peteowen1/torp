###### need to change 'max(as.Date(utc_start_time))' as it doesn't account for regression that should happen between seasons
###### as round 1 filters for all games before gwk1 and therefore no regression happens
plyr_ratings <- function(player_df, team_df, season_val, round_val,decay = 500,prior_games = 4) {
  gwk <- sprintf("%02d", round_val) # keep this in case you change to round -1 or round +1
  match_ref <- paste0("CD_M", season_val, "014", gwk)
  date_val <- team_df %>%
    dplyr::filter(season == season_val, round.roundNumber == round_val) %>%
    dplyr::summarise(max(utcStartTime)) %>%
    dplyr::pull()

  if (is.na(date_val)) {
    date_val <- Sys.Date()  #max(team_df$utcStartTime)
  }

  plyr_df <- player_df %>%
    dplyr::ungroup() %>%
    dplyr::filter(match_id <= match_ref) %>%
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(as.Date(date_val) - as.Date(utc_start_time))) / decay)
    ) %>%
    dplyr::group_by(plyr_nm,player_id) %>%
    dplyr::summarise(
      gms = dplyr::n_distinct(match_id),
      wt_gms = sum(unique(weight_gm), na.rm = TRUE),
      tot_p_sum = sum(tot_p_adj * weight_gm),
      tot_wpa_sum = sum(tot_wpa * weight_gm),
      tot_p_g = sum(tot_p_adj * weight_gm) / wt_gms,
      tot_wpa_g = sum(tot_wpa * weight_gm) / wt_gms,
      recv_g = sum(recv_pts * weight_gm) / wt_gms,
      disp_g = sum(disp_pts * weight_gm) / wt_gms,
      bayes_g = sum(tot_p_adj * weight_gm) / (wt_gms + prior_games),
      bayes_recv_g = sum(recv_pts * weight_gm) / (wt_gms + prior_games),
      bayes_disp_g = sum(disp_pts * weight_gm) / (wt_gms + prior_games),
      posn = dplyr::last(pos),
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
    dplyr::ungroup()

  # final <- bind_rows(final, teams2)
  print(round_val)
  return(final_df)
}
