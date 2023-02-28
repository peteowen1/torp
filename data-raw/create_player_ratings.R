###########
if (exists("teams") == FALSE) {
teams <-
  fitzRoy::fetch_lineup(season = 2021, comp = "AFLM") %>%
  dplyr::bind_rows(fitzRoy::fetch_lineup(season = 2022, comp = "AFLM")) %>%
  dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)))
}

if (exists("chains") == FALSE) {
  chains_21 <- purrr::map_df(1:27 ,~readRDS(glue::glue("./chain-data/chain_data_2021_{.}.rds")))
  chains_22 <- purrr::map_df(1:27 ,~readRDS(glue::glue("./chain-data/chain_data_2022_{.}.rds")))
  chains <- dplyr::bind_rows(chains_21,chains_22)
  rm(chains_21,chains_22)
}

if (exists("model_data_wp") == FALSE) {
model_data_wp_21 <- purrr::map_df(1:27 ,~readRDS(glue::glue("./pbp-data/pbp_data_2021_{.}.rds")))
model_data_wp_22 <- purrr::map_df(1:27 ,~readRDS(glue::glue("./pbp-data/pbp_data_2022_{.}.rds")))
model_data_wp <- dplyr::bind_rows(model_data_wp_21,model_data_wp_22)
rm(model_data_wp_21,model_data_wp_22)
}
######################################### REMEMBER TO UNCOMMENT ADD_WP_VARS ON ROW 30 AND 52
### player value rule of thumb:
### APY = bayes_g * 135k + 100k
### so a replacement player is worth 100k, and someone with a bayes_g of 7 is worth 7*150k+100k = 1.15m
### calculation is (teams cap ($13m) - team players (35) * min-salary (100k)) divided by mean "team bayes_g rating" (70)
### (14m - 35*100k)/70 = 150,000
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
decay <- 500

plyr_gm_df <-
  model_data_wp %>% #add_wp_vars() %>%
  dplyr::select(player_name, player_id, match_id,utc_start_time,home_away,away_team,home_team,
                     delta_epv,team,player_position,round_week,wpa) %>%
  dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay),
                opp_tm = ifelse(home_away=="Home",away_team,home_team)) %>%
  dplyr::group_by(player_name, player_id, match_id) %>%
  dplyr::summarise(
    gms = dplyr::n_distinct(match_id),
    #wt_gms = sum(unique(weight_gm), na.rm = TRUE),
    utc_start_time = max(utc_start_time),
    weight_gm = max(weight_gm),
    disp_pts = sum((delta_epv) / 2),
    disp_pts_wt = sum(delta_epv * max(weight_gm)) / 2,
    disp_wpa = sum((wpa) / 2),
    disp = floor(dplyr::n()/2),
    tm = dplyr::last(team),
    opp = dplyr::last(opp_tm),
    pos = dplyr::last(player_position),
    round = as.numeric(dplyr::last(round_week)),
    season = dplyr::last(lubridate::year(utc_start_time))
  ) %>%
  dplyr::left_join(
    model_data_wp %>% #add_wp_vars() %>%
      dplyr::select(lead_player, lead_player_id, match_id,utc_start_time,home_away,away_team,home_team,
                       delta_epv,team,player_position,round_week,pos_team,wpa) %>%
      dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay)) %>%
      dplyr::group_by(lead_player, lead_player_id,match_id) %>%
      dplyr::summarise( #### CHANGE TO IF_ELSE PLZ
        recv_pts = sum((ifelse(pos_team == -1, 1.5 * delta_epv * pos_team ,delta_epv * pos_team)) / 2),
        recv_pts_wt = sum(delta_epv * pos_team * max(weight_gm)) / 2,
        recv_wpa = sum((wpa) / 2),
        recvs = dplyr::n()
      ),
    by = c("player_name" = "lead_player", "player_id" = "lead_player_id","match_id"="match_id")
  ) %>%
  dplyr::left_join(chains %>%
              dplyr::filter(description == "Spoil") %>%
                dplyr::mutate(weight_gm = exp(as.numeric(-(max(as.Date(utcStartTime)) - as.Date(utcStartTime))) / decay)) %>%
                dplyr::group_by(playerId, matchId) %>%
                dplyr::mutate() %>%
                dplyr::summarise(spoils = dplyr::n(),
                          spoil_pts = spoils * 0.25,
                          spoil_pts_wt = spoil_pts * max(weight_gm)),
              by = c("player_id" = "playerId","match_id"="matchId")) %>%
  dplyr::mutate(spoil_pts = tidyr::replace_na(spoil_pts,0) ,
                spoil_pts_wt = tidyr::replace_na(spoil_pts_wt,0) ,
                tot_p = recv_pts + disp_pts + spoil_pts,
                tot_p_wt = recv_pts_wt + disp_pts_wt + spoil_pts_wt,
                tot_wpa = recv_wpa + disp_wpa) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(rep_p = 2, #quantile(tot_p,0.2),
         tot_p_adj = tot_p - rep_p) %>%
  dplyr::relocate(tot_p_adj,disp) %>%
  dplyr::filter(!is.na(tm))


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
    dplyr::group_by(player_name,player_id) %>%
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

###############
this_week <- plyr_ratings(plyr_gm_df,teams,2022,28) %>%
  dplyr::left_join(fitzRoy::fetch_player_details(),by = c("player.playerId" = "providerId"))

season_table <- plyr_gm_df %>%
  dplyr::filter(season==2022, round >= 1, round <= 28) %>%
  dplyr::group_by(player_name,player_id,tm,pos) %>%
  dplyr::summarise(tot_points = sum(tot_p_adj), g = dplyr::n(),p_g = tot_points/g) %>%
  dplyr::left_join(fitzRoy::fetch_player_details(),by = c("player_id" = "providerId")) %>%
  #dplyr::filter(year(dateOfBirth)>=2001) %>%
  dplyr::arrange(-tot_points) #%>% view()

# final_22 %>% filter(round.roundNumber==2) %>%
#   left_join(final_22 %>% filter(round.roundNumber==23),by = c('player.playerId'='player.playerId')) %>%
#   mutate(delta = bayes_g.y - bayes_g.x) %>%
#   relocate(delta,player_name.x,bayes_g.y,gms.x,gms.y) %>% view()
