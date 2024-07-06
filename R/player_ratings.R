###### need to change 'max(as.Date(utc_start_time))' as it doesn't account for regression that should happen between seasons
###### as round 1 filters for all games before gwk1 and therefore no regression happens

#' Get TORP ratings
#'
#' @param season_val
#' @param round_num
#' @param decay
#' @param prior_games
#'
#' @export
torp_ratings <- function(season_val = get_afl_season(type = "next"),
                         round_val = get_afl_week(type = "next"),
                         decay = 365,
                         loading = 1.5,
                         prior_games_recv = 3,
                         prior_games_disp = 6
                         ) {
  gwk <- sprintf("%02d", round_val) # keep this in case you change to round -1 or round +1
  match_ref <- paste0("CD_M", season_val, "014", gwk)
  date_val <- fixtures %>%
    dplyr::filter(compSeason.year == season_val, round.roundNumber == round_val) %>%
    dplyr::summarise(lubridate::as_date(min(utcStartTime))) %>%
    dplyr::pull()

  if (is.na(date_val)) {
    cli::cli_abort("fixtures for this date not available yet") # max(team_df$utcStartTime)
  }

  plyr_df <- plyr_gm_df %>%
    dplyr::ungroup() %>%
    dplyr::filter(match_id <= match_ref) %>%
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(as.Date(date_val) - as.Date(utc_start_time))) / decay)
    ) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      player_name = max(plyr_nm),
      gms = dplyr::n_distinct(match_id),
      wt_gms = sum(unique(weight_gm), na.rm = TRUE),
      tot_p_sum = sum(tot_p_adj * weight_gm),
      # tot_wpa_sum = sum(tot_wpa * weight_gm),
      tot_p_g = sum(tot_p_adj * weight_gm) / wt_gms,
      # tot_wpa_g = sum(tot_wpa * weight_gm) / wt_gms,
      recv_g = sum(recv_pts_adj * weight_gm) / wt_gms,
      disp_g = sum(disp_pts_adj * weight_gm) / wt_gms,
      spoil_g = sum(spoil_pts_adj * weight_gm) / wt_gms,
      hitout_g = sum(hitout_pts_adj * weight_gm) / wt_gms,
      torp_recv = loading * (sum(recv_pts_adj * weight_gm) / (wt_gms + prior_games_recv)),
      torp_disp = loading * (sum(disp_pts_adj * weight_gm) / (wt_gms + prior_games_disp)),
      torp_spoil = loading * (1.2) * (sum(spoil_pts_adj * weight_gm) / (wt_gms + prior_games_recv)),
      torp_hitout = loading *(sum(hitout_pts_adj * weight_gm) / (wt_gms + prior_games_recv)),
      # torp_recv_old = sum(recv_pts_adj * weight_gm) / (wt_gms + 4),
      # torp_disp_old = sum(disp_pts_adj * weight_gm) / (wt_gms + 4),
      # torp_spoil_old = sum(spoil_pts_adj * weight_gm) / (wt_gms + 4),
      # torp_hitout_old = sum(hitout_pts_adj * weight_gm) / (wt_gms + 4),
      torp = round(torp_recv + torp_disp + torp_spoil + torp_hitout,2),
      torp_recv_adj = round(torp_recv, 2),
      torp_disp_adj = round(torp_disp, 2),
      torp_spoil_adj = round(torp_spoil, 2),
      torp_hitout_adj = round(torp_hitout, 2),
      # torp_old = sum(tot_p_adj * weight_gm) / (wt_gms + 4),
      # torp_ratio = pmax(pmin(torp_old / (torp_recv_old + torp_disp_old + torp_spoil_old + torp_hitout_old), 1), -1),
      # torp_recv_adj1 = torp_recv_old * torp_ratio,
      # torp_disp_adj1 = torp_disp_old * torp_ratio,
      # torp_spoil_adj1 = torp_spoil_old * torp_ratio,
      # torp_hitout_adj1 = torp_hitout_old * torp_ratio,
      # torp_diff = torp_old - (torp_recv_adj1 + torp_disp_adj1 + torp_spoil_adj1 + torp_hitout_adj1),
      # torp_recv_adj = round((torp_recv_adj1 + torp_diff / 4) * loading, 2),
      # torp_disp_adj = round((torp_disp_adj1 + torp_diff / 4) * loading, 2),
      # torp_spoil_adj = round((torp_spoil_adj1 + torp_diff / 4) * loading, 2),
      # torp_hitout_adj = round((torp_hitout_adj1 + torp_diff / 4) * loading, 2),
      torp = round(torp, 2),
      # torp_old = round(torp_old * loading, 2),
      posn = dplyr::last(pos)
    ) #%>%
    # mutate(torp = case_when(
    #   posn == "KEY_DEFENDER" ~ torp * 1.4,
    #   posn == "KEY_FORWARD" ~ torp * 0.9,
    #   posn == "MEDIUM_DEFENDER" ~ torp * 0.9,
    #   posn == "MEDIUM_FORWARD" ~ torp * 0.7,
    #   posn == "MIDFIELDER" ~ torp * 1.1,
    #   posn == "MIDFIELDER_FORWARD" ~ torp * 0.7,
    #   posn == "RUCK" ~ torp * 1.8,
    #   TRUE ~ NA_real_
    # ))


  final_df <- plyr_tm_db %>%
    dplyr::filter(season == season_val) %>%
    dplyr::left_join(plyr_df, by = c("providerId" = "player_id")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      round = round_val,
      season = season_val
    ) %>%
    dplyr::left_join(fixtures %>%
      dplyr::group_by(season = compSeason.year, round = round.roundNumber) %>%
      dplyr::summarise(ref_date = lubridate::as_date(min(utcStartTime)))) %>%
    dplyr::mutate(
      age =
        lubridate::decimal_date(lubridate::as_date(ref_date)) -
          lubridate::decimal_date(lubridate::as_date(dateOfBirth))
    ) %>%
    dplyr::select(
      player_id = providerId, player_name = player_name.x, age, team,
      torp,
      # torp_old,
      torp_recv = torp_recv_adj,
      torp_disp = torp_disp_adj,
      torp_spoil = torp_spoil_adj,
      torp_hitout = torp_hitout_adj,
      position = position, season, round,
      gms , wt_gms
    ) %>%
    dplyr::arrange(-torp)

  # final <- bind_rows(final, teams2)
  print(glue::glue("TORP ratings as at {season_val} round {round_val}"))
  return(final_df)
}

#' Get game ratings
#'
#'
#' @param season_val
#' @param round_num
#' @param matchid
#' @param team
#'
#' @export

player_game_ratings <- function(season_val = get_afl_season(),
                                round_num = get_afl_week(),
                                matchid = FALSE,
                                team = FALSE) {
  if (matchid != FALSE) {
    df <- plyr_gm_df %>%
      dplyr::filter(match_id %in% matchid)
    if (nrow(df) == 0) {
      cli::cli_abort("Match ID not found")
    }
  }

  if (matchid == FALSE) {
    if (team == FALSE) {
      df <- plyr_gm_df %>%
        dplyr::filter(
          season %in% season_val,
          round %in% round_num
        )
    }
    if (team != FALSE) {
      df <- plyr_gm_df %>%
        dplyr::filter(
          season %in% season_val,
          round %in% round_num,
          (tm == team | opp == team)
        )
      if (nrow(df) == 0) {
        cli::cli_abort("Team not found please use one of: Adelaide Crows, Brisbane Lions, Carlton, Collingwood,
                       Essendon, Fremantle, Geelong Cats, Gold Coast Suns, GWS Giants, Hawthorn, Melbourne,
                       North Melbourne, Port Adelaide, Richmond, St Kilda, Sydney Swans, West Coast Eagles, Western Bulldogs")
      }
    }
  }

  final <- df %>%
    dplyr::arrange(-tot_p_adj) %>%
    dplyr::mutate(
      total_points = round(tot_p_adj, 1),
      recv_points = round(recv_pts_adj, 1),
      disp_points = round(disp_pts_adj, 1),
      spoil_points = round(spoil_pts_adj, 1),
      hitout_points = round(hitout_pts_adj, 1)
    ) %>%
    dplyr::select(season, round,
      player_name = plyr_nm, position = pos, team_id, team = tm, opp,
      total_points, recv_points, disp_points, spoil_points,hitout_points, player_id, match_id
    )

  return(final)
}


#' Get season total ratings
#'
#'
#' @param season_val
#'
#' @export

player_season_ratings <- function(season_val = get_afl_season(), round_num = NA) {
  df <- dplyr::tibble()

  summary_func <- function(x){
    dplyr::summarise(x,
    team = get_mode(team),
    position = max(position),
    games = dplyr::n(),
    season_points = sum(total_points),
    season_recv = sum(recv_points),
    season_disp = sum(disp_points),
    season_spoil = sum(spoil_points),
    season_hitout = sum(hitout_points),
    ppg = season_points/games
    )
  }

  if (season_val < get_afl_season()) {
    if (any(is.na(round_num))) {
      df <- player_game_ratings(season_val, 0:99) %>%
        dplyr::group_by(season, player_name, player_id, team_id) %>%
        summary_func() %>%
        dplyr::arrange(-season_points)
    }
    if (!any(is.na(round_num))) {
      df <- player_game_ratings(season_val, round_num) %>%
        dplyr::group_by(season, player_name, player_id, team_id) %>%
        summary_func() %>%
        dplyr::arrange(-season_points)
    }
  }

  if (season_val == get_afl_season()) {
    if (any(is.na(round_num))) {
      df <- player_game_ratings(season_val, 0:get_afl_week()) %>%
        dplyr::group_by(season, player_name, player_id, team_id) %>%
        summary_func() %>%
        dplyr::arrange(-season_points)
    }
    if (!any(is.na(round_num))) {
      df <- player_game_ratings(season_val, round_num) %>%
        dplyr::group_by(season, player_name, player_id, team_id) %>%
        summary_func() %>%
        dplyr::arrange(-season_points)
    }
  }

  return(df)
}
