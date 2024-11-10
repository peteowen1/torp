#' Get TORP ratings
#'
#' This function calculates TORP (Total Overall Rating Points) for players based on their performance.
#'
#' @param season_val The season to calculate ratings for. Default is the next season.
#' @param round_val The round to calculate ratings for. Default is the next round.
#' @param decay The decay factor for weighting games. Default is 365.
#' @param loading The loading factor for TORP calculations. Default is 1.5.
#' @param prior_games_recv The number of prior games to consider for receiving. Default is 4.
#' @param prior_games_disp The number of prior games to consider for disposal. Default is 6.
#'
#' @return A data frame containing player TORP ratings.
#' @export
#'
#' @importFrom dplyr filter summarise pull ungroup mutate group_by n_distinct last arrange left_join select
#' @importFrom lubridate as_date decimal_date
#' @importFrom glue glue
#' @importFrom cli cli_abort
torp_ratings <- function(season_val = get_afl_season(type = "next"),
                         round_val = get_afl_week(type = "next"),
                         decay = 365,
                         loading = 1.5,
                         prior_games_recv = 4,
                         prior_games_disp = 6) {
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val)
  }

  if (is.null(plyr_gm_df)) {
    plyr_gm_df <- plyr_gm_df
  }

  gwk <- sprintf("%02d", round_val)
  match_ref <- paste0("CD_M", season_val, "014", gwk)

  date_val <- load_fixtures(season_val) %>%
    dplyr::filter(.data$compSeason.year == season_val, .data$round.roundNumber == round_val) %>%
    dplyr::summarise(lubridate::as_date(min(.data$utcStartTime))) %>%
    dplyr::pull()

  if (is.na(date_val)) {
    cli::cli_warn("Fixtures for this date not available yet")
    return(data.frame())
  } else {
    plyr_gm_df_rnd <- calculate_player_stats(plyr_gm_df, match_ref, date_val, decay, loading, prior_games_recv, prior_games_disp)

    final_df <- prepare_final_dataframe(plyr_tm_df, plyr_gm_df_rnd, season_val, round_val)

    message(glue::glue("TORP ratings as at {season_val} round {round_val}"))
    return(final_df)
  }
}

#' Calculate player statistics
#'
#' @param plyr_gm_df Player game data frame
#' @param match_ref Match reference
#' @param date_val Date value
#' @param decay Decay factor
#' @param loading Loading factor
#' @param prior_games_recv Prior games for receiving
#' @param prior_games_disp Prior games for disposal
#'
#' @return A data frame with calculated player statistics
#'
#' @importFrom dplyr filter mutate group_by summarise n_distinct last ungroup
calculate_player_stats <- function(plyr_gm_df = NULL, match_ref, date_val, decay, loading, prior_games_recv, prior_games_disp) {
  if (is.null(plyr_gm_df)) {
    plyr_gm_df <- plyr_gm_df
  }
  plyr_gm_df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$match_id <= match_ref) %>%
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(lubridate::as_date(date_val) - lubridate::as_date(.data$utc_start_time))) / decay)
    ) %>%
    dplyr::group_by(.data$player_id) %>%
    dplyr::summarise(
      player_name = max(.data$plyr_nm),
      gms = dplyr::n_distinct(.data$match_id),
      wt_gms = sum(unique(.data$weight_gm), na.rm = TRUE),
      tot_p_sum = sum(.data$tot_p_adj * .data$weight_gm),
      tot_p_g = sum(.data$tot_p_adj * .data$weight_gm) / .data$wt_gms,
      recv_g = sum(.data$recv_pts_adj * .data$weight_gm) / .data$wt_gms,
      disp_g = sum(.data$disp_pts_adj * .data$weight_gm) / .data$wt_gms,
      spoil_g = sum(.data$spoil_pts_adj * .data$weight_gm) / .data$wt_gms,
      hitout_g = sum(.data$hitout_pts_adj * .data$weight_gm) / .data$wt_gms,
      torp_recv = loading * (sum(.data$recv_pts_adj * .data$weight_gm) / (.data$wt_gms + prior_games_recv)),
      torp_disp = loading * (sum(.data$disp_pts_adj * .data$weight_gm) / (.data$wt_gms + prior_games_disp)),
      torp_spoil = loading * (1.2) * (sum(.data$spoil_pts_adj * .data$weight_gm) / (.data$wt_gms + prior_games_recv)),
      torp_hitout = loading * (sum(.data$hitout_pts_adj * .data$weight_gm) / (.data$wt_gms + prior_games_recv)),
      torp = round(.data$torp_recv + .data$torp_disp + .data$torp_spoil + .data$torp_hitout, 2),
      torp_recv_adj = round(.data$torp_recv, 2),
      torp_disp_adj = round(.data$torp_disp, 2),
      torp_spoil_adj = round(.data$torp_spoil, 2),
      torp_hitout_adj = round(.data$torp_hitout, 2),
      posn = dplyr::last(.data$pos),
      .groups = "drop"
    )
}

#' Prepare final dataframe
#'
#' @param plyr_tm_df Player team database
#' @param plyr_gm_df Player statistics dataframe
#' @param season_val Season value
#' @param round_val Round value
#'
#' @return A final dataframe with player ratings
#'
#' @importFrom dplyr filter left_join ungroup mutate select arrange
prepare_final_dataframe <- function(plyr_tm_df = NULL, plyr_gm_df = NULL, season_val, round_val) {
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val)
  }

  if (is.null(plyr_gm_df)) {
    plyr_gm_df <- plyr_gm_df
  }

  plyr_tm_df %>%
    dplyr::filter(.data$season == season_val) %>%
    dplyr::left_join(plyr_gm_df, by = c("providerId" = "player_id")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      round = round_val,
      season = season_val
    ) %>%
    dplyr::left_join(fixtures %>%
      dplyr::group_by(season = .data$compSeason.year, round = .data$round.roundNumber) %>%
      dplyr::summarise(ref_date = lubridate::as_date(min(.data$utcStartTime)), .groups = "drop")) %>%
    dplyr::mutate(
      age = lubridate::decimal_date(lubridate::as_date(.data$ref_date)) -
        lubridate::decimal_date(lubridate::as_date(.data$dateOfBirth))
    ) %>%
    dplyr::select(
      player_id = .data$providerId, player_name = .data$player_name.x, age = .data$age, team = .data$team,
      torp = .data$torp, torp_recv = .data$torp_recv_adj, torp_disp = .data$torp_disp_adj,
      torp_spoil = .data$torp_spoil_adj, torp_hitout = .data$torp_hitout_adj,
      position = .data$position, season = .data$season, round = .data$round, gms = .data$gms, wt_gms = .data$wt_gms
    ) %>%
    dplyr::arrange(-.data$torp)
}

#' Get game ratings
#'
#' This function retrieves game ratings for players based on specified criteria.
#'
#' @param season_val The season to get ratings for. Default is the current season.
#' @param round_num The round number to get ratings for. Default is the current round.
#' @param matchid The match ID to filter by. Default is FALSE (no filtering).
#' @param team The team to filter by. Default is FALSE (no filtering).
#'
#' @return A data frame containing player game ratings.
#' @export
#'
#' @importFrom dplyr filter arrange mutate select
player_game_ratings <- function(season_val = get_afl_season(),
                                round_num = get_afl_week(),
                                matchid = FALSE,
                                team = FALSE) {
  df <- filter_game_data(plyr_gm_df, season_val, round_num, matchid, team)

  df %>%
    dplyr::arrange(-.data$tot_p_adj) %>%
    dplyr::mutate(
      total_points = round(.data$tot_p_adj, 1),
      recv_points = round(.data$recv_pts_adj, 1),
      disp_points = round(.data$disp_pts_adj, 1),
      spoil_points = round(.data$spoil_pts_adj, 1),
      hitout_points = round(.data$hitout_pts_adj, 1)
    ) %>%
    dplyr::select(
      season = .data$season, round = .data$round,
      player_name = .data$plyr_nm, position = .data$pos, team_id = .data$team_id, team = .data$tm, opp = .data$opp,
      total_points = .data$total_points, recv_points = .data$recv_points, disp_points = .data$disp_points,
      spoil_points = .data$spoil_points, hitout_points = .data$hitout_points,
      player_id = .data$player_id, match_id = .data$match_id
    )
}

#' Filter game data
#'
#' @param df Input data frame
#' @param season_val Season value
#' @param round_num Round number
#' @param matchid Match ID
#' @param team Team name
#'
#' @return Filtered data frame
#'
#' @importFrom dplyr filter
#' @importFrom cli cli_abort
filter_game_data <- function(df, season_val, round_num, matchid, team) {
  if (matchid != FALSE) {
    df <- df %>% dplyr::filter(.data$match_id %in% matchid)
    if (nrow(df) == 0) {
      cli::cli_abort("Match ID not found")
    }
  } else if (team != FALSE) {
    df <- df %>% dplyr::filter(
      .data$season %in% season_val,
      .data$round %in% round_num,
      (.data$tm == team | .data$opp == team)
    )
    if (nrow(df) == 0) {
      cli::cli_abort("Team not found. Please use one of: Adelaide Crows, Brisbane Lions, Carlton, Collingwood, Essendon, Fremantle, Geelong Cats, Gold Coast Suns, GWS Giants, Hawthorn, Melbourne, North Melbourne, Port Adelaide, Richmond, St Kilda, Sydney Swans, West Coast Eagles, Western Bulldogs")
    }
  } else {
    df <- df %>% dplyr::filter(
      .data$season %in% season_val,
      .data$round %in% round_num
    )
  }
  return(df)
}

#' Get season total ratings
#'
#' This function calculates season total ratings for players.
#'
#' @param season_val The season to calculate ratings for. Default is the current season.
#' @param round_num The round number to calculate ratings for. Default is NA (all rounds).
#'
#' @return A data frame containing player season total ratings.
#' @export
#'
#' @importFrom dplyr group_by summarise arrange n
player_season_ratings <- function(season_val = get_afl_season(), round_num = NA) {
  df <- get_season_data(season_val, round_num)

  df %>%
    dplyr::group_by(.data$season, .data$player_name, .data$player_id, .data$team_id) %>%
    dplyr::summarise(
      team = get_mode(.data$team),
      position = max(.data$position),
      games = dplyr::n(),
      season_points = sum(.data$total_points),
      season_recv = sum(.data$recv_points),
      season_disp = sum(.data$disp_points),
      season_spoil = sum(.data$spoil_points),
      season_hitout = sum(.data$hitout_points),
      ppg = .data$season_points / .data$games,
      .groups = "drop"
    ) %>%
    dplyr::arrange(-.data$season_points)
}

#' Get season data
#'
#' @param season_val Season value
#' @param round_num Round number
#'
#' @return Season data frame
get_season_data <- function(season_val, round_num) {
  if (season_val < get_afl_season()) {
    round_range <- if (any(is.na(round_num))) 0:99 else round_num
  } else {
    round_range <- if (any(is.na(round_num))) 0:get_afl_week() else round_num
  }

  player_game_ratings(season_val, round_range)
}
