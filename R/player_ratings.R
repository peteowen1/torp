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
#' @param plyr_tm_df Optional pre-loaded player team data. If NULL, will load automatically.
#' @param player_game_data Optional pre-loaded player game data. If NULL, will load automatically.
#' @param prior_games_spoil Prior games for spoil shrinkage. Default is \code{RATING_PRIOR_GAMES_SPOIL}.
#' @param prior_games_hitout Prior games for hitout shrinkage. Default is \code{RATING_PRIOR_GAMES_HITOUT}.
#'
#' @return A data frame containing player TORP ratings.
#' @export
#'
#' @importFrom dplyr filter summarise pull ungroup mutate group_by n_distinct arrange left_join select
#' @importFrom lubridate as_date decimal_date
#' @importFrom cli cli_abort
#' @importFrom utils data
calculate_torp_ratings <- function(season_val = get_afl_season(type = "current"),
                         round_val = get_afl_week(type = "next"),
                         decay = RATING_DECAY_DEFAULT_DAYS,
                         loading = RATING_LOADING_DEFAULT,
                         prior_games_recv = RATING_PRIOR_GAMES_RECV,
                         prior_games_disp = RATING_PRIOR_GAMES_DISP,
                         plyr_tm_df = NULL,
                         player_game_data = NULL,
                         prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
                         prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
                         fixtures = NULL) {
  # Load player team details if not provided
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val)
    # Fall back to previous season if current season data unavailable
    if (nrow(plyr_tm_df) == 0 || !"season" %in% names(plyr_tm_df)) {
      plyr_tm_df <- load_player_details(season_val - 1)
    }
  }

  # Load player game data if not provided
  if (is.null(player_game_data)) {
    player_game_data <- load_player_game_data(TRUE)
  }

  # Load fixtures if not provided
  if (is.null(fixtures)) {
    fixtures <- load_fixtures(TRUE)
  }

  gwk <- sprintf("%02d", round_val)
  match_ref <- paste0("CD_M", season_val, "014", gwk)

  date_val <- fixtures |>
    dplyr::filter(.data$compSeason.year == season_val, .data$round.roundNumber == round_val) |>
    dplyr::summarise(lubridate::as_date(min(.data$utcStartTime))) |>
    dplyr::pull()

  if (is.na(date_val)) {
    cli::cli_warn("Fixtures for this date not available yet")
    return(data.frame())
  } else {
    plyr_gm_df_rnd <- calculate_player_stats(player_game_data, match_ref, date_val, decay, loading, prior_games_recv, prior_games_disp, prior_games_spoil = prior_games_spoil, prior_games_hitout = prior_games_hitout)

    final_df <- prepare_final_dataframe(plyr_tm_df, plyr_gm_df_rnd, season_val, round_val)

    cli::cli_inform("TORP ratings as at {season_val} round {round_val}")
    return(final_df)
  }
}

#' Calculate player statistics
#'
#' @param player_game_data Player game data frame
#' @param match_ref Match reference
#' @param date_val Date value
#' @param decay Decay factor
#' @param loading Loading factor
#' @param prior_games_recv Prior games for receiving
#' @param prior_games_disp Prior games for disposal
#' @param prior_games_spoil Prior games for spoil shrinkage. Default is \code{RATING_PRIOR_GAMES_SPOIL}.
#' @param prior_games_hitout Prior games for hitout shrinkage. Default is \code{RATING_PRIOR_GAMES_HITOUT}.
#'
#' @return A data frame with calculated player statistics
#'
#' @importFrom data.table as.data.table uniqueN setDT
calculate_player_stats <- function(player_game_data = NULL, match_ref, date_val, decay, loading, prior_games_recv, prior_games_disp, prior_games_spoil = RATING_PRIOR_GAMES_SPOIL, prior_games_hitout = RATING_PRIOR_GAMES_HITOUT) {
  if (is.null(player_game_data)) {
    player_game_data <- load_player_game_data(TRUE)
  }

  # Convert to data.table (makes a copy to avoid modifying original)
  dt <- data.table::as.data.table(player_game_data)

  # Set key for efficient filtering and grouping
  data.table::setkey(dt, match_id)

  # Filter and add computed columns
  dt <- dt[match_id <= match_ref]
  dt[, weight_gm := exp(as.numeric(-(as.Date(date_val) - as.Date(utc_start_time))) / decay)]

  # Handle plyr_nm: use existing if present, otherwise construct from given + surname
  if (!"plyr_nm" %in% names(dt)) {
    dt[, plyr_nm := paste(player_given_name, player_surname)]
  }

  # Aggregate by player_id using data.table syntax

  # First pass: compute base aggregations
  result <- dt[, .(
    player_name = max(plyr_nm),
    gms = data.table::uniqueN(match_id),
    wt_gms = sum(weight_gm[!duplicated(match_id)], na.rm = TRUE),
    tot_p_sum = sum(tot_p_adj * weight_gm, na.rm = TRUE),
    recv_sum = sum(recv_pts_adj * weight_gm, na.rm = TRUE),
    disp_sum = sum(disp_pts_adj * weight_gm, na.rm = TRUE),
    spoil_sum = sum(spoil_pts_adj * weight_gm, na.rm = TRUE),
    hitout_sum = sum(hitout_pts_adj * weight_gm, na.rm = TRUE),
    posn = data.table::last(pos)
  ), by = player_id]

  # Second pass: compute derived columns by reference (avoids repeated division)
  result[, `:=`(
    tot_p_g = tot_p_sum / wt_gms,
    recv_g = recv_sum / wt_gms,
    disp_g = disp_sum / wt_gms,
    spoil_g = spoil_sum / wt_gms,
    hitout_g = hitout_sum / wt_gms,
    torp_recv = loading * recv_sum / (wt_gms + prior_games_recv),
    torp_disp = loading * disp_sum / (wt_gms + prior_games_disp),
    torp_spoil = loading * spoil_sum / (wt_gms + prior_games_spoil),
    torp_hitout = loading * hitout_sum / (wt_gms + prior_games_hitout)
  )]

  # Third pass: compute final torp and rounded values
  result[, `:=`(
    torp = round(torp_recv + torp_disp + torp_spoil + torp_hitout, 2),
    torp_recv_adj = round(torp_recv, 2),
    torp_disp_adj = round(torp_disp, 2),
    torp_spoil_adj = round(torp_spoil, 2),
    torp_hitout_adj = round(torp_hitout, 2)
  )]

  # Remove intermediate columns
  result[, c("tot_p_sum", "recv_sum", "disp_sum", "spoil_sum", "hitout_sum") := NULL]

  return(result)
}

#' Prepare final dataframe
#'
#' @param plyr_tm_df Player team database
#' @param player_game_data Player statistics dataframe (aggregated from calculate_player_stats)
#' @param season_val Season value
#' @param round_val Round value
#'
#' @return A final dataframe with player ratings
#'
#' @importFrom dplyr filter left_join ungroup mutate select arrange
#' @importFrom utils data
prepare_final_dataframe <- function(plyr_tm_df = NULL, player_game_data = NULL, season_val, round_val) {
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val)
    # Fall back to previous season if current season data unavailable
    if (nrow(plyr_tm_df) == 0 || !"season" %in% names(plyr_tm_df)) {
      plyr_tm_df <- load_player_details(season_val - 1)
      season_val_details <- season_val - 1
    } else {
      season_val_details <- season_val
    }
  } else {
    if ("season" %in% names(plyr_tm_df) && !season_val %in% plyr_tm_df$season) {
      season_val_details <- max(plyr_tm_df$season)
    } else {
      season_val_details <- season_val
    }
  }

  if (is.null(player_game_data)) {
    player_game_data <- load_player_game_data(season_val)
  }

  # Load fixtures data safely
  fixtures <- NULL
  fixtures <- load_fixtures(TRUE)

  plyr_tm_df |>
    dplyr::filter(.data$season == season_val_details) |>
    dplyr::left_join(player_game_data, by = c("providerId" = "player_id")) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      round = round_val,
      season = season_val
    ) |>
    dplyr::left_join(fixtures |>
      dplyr::group_by(season = .data$compSeason.year, round = .data$round.roundNumber) |>
      dplyr::summarise(ref_date = lubridate::as_date(min(.data$utcStartTime)), .groups = "drop")) |>
    dplyr::mutate(
      age = lubridate::decimal_date(lubridate::as_date(.data$ref_date)) -
        lubridate::decimal_date(lubridate::as_date(.data$dateOfBirth))
    ) |>
    dplyr::select(
      player_id = "providerId", player_name = "player_name.x", age = "age", team = "team",
      torp = "torp", torp_recv = "torp_recv_adj", torp_disp = "torp_disp_adj",
      torp_spoil = "torp_spoil_adj", torp_hitout = "torp_hitout_adj",
      position = "position", season = "season", round = "round", gms = "gms", wt_gms = "wt_gms"
    ) |>
    dplyr::arrange(-.data$torp)
}

#' @rdname calculate_torp_ratings
#' @export
torp_ratings <- calculate_torp_ratings
