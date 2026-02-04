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
#' @param plyr_gm_df Optional pre-loaded player game data. If NULL, will load automatically.
#'
#' @return A data frame containing player TORP ratings.
#' @export
#'
#' @importFrom dplyr filter summarise pull ungroup mutate group_by n_distinct last arrange left_join select
#' @importFrom lubridate as_date decimal_date
#' @importFrom glue glue
#' @importFrom cli cli_abort
#' @importFrom utils data
torp_ratings <- function(season_val = get_afl_season(type = "current"),
                         round_val = get_afl_week(type = "next"),
                         decay = 365,
                         loading = 1.5,
                         prior_games_recv = 4,
                         prior_games_disp = 6,
                         plyr_tm_df = NULL,
                         plyr_gm_df = NULL) {
  # Load player team details if not provided
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val)
  }

  # Load player game statistics if not provided
  if (is.null(plyr_gm_df)) {
    plyr_gm_df <- torp::plyr_gm_df #load_player_stats(season_val)
  }

  gwk <- sprintf("%02d", round_val)
  match_ref <- paste0("CD_M", season_val, "014", gwk)

  date_val <- load_fixtures(TRUE) %>%
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
#' @importFrom data.table as.data.table uniqueN last setDT
calculate_player_stats <- function(plyr_gm_df = NULL, match_ref, date_val, decay, loading, prior_games_recv, prior_games_disp) {
  if (is.null(plyr_gm_df)) {
    plyr_gm_df <- torp::plyr_gm_df
  }

  # Convert to data.table (makes a copy to avoid modifying original)
  dt <- data.table::as.data.table(plyr_gm_df)

  # Filter and add computed columns

  dt <- dt[match_id <= match_ref]
  dt[, weight_gm := exp(as.numeric(-(as.Date(date_val) - as.Date(utc_start_time))) / decay)]
  dt[, plyr_nm := paste(player_given_name, player_surname)]

  # Aggregate by player_id using data.table syntax

  # First pass: compute base aggregations
  result <- dt[, .(
    player_name = max(plyr_nm),
    gms = data.table::uniqueN(match_id),
    wt_gms = sum(unique(weight_gm), na.rm = TRUE),
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
    torp_spoil = loading * 1.2 * spoil_sum / (wt_gms + prior_games_recv),
    torp_hitout = loading * hitout_sum / (wt_gms + prior_games_recv)
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
#' @param plyr_gm_df Player statistics dataframe
#' @param season_val Season value
#' @param round_val Round value
#'
#' @return A final dataframe with player ratings
#'
#' @importFrom dplyr filter left_join ungroup mutate select arrange
#' @importFrom utils data
prepare_final_dataframe <- function(plyr_tm_df = NULL, plyr_gm_df = NULL, season_val, round_val) {
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val)
  }

  if (is.null(plyr_gm_df)) {
    plyr_gm_df <- load_player_stats(season_val)
  }

  # Load fixtures data safely
  fixtures <- NULL
  fixtures <- load_fixtures(TRUE)
  # utils::data("fixtures", package = "torp", envir = environment())

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
    ) %>% # colnames()
    dplyr::select(
      player_id = "providerId", player_name = "player_name.x", age = "age", team = "team",
      torp = "torp", torp_recv = "torp_recv_adj", torp_disp = "torp_disp_adj",
      torp_spoil = "torp_spoil_adj", torp_hitout = "torp_hitout_adj",
      position = "position", season = "season", round = "round", gms = "gms", wt_gms = "wt_gms"
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

  # Input validation
  if (!is.numeric(season_val) && !is.na(season_val)) {
    cli::cli_abort("season_val must be numeric (e.g., 2024)")
  }

  if (!is.numeric(round_num) && !is.na(round_num)) {
    cli::cli_abort("round_num must be numeric (e.g., 1, 2, 3...)")
  }

  if (any(season_val < 1990 | season_val > 2030)) {
    cli::cli_abort("All seasons must be between 1990 and 2030")
  }

  # Validate reasonable round range
  if (is.numeric(round_num) && (any(round_num < 0) || any(round_num > 30))) {
    cli::cli_abort("round_num must be between 0 and 30")
  }

  # Load player game data from package

  plyr_gm_df <- torp::plyr_gm_df

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
      season = "season", round = "round",
      player_name = "plyr_nm", position = "pos", team_id = "team_id", team = "tm", opp = "opp",
      total_points = "total_points", recv_points = "recv_points", disp_points = "disp_points",
      spoil_points = "spoil_points", hitout_points = "hitout_points",
      player_id = "player_id", match_id = "match_id"
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

  # Input validation
  if (!is.numeric(season_val) && !is.na(season_val)) {
    cli::cli_abort("season_val must be numeric (e.g., 2024)")
  }

  if (!is.numeric(round_num) && !all(is.na(round_num))) {
    cli::cli_abort("round_num must be numeric (e.g., 1, 2, 3...)")
  }

  # Validate reasonable season range (handles vectors)
  if (any(season_val < 1990 | season_val > 2030)) {
    cli::cli_abort("All seasons must be between 1990 and 2030")
  }

  # Validate reasonable round range
  if (is.numeric(round_num) && any(round_num < 0 | round_num > 30, na.rm = TRUE)) {
    cli::cli_abort("round_num must be between 0 and 30")
  }

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
#' @param season_val Season value (can be a vector)
#' @param round_num Round number
#'
#' @return Season data frame
get_season_data <- function(season_val, round_num) {
  current_season <- get_afl_season()

  # Determine round range based on whether we're looking at past or current seasons
  if (all(season_val < current_season)) {
    # All historical seasons - use all rounds
    round_range <- if (any(is.na(round_num))) 0:99 else round_num
  } else if (all(season_val == current_season)) {
    # Current season only - limit to current week
    round_range <- if (any(is.na(round_num))) 0:get_afl_week() else round_num
  } else {
    # Mix of seasons - use provided rounds or all available
    round_range <- if (any(is.na(round_num))) 0:99 else round_num
  }

  player_game_ratings(season_val, round_range)
}
