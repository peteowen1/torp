#' Get game ratings
#'
#' This function retrieves game ratings for players based on specified criteria.
#'
#' @param season_val The season to get ratings for. Default is the current season.
#' @param round_num The round number to get ratings for. Default is the current round.
#' @param matchid The match ID to filter by. Default is NULL (no filtering).
#' @param team The team to filter by. Default is NULL (no filtering).
#' @param player_game_data Optional pre-loaded player game data. If NULL, will load automatically.
#'
#' @return A data frame containing player game ratings.
#' @export
#'
#' @importFrom dplyr filter arrange mutate select
player_game_ratings <- function(season_val = get_afl_season(),
                                round_num = get_afl_week(),
                                matchid = NULL,
                                team = NULL,
                                player_game_data = NULL) {

  # Input validation
  if (!is.numeric(season_val) && !is.na(season_val)) {
    cli::cli_abort("season_val must be numeric (e.g., 2024)")
  }

  if (!is.numeric(round_num) && !is.na(round_num)) {
    cli::cli_abort("round_num must be numeric (e.g., 1, 2, 3...)")
  }

  max_season <- get_afl_season() + 1L
  if (any(season_val < 1990 | season_val > max_season)) {
    cli::cli_abort("All seasons must be between 1990 and {max_season}")
  }

  # Validate reasonable round range
  if (is.numeric(round_num) && (any(round_num < 0) || any(round_num > 28))) {
    cli::cli_abort("round_num must be between 0 and 28")
  }

  # Load player game data if not provided
  if (is.null(player_game_data)) {
    player_game_data <- load_player_game_data(TRUE)
  }

  df <- filter_game_data(player_game_data, season_val, round_num, matchid, team)

  df |>
    dplyr::arrange(-.data$tot_p_adj) |>
    dplyr::mutate(
      total_points = round(.data$tot_p_adj, 1),
      recv_points = round(.data$recv_pts_adj, 1),
      disp_points = round(.data$disp_pts_adj, 1),
      spoil_points = round(.data$spoil_pts_adj, 1),
      hitout_points = round(.data$hitout_pts_adj, 1)
    ) |>
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
#' @param matchid Match ID (NULL for no filtering)
#' @param team Team name (NULL for no filtering)
#'
#' @return Filtered data frame
#'
#' @importFrom dplyr filter
#' @importFrom cli cli_abort
filter_game_data <- function(df, season_val, round_num, matchid, team) {
  if (!is.null(matchid)) {
    df <- df |> dplyr::filter(.data$match_id %in% matchid)
    if (nrow(df) == 0) {
      cli::cli_abort("Match ID not found")
    }
  } else if (!is.null(team)) {
    df <- df |> dplyr::filter(
      .data$season %in% season_val,
      .data$round %in% round_num,
      (.data$tm == team | .data$opp == team)
    )
    if (nrow(df) == 0) {
      cli::cli_abort("Team not found. Please use one of: Adelaide Crows, Brisbane Lions, Carlton, Collingwood, Essendon, Fremantle, Geelong Cats, Gold Coast Suns, GWS Giants, Hawthorn, Melbourne, North Melbourne, Port Adelaide, Richmond, St Kilda, Sydney Swans, West Coast Eagles, Western Bulldogs")
    }
  } else {
    df <- df |> dplyr::filter(
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
  max_season <- get_afl_season() + 1L
  if (any(season_val < 1990 | season_val > max_season)) {
    cli::cli_abort("All seasons must be between 1990 and {max_season}")
  }

  # Validate reasonable round range
  if (is.numeric(round_num) && any(round_num < 0 | round_num > 28, na.rm = TRUE)) {
    cli::cli_abort("round_num must be between 0 and 28")
  }

  df <- get_season_data(season_val, round_num)

  df |>
    dplyr::group_by(.data$season, .data$player_name, .data$player_id, .data$team_id) |>
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
    ) |>
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
    round_range <- if (any(is.na(round_num))) 0:28 else round_num
  } else if (all(season_val == current_season)) {
    # Current season only - limit to current week
    round_range <- if (any(is.na(round_num))) 0:get_afl_week() else round_num
  } else {
    # Mix of seasons - use provided rounds or all available
    round_range <- if (any(is.na(round_num))) 0:28 else round_num
  }

  player_game_ratings(season_val, round_range)
}
