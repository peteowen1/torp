#' Get game ratings
#'
#' Convenience wrapper around [load_player_game_ratings()] with filtering
#' by season, round, match, or team. Returns the same data as the load
#' function — both pull from the same pre-computed release.
#'
#' @param season_val The season to get ratings for. Default is the current season.
#' @param round_num The round number to get ratings for. Default is the current round.
#' @param matchid The match ID to filter by. Default is NULL (no filtering).
#' @param team The team to filter by. Default is NULL (no filtering).
#'
#' @return A data frame containing player game ratings.
#' @export
#'
#' @importFrom dplyr filter arrange
player_game_ratings <- function(season_val = get_afl_season(),
                                round_num = get_afl_week(),
                                matchid = NULL,
                                team = NULL) {

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

  df <- load_player_game_ratings(season_val)
  df <- filter_game_data(df, season_val, round_num, matchid, team)
  df |> dplyr::arrange(-.data$total_p80)
}

#' Compute player game ratings from raw player game data
#'
#' Internal function used by the pipeline to transform raw credits from
#' [create_player_game_data()] into the display-friendly ratings format
#' that gets released to torpdata.
#'
#' @param player_game_data Player game data (output of [create_player_game_data()]).
#' @param season_val Season(s) to compute for.
#' @param round_num Round(s) to compute for.
#'
#' @return A data frame in player game ratings format.
#' @keywords internal
#'
#' @importFrom dplyr arrange mutate select
.compute_player_game_ratings <- function(player_game_data,
                                         season_val,
                                         round_num) {

  df <- filter_game_data(player_game_data, season_val, round_num, matchid = NULL, team = NULL)

  df |>
    dplyr::arrange(-.data$total_credits_adj) |>
    dplyr::mutate(
      tog_frac = pmax(.data$time_on_ground_percentage / 100, 0.1),
      total_points = round(.data$total_credits, 1),
      recv_points = round(.data$recv_credits, 1),
      disp_points = round(.data$disp_credits, 1),
      spoil_points = round(.data$spoil_credits, 1),
      hitout_points = round(.data$hitout_credits, 1),
      total_p80 = round(.data$total_credits_adj, 1),
      recv_p80 = round(.data$recv_credits_adj, 1),
      disp_p80 = round(.data$disp_credits_adj, 1),
      spoil_p80 = round(.data$spoil_credits_adj, 1),
      hitout_p80 = round(.data$hitout_credits_adj, 1)
    ) |>
    dplyr::select(
      season = "season", round = "round",
      player_name = "player_name", position = "listed_position", team_id = "team_id", team = "team", opp = "opponent",
      tog = "tog_frac",
      total_points = "total_points", recv_points = "recv_points", disp_points = "disp_points",
      spoil_points = "spoil_points", hitout_points = "hitout_points",
      total_p80 = "total_p80", recv_p80 = "recv_p80", disp_p80 = "disp_p80",
      spoil_p80 = "spoil_p80", hitout_p80 = "hitout_p80",
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
      (.data$team == .env$team | .data$opp == .env$team)
    )
    if (nrow(df) == 0) {
      cli::cli_abort(paste0(
        "Team not found. Please use one of: ",
        paste(AFL_TEAMS$name, collapse = ", ")
      ))
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
#' Convenience wrapper around [load_player_season_ratings()] with filtering.
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

  load_player_season_ratings(season_val)
}

#' Compute player season ratings from player game ratings
#'
#' Internal function used by the pipeline to aggregate per-game ratings
#' into season totals for release to torpdata.
#'
#' @param player_game_ratings_df Player game ratings data frame
#'   (output of [.compute_player_game_ratings()]).
#'
#' @return A data frame with season-total ratings.
#' @keywords internal
.compute_player_season_ratings <- function(player_game_ratings_df) {
  player_game_ratings_df |>
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
      avg_p80 = mean(.data$total_p80),
      avg_tog = mean(.data$tog),
      .groups = "drop"
    ) |>
    dplyr::arrange(-.data$season_points)
}
