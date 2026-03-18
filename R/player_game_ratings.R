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
  df |> dplyr::arrange(-.data$epv_p80)
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
    dplyr::arrange(-.data$epv_adj) |>
    dplyr::mutate(
      tog_frac = pmax(.data$time_on_ground_percentage / 100, 0.1),
      epv_raw = round(.data$epv, 1),
      recv_epv_raw = round(.data$recv_epv, 1),
      disp_epv_raw = round(.data$disp_epv, 1),
      spoil_epv_raw = round(.data$spoil_epv, 1),
      hitout_epv_raw = round(.data$hitout_epv, 1),
      epv_p80 = round(.data$epv_adj, 1),
      recv_epv_p80 = round(.data$recv_epv_adj, 1),
      disp_epv_p80 = round(.data$disp_epv_adj, 1),
      spoil_epv_p80 = round(.data$spoil_epv_adj, 1),
      hitout_epv_p80 = round(.data$hitout_epv_adj, 1)
    ) |>
    dplyr::select(
      season = "season", round = "round",
      player_name = "player_name", position = "listed_position", team_id = "team_id", team = "team", opp = "opponent",
      tog = "tog_frac",
      epv_raw = "epv_raw", recv_epv_raw = "recv_epv_raw", disp_epv_raw = "disp_epv_raw",
      spoil_epv_raw = "spoil_epv_raw", hitout_epv_raw = "hitout_epv_raw",
      epv_p80 = "epv_p80", recv_epv_p80 = "recv_epv_p80", disp_epv_p80 = "disp_epv_p80",
      spoil_epv_p80 = "spoil_epv_p80", hitout_epv_p80 = "hitout_epv_p80",
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
#' @param round_num Deprecated and ignored. Retained for backwards compatibility.
#'
#' @return A data frame containing player season total ratings.
#' @export
#'
#' @importFrom dplyr group_by summarise arrange n
player_season_ratings <- function(season_val = get_afl_season(), round_num = NULL) {
  # Backwards compat: round_num was never functional but was in the signature

  if (!is.null(round_num)) {
    cli::cli_warn("{.arg round_num} is ignored by {.fn player_season_ratings} and will be removed in a future version.")
  }

  # Input validation
  if (!is.numeric(season_val) && !is.na(season_val)) {
    cli::cli_abort("season_val must be numeric (e.g., 2024)")
  }

  # Validate reasonable season range (handles vectors)
  max_season <- get_afl_season() + 1L
  if (any(season_val < 1990 | season_val > max_season)) {
    cli::cli_abort("All seasons must be between 1990 and {max_season}")
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
      position = get_mode(.data$position),
      games = dplyr::n(),
      season_epv = sum(.data$epv_raw),
      season_recv_epv = sum(.data$recv_epv_raw),
      season_disp_epv = sum(.data$disp_epv_raw),
      season_spoil_epv = sum(.data$spoil_epv_raw),
      season_hitout_epv = sum(.data$hitout_epv_raw),
      epv_pg = .data$season_epv / .data$games,
      avg_p80 = mean(.data$epv_p80),
      avg_tog = mean(.data$tog),
      .groups = "drop"
    ) |>
    dplyr::arrange(-.data$season_epv)
}
