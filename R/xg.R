#' Calculate xGs for AFL Matches
#'
#' @param season AFL season
#' @param round AFL round
#' @param quarter AFL match quarter
#'
#' @return A data frame with xG statistics for the specified matches
#' @export
#'
#' @importFrom dplyr group_by filter summarise if_else
calculate_match_xgs <- function(season = get_afl_season(), round = get_afl_week(), quarter = 1:4) {
  df <- tryCatch(
    load_pbp(seasons = season, rounds = round),
    error = function(e) {
      cli::cli_abort("Could not load play-by-play data for season {season}, round {round}: {e$message}")
    }
  )

  if (nrow(df) == 0) {
    cli::cli_abort("No play-by-play data available for season {season}, round {round}.")
  }

  if (!"xscore" %in% names(df)) {
    cli::cli_abort(c(
      "PBP data missing {.val xscore} column.",
      "i" = "This column is created by {.fn add_shot_vars}. Ensure your PBP data has been processed through the shot model pipeline."
    ))
  }

  # Normalise team names so PBP 'team' col (short names) matches home/away names
  df <- df |>
    dplyr::mutate(
      home_team_name = torp_replace_teams(.data$home_team_name),
      away_team_name = torp_replace_teams(.data$away_team_name)
    )

  shots_df <- df |>
    dplyr::group_by(.data$match_id) |>
    dplyr::filter(.data$period %in% quarter) |>
    dplyr::summarise(
      home_team = max(.data$home_team_name),
      home_shots_score = sum(dplyr::if_else(.data$team == .data$home_team_name, .data$points_shot, 0), na.rm = TRUE),
      home_xscore = sum(dplyr::if_else(.data$team == .data$home_team_name, .data$xscore * .data$shot_row, 0), na.rm = TRUE),
      home_scored_goals = sum(dplyr::if_else(.data$team == .data$home_team_name, dplyr::if_else(.data$points_shot == 6, 1, 0), 0), na.rm = TRUE),
      home_scored_behinds = sum(dplyr::if_else(.data$team == .data$home_team_name, dplyr::if_else(.data$points_shot == 1, 1, 0), 0), na.rm = TRUE),
      away_team = max(.data$away_team_name),
      away_shots_score = sum(dplyr::if_else(.data$team == .data$away_team_name, .data$points_shot, 0), na.rm = TRUE),
      away_xscore = sum(dplyr::if_else(.data$team == .data$away_team_name, .data$xscore * .data$shot_row, 0), na.rm = TRUE),
      away_scored_goals = sum(dplyr::if_else(.data$team == .data$away_team_name, dplyr::if_else(.data$points_shot == 6, 1, 0), 0), na.rm = TRUE),
      away_scored_behinds = sum(dplyr::if_else(.data$team == .data$away_team_name, dplyr::if_else(.data$points_shot == 1, 1, 0), 0), na.rm = TRUE),
      score_diff = .data$home_shots_score - .data$away_shots_score,
      xscore_diff = .data$home_xscore - .data$away_xscore,
      total_points = .data$home_shots_score + .data$away_shots_score,
      total_xpoints = .data$home_xscore + .data$away_xscore,
      .groups = "drop"
    )
  # Guard: matches with zero xscores indicate a team name mismatch between

  # the PBP 'team' column and 'home_team_name'/'away_team_name'
  zero_xg <- shots_df$match_id[shots_df$total_xpoints == 0]
  if (length(zero_xg) > 0) {
    cli::cli_warn(c(
      "{length(zero_xg)} match{?es} ha{?s/ve} zero total xscore — likely a team name mismatch.",
      "i" = "Check that PBP 'team' col matches 'home_team_name'/'away_team_name'.",
      "i" = "Affected: {paste(utils::head(zero_xg, 5), collapse = ', ')}"
    ))
  }

  return(shots_df)
}

#' @rdname calculate_match_xgs
#' @description `match_xgs()` is deprecated; use `calculate_match_xgs()` instead.
#' @export
match_xgs <- function(season = get_afl_season(), round = get_afl_week(), quarter = 1:4) {
  .Deprecated("calculate_match_xgs", package = "torp", old = "match_xgs")
  calculate_match_xgs(season = season, round = round, quarter = quarter)
}
