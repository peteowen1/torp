#' Summarise shot-level PBP into match-level xG stats
#' @param pbp Play-by-play data with xscore and shot_row columns
#' @param quarter Numeric vector of quarters to include
#' @return Tibble with one row per match
#' @keywords internal
.summarise_match_xg <- function(pbp, quarter = 1:4) {
  shots_df <- pbp |>
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

  zero_xg <- shots_df$match_id[shots_df$total_xpoints == 0]
  if (length(zero_xg) > 0) {
    cli::cli_warn(c(
      "{length(zero_xg)} match{?es} ha{?s/ve} zero total xscore -- likely a team name mismatch.",
      "i" = "Check that PBP 'team' col matches 'home_team_name'/'away_team_name'.",
      "i" = "Affected: {paste(utils::head(zero_xg, 5), collapse = ', ')}"
    ))
  }

  shots_df
}


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

  shots_df <- .summarise_match_xg(df, quarter)

  return(shots_df)
}


#' Get Live xG for a Match or Round
#'
#' Scrapes chain data from the AFL API, runs the shot model pipeline, and
#' computes match-level xG summaries. Useful for matches that just finished
#' before the daily torpdata pipeline has run.
#'
#' @param match Input: either a match ID string (e.g. `"CD_M20260140201"`)
#'   or a pre-scraped chains data.frame from [get_match_chains()].
#'   If `NULL` (default), uses `season` and `round` to fetch chains.
#' @param season Numeric season year (default: current season via
#'   [get_afl_season()]). Only used when `match` is `NULL`.
#' @param round Numeric round number. Only used when `match` is `NULL`.
#' @param quarter Numeric vector of quarters to include (default: `1:4`).
#' @param detail Logical. If `TRUE`, returns the full play-by-play data
#'   (filtered to shots) instead of match-level summaries. Useful for
#'   debugging xG values. Default is `FALSE`.
#'
#' @return When `detail = FALSE` (default), a tibble with one row per match.
#'   When `detail = TRUE`, the full shot-level PBP with columns like
#'   `xscore`, `goal_prob`, `behind_prob`, `points_shot`, `shot_row`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # From match ID
#' get_xg("CD_M20260140201")
#'
#' # From season and round (all matches in the round)
#' get_xg(round = 2)
#' get_xg(season = 2025, round = 14)
#'
#' # Debug: return shot-level data
#' get_xg(round = 2, detail = TRUE)
#'
#' # From pre-scraped chains
#' chains <- get_match_chains(2026, 2)
#' get_xg(chains)
#' }
get_xg <- function(match = NULL,
                   season = get_afl_season(),
                   round = NULL,
                   quarter = 1:4,
                   detail = FALSE) {
  # --- Resolve input: match ID, season+round, or pre-scraped chains ---
  if (is.null(match)) {
    if (is.null(round)) {
      cli::cli_abort("Must provide either {.arg match} or {.arg round}.")
    }
    cli::cli_inform("Fetching chains for {.val {season}} round {.val {round}}...")
    chains <- get_match_chains(season, round)
  } else if (is.character(match) && length(match) == 1 && grepl("^CD_M", match)) {
    chains <- get_match_chains(match)
  } else if (is.data.frame(match) && nrow(match) > 0) {
    chains <- match
  } else {
    cli::cli_abort("{.arg match} must be a match ID string, a chains data.frame, or {.code NULL} (with {.arg round} specified).")
  }

  # --- Run shot model pipeline ---
  cli::cli_inform("Running shot model pipeline...")
  pbp <- chains |>
    clean_pbp() |>
    clean_model_data_epv() |>
    clean_shots_data() |>
    add_shot_vars()

  # --- Detail mode: return shot-level rows for debugging ---
  if (detail) {
    cli::cli_inform("Done! Returning shot-level detail.")
    return(pbp |> dplyr::filter(.data$shot_row == 1, .data$period %in% quarter))
  }

  # --- Summarise to match-level xG ---
  shots_df <- .summarise_match_xg(pbp, quarter)

  cli::cli_inform("Done!")
  shots_df
}


#' @rdname calculate_match_xgs
#' @description `match_xgs()` is deprecated; use `calculate_match_xgs()` instead.
#' @export
match_xgs <- function(season = get_afl_season(), round = get_afl_week(), quarter = 1:4) {
  .Deprecated("calculate_match_xgs", package = "torp", old = "match_xgs")
  calculate_match_xgs(season = season, round = round, quarter = quarter)
}


#' Extract player shooting skill from the shot model
#'
#' Extracts per-player random effects from the shot GAM model, representing
#' each player's shooting ability relative to average (controlling for shot
#' location, play type, and position). Positive values = better than average
#' conversion.
#'
#' @param shot_model Optional fitted shot GAM model. If NULL, loads from
#'   torpmodels via [load_model_with_fallback()].
#' @return A data.table with columns `player_id`, `player_name`, `xg_skill`
#'   (random effect coefficient), `xg_skill_se` (standard error), and
#'   `n_shots` (number of shots in training data, if available). Sorted by
#'   `xg_skill` descending. Returns NULL if extraction fails.
#' @export
extract_player_xg_skill <- function(shot_model = NULL) {
  if (is.null(shot_model)) {
    shot_model <- tryCatch(
      load_model_with_fallback("shot"),
      error = function(e) {
        cli::cli_warn("Could not load shot model: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(shot_model)) return(NULL)
  }

  re <- tryCatch(
    extract_gam_random_effects(shot_model, "player_id_shot"),
    error = function(e) {
      cli::cli_warn("Could not extract player random effects: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(re)) return(NULL)

  result <- re[, .(player_id = level, xg_skill = coefficient, xg_skill_se = se)]

  # Count shots per player from training data if available
  if (!is.null(shot_model$model) && "player_id_shot" %in% names(shot_model$model)) {
    shot_counts <- as.data.table(shot_model$model)[, .(n_shots = .N), by = .(player_id = player_id_shot)]
    shot_counts[, player_id := as.character(player_id)]
    result <- merge(result, shot_counts, by = "player_id", all.x = TRUE)
    result[is.na(n_shots), n_shots := 0L]
  }

  # Join player names from player_details
  pd <- tryCatch(load_player_details(get_afl_season()), error = function(e) NULL)
  if (!is.null(pd)) {
    pd_dt <- data.table::as.data.table(pd)
    pd_dt <- unique(pd_dt[, .(player_id, player_name)])
    result <- merge(result, pd_dt, by = "player_id", all.x = TRUE)
  } else {
    result[, player_name := NA_character_]
  }

  data.table::setorder(result, -xg_skill)
  result[, .(player_id, player_name, xg_skill, xg_skill_se, n_shots)]
}

