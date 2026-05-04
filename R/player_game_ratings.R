#' Get game ratings
#'
#' Convenience wrapper around [load_player_game_ratings()] with filtering
#' by season, round, match, or team. Returns the same data as the load
#' function — both pull from the same pre-computed release.
#'
#' @param season_val The season to get ratings for. Default is the current season.
#' @param round_val The round number to get ratings for. Default is the current round.
#' @param matchid The match ID to filter by. Default is NULL (no filtering).
#' @param team The team to filter by. Default is NULL (no filtering).
#' @param per80 Logical. If `TRUE`, return per-80-minute rates instead of
#'   totals. Default `FALSE` (totals).
#' @param round_num Deprecated. Use \code{round_val} instead.
#'
#' @return A data frame containing player game ratings.
#' @export
#'
#' @importFrom dplyr filter arrange
player_game_ratings <- function(season_val = get_afl_season(),
                                round_val = get_afl_week(),
                                matchid = NULL,
                                team = NULL,
                                per80 = FALSE,
                                round_num = NULL) {

  # Deprecation shim: round_num → round_val
  if (!is.null(round_num)) {
    cli::cli_warn("{.arg round_num} is deprecated in {.fn player_game_ratings}. Use {.arg round_val} instead.")
    round_val <- round_num
  }

  # Input validation
  if (!is.numeric(season_val) && !is.na(season_val)) {
    cli::cli_abort("season_val must be numeric (e.g., 2024)")
  }

  if (isTRUE(round_val)) round_val <- 0:28
  if (!is.numeric(round_val) && !is.na(round_val)) {
    cli::cli_abort("round_val must be numeric (e.g., 1, 2, 3...) or TRUE for all rounds")
  }

  max_season <- get_afl_season() + 1L
  if (any(season_val < 1990 | season_val > max_season)) {
    cli::cli_abort("All seasons must be between 1990 and {max_season}")
  }

  # Validate reasonable round range
  if (is.numeric(round_val) && (any(round_val < 0) || any(round_val > 28))) {
    cli::cli_abort("round_val must be between 0 and 28")
  }

  df <- load_player_game_ratings(season_val)
  if ("recv_epv_raw" %in% names(df)) {
    df <- .center_epv_raw(df)
  }
  df <- filter_game_data(df, season_val, round_val, matchid, team)

  # Compute PSV p80s if missing
  dt <- data.table::as.data.table(df)
  for (col in c("psv", "osv", "dsv")) {
    p80_col <- paste0(col, "_p80")
    if (col %in% names(dt) && !p80_col %in% names(dt)) {
      dt[, (p80_col) := round(get(col) / pmax(tog, 0.01), 1)]
    }
  }

  if (per80) {
    val_cols <- c("epv_p80", "recv_epv_p80", "disp_epv_p80", "spoil_epv_p80", "hitout_epv_p80",
                  "wp_credit_p80", "wp_disp_credit_p80", "wp_recv_credit_p80",
                  "psv_p80", "osv_p80", "dsv_p80", "torp_value_p80")
  } else {
    val_cols <- c("epv", "recv_epv", "disp_epv", "spoil_epv", "hitout_epv",
                  "wp_credit", "wp_disp_credit", "wp_recv_credit",
                  "psv", "osv", "dsv", "torp_value")
  }

  col_order <- c(
    "season", "round", "player_name", "position_group", "team", "opp", "tog",
    val_cols,
    "player_id", "team_id", "match_id"
  )

  result <- dt[, intersect(col_order, names(dt)), with = FALSE]
  sort_col <- if (per80) "torp_value_p80" else "torp_value"
  if (!sort_col %in% names(result)) sort_col <- if (per80) "epv_p80" else "epv"
  if (sort_col %in% names(result)) data.table::setorderv(result, sort_col, order = -1L)
  result
}

#' Compute player game ratings from raw player game data
#'
#' Internal function used by the pipeline to transform raw credits from
#' [create_player_game_data()] into the display-friendly ratings format
#' that gets released to torpdata.
#'
#' @param player_game_data Player game data (output of [create_player_game_data()]).
#' @param season_val Season(s) to compute for.
#' @param round_val Round(s) to compute for.
#'
#' @return A data frame in player game ratings format.
#' @keywords internal
#'
#' @importFrom dplyr arrange mutate select
.compute_player_game_ratings <- function(player_game_data,
                                         season_val,
                                         round_val) {

  df <- filter_game_data(player_game_data, season_val, round_val, matchid = NULL, team = NULL)

  if (!"lineup_position" %in% names(df)) {
    cli::cli_abort("Column {.val lineup_position} required for position centering but not found in player_game_data.")
  }

  has_wpa <- "wp_credit" %in% names(df)

  # Use _oadj (opponent-adjusted) columns when available, fall back to raw
  has_oadj <- all(c("recv_epv_oadj", "disp_epv_oadj",
                     "spoil_epv_oadj", "hitout_epv_oadj") %in% names(df))
  recv_col <- if (has_oadj) "recv_epv_oadj" else "recv_epv"
  disp_col <- if (has_oadj) "disp_epv_oadj" else "disp_epv"
  spoil_col <- if (has_oadj) "spoil_epv_oadj" else "spoil_epv"
  hitout_col <- if (has_oadj) "hitout_epv_oadj" else "hitout_epv"

  df <- df |>
    dplyr::arrange(-.data$epv_adj) |>
    dplyr::mutate(
      position_group = dplyr::if_else(.data$position_group == "MIDFIELDER_FORWARD",
                                      "MEDIUM_FORWARD", .data$position_group),
      tog_frac = pmax(dplyr::coalesce(.data$time_on_ground_percentage / 100, 0.1), 0.1),
      recv_epv_c = round(.data[[recv_col]] -
        sum(.data[[recv_col]]) / sum(.data$tog_frac) * .data$tog_frac, 1),
      disp_epv_c = round(.data[[disp_col]] -
        sum(.data[[disp_col]]) / sum(.data$tog_frac) * .data$tog_frac, 1),
      spoil_epv_c = round(.data[[spoil_col]] -
        sum(.data[[spoil_col]]) / sum(.data$tog_frac) * .data$tog_frac, 1),
      hitout_epv_c = round(.data[[hitout_col]] -
        sum(.data[[hitout_col]]) / sum(.data$tog_frac) * .data$tog_frac, 1),
      epv_c = round(.data$recv_epv_c + .data$disp_epv_c +
        .data$spoil_epv_c + .data$hitout_epv_c, 1),
      .by = c("season", "lineup_position")
    ) |>
    dplyr::mutate(
      epv_p80 = round(.data$epv_c / .data$tog_frac, 1),
      recv_epv_p80 = round(.data$recv_epv_c / .data$tog_frac, 1),
      disp_epv_p80 = round(.data$disp_epv_c / .data$tog_frac, 1),
      spoil_epv_p80 = round(.data$spoil_epv_c / .data$tog_frac, 1),
      hitout_epv_p80 = round(.data$hitout_epv_c / .data$tog_frac, 1)
    )

  # WPA centering (mirrors EPV pattern, guarded for old data)
  if (has_wpa) {
    df <- df |>
      dplyr::mutate(
        wp_credit_c = round(.data$wp_credit -
          sum(.data$wp_credit) / sum(.data$tog_frac) * .data$tog_frac, 3),
        wp_disp_credit_c = round(.data$wp_disp_credit -
          sum(.data$wp_disp_credit) / sum(.data$tog_frac) * .data$tog_frac, 3),
        wp_recv_credit_c = round(.data$wp_recv_credit -
          sum(.data$wp_recv_credit) / sum(.data$tog_frac) * .data$tog_frac, 3),
        .by = c("season", "lineup_position")
      ) |>
      dplyr::mutate(
        wp_credit_p80 = round(.data$wp_credit_c / .data$tog_frac, 3),
        wp_disp_credit_p80 = round(.data$wp_disp_credit_c / .data$tog_frac, 3),
        wp_recv_credit_p80 = round(.data$wp_recv_credit_c / .data$tog_frac, 3)
      )
  }

  # Final column select
  df |>
    dplyr::select(
      season = "season", round = "round",
      player_name = "player_name", position_group = "position_group",
      lineup_position = "lineup_position",
      team = "team", opp = "opponent",
      tog = "tog_frac",
      epv = "epv_c", recv_epv = "recv_epv_c", disp_epv = "disp_epv_c",
      spoil_epv = "spoil_epv_c", hitout_epv = "hitout_epv_c",
      epv_p80 = "epv_p80", recv_epv_p80 = "recv_epv_p80", disp_epv_p80 = "disp_epv_p80",
      spoil_epv_p80 = "spoil_epv_p80", hitout_epv_p80 = "hitout_epv_p80",
      dplyr::any_of(c(
        wp_credit = "wp_credit_c", wp_disp_credit = "wp_disp_credit_c",
        wp_recv_credit = "wp_recv_credit_c",
        "wp_credit_p80", "wp_disp_credit_p80", "wp_recv_credit_p80"
      )),
      player_id = "player_id", team_id = "team_id", match_id = "match_id"
    )
}

#' Center EPV raw columns to per-position-season mean of zero
#'
#' @param df Player game ratings data frame.
#' @return Data frame with centered EPV and per-80 columns.
#' @keywords internal
.center_epv_raw <- function(df) {
  if (!"lineup_position" %in% names(df)) {
    cli::cli_abort("Column {.val lineup_position} required for EPV centering but not found. Ensure teams data is joined upstream.")
  }
  df |>
    dplyr::mutate(
      .total_tog = sum(.data$tog),
      recv_epv = round(.data$recv_epv_raw -
        dplyr::if_else(.data$.total_tog > 0, sum(.data$recv_epv_raw) / .data$.total_tog * .data$tog, 0), 1),
      disp_epv = round(.data$disp_epv_raw -
        dplyr::if_else(.data$.total_tog > 0, sum(.data$disp_epv_raw) / .data$.total_tog * .data$tog, 0), 1),
      spoil_epv = round(.data$spoil_epv_raw -
        dplyr::if_else(.data$.total_tog > 0, sum(.data$spoil_epv_raw) / .data$.total_tog * .data$tog, 0), 1),
      hitout_epv = round(.data$hitout_epv_raw -
        dplyr::if_else(.data$.total_tog > 0, sum(.data$hitout_epv_raw) / .data$.total_tog * .data$tog, 0), 1),
      epv = round(.data$recv_epv + .data$disp_epv +
        .data$spoil_epv + .data$hitout_epv, 1),
      .by = c("season", "lineup_position")
    ) |>
    dplyr::select(-".total_tog") |>
    dplyr::mutate(
      .safe_tog = pmax(.data$tog, 0.1),
      epv_p80 = round(.data$epv / .data$.safe_tog, 1),
      recv_epv_p80 = round(.data$recv_epv / .data$.safe_tog, 1),
      disp_epv_p80 = round(.data$disp_epv / .data$.safe_tog, 1),
      spoil_epv_p80 = round(.data$spoil_epv / .data$.safe_tog, 1),
      hitout_epv_p80 = round(.data$hitout_epv / .data$.safe_tog, 1)
    ) |>
    dplyr::select(-".safe_tog") |>
    dplyr::select(-"epv_raw", -"recv_epv_raw", -"disp_epv_raw",
                   -"spoil_epv_raw", -"hitout_epv_raw")
}

#' Filter game data
#'
#' @param df Input data frame
#' @param season_val Season value
#' @param round_val Round number
#' @param matchid Match ID (NULL for no filtering)
#' @param team Team name (NULL for no filtering)
#'
#' @return Filtered data frame
#' @keywords internal
#'
#' @importFrom dplyr filter
#' @importFrom cli cli_abort
filter_game_data <- function(df, season_val, round_val, matchid, team) {
  if (!is.null(matchid)) {
    df <- df |> dplyr::filter(.data$match_id %in% matchid)
    if (nrow(df) == 0) {
      cli::cli_abort("Match ID not found")
    }
  } else if (!is.null(team)) {
    df <- df |> dplyr::filter(
      .data$season %in% season_val,
      .data$round %in% round_val,
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
      .data$round %in% round_val
    )
  }
  return(df)
}

#' Get season total ratings
#'
#' Convenience wrapper around [load_player_season_ratings()] with filtering.
#'
#' @param season_val The season to calculate ratings for. Default is the current season.
#' @param per80 Logical. If `TRUE`, return per-80-minute averages instead of
#'   season totals. Default `FALSE` (totals).
#' @param round_num Deprecated and ignored. Retained for backwards compatibility.
#'
#' @return A data frame containing player season ratings.
#' @export
#'
#' @importFrom dplyr group_by summarise arrange n
player_season_ratings <- function(season_val = get_afl_season(),
                                  per80 = FALSE,
                                  round_num = NULL) {
  if (!is.null(round_num)) {
    cli::cli_warn("{.arg round_num} is ignored by {.fn player_season_ratings} and will be removed in a future version.")
  }

  if (!is.numeric(season_val) && !is.na(season_val)) {
    cli::cli_abort("season_val must be numeric (e.g., 2024)")
  }

  max_season <- get_afl_season() + 1L
  if (any(season_val < 1990 | season_val > max_season)) {
    cli::cli_abort("All seasons must be between 1990 and {max_season}")
  }

  pgr <- load_player_game_ratings(season_val)
  if ("recv_epv_raw" %in% names(pgr)) {
    pgr <- .center_epv_raw(pgr)
  }
  .compute_player_season_ratings(pgr, per80 = per80)
}

#' Compute player season ratings from player game ratings
#'
#' Internal function used by the pipeline to aggregate per-game ratings
#' into season totals for release to torpdata.
#'
#' @param player_game_ratings_df Player game ratings data frame
#'   (output of [.compute_player_game_ratings()]).
#' @param per80 If TRUE, return per-80-minute averages instead of totals.
#'
#' @return A data frame with season ratings.
#' @keywords internal
.compute_player_season_ratings <- function(player_game_ratings_df, per80 = FALSE) {
  df <- data.table::as.data.table(player_game_ratings_df)

  # Backward compat: older player_game_ratings parquets on torpdata have a
  # `position` column (the 6-way class); it was renamed to `position_group`
  # on the producer side. Remap on load so old releases still work.
  if (!"position_group" %in% names(df) && "position" %in% names(df)) {
    data.table::setnames(df, "position", "position_group")
  }

  # Base aggregation
  result <- df[, {
    out <- list(
      team = get_mode(team),
      position_group = get_mode(position_group),
      games = .N,
      avg_tog = round(mean(tog, na.rm = TRUE), 2)
    )
    # EPV columns (always present)
    for (col in c("epv", "recv_epv", "disp_epv", "spoil_epv", "hitout_epv")) {
      if (col %in% names(.SD)) out[[col]] <- round(sum(get(col), na.rm = TRUE), 1)
    }
    # WPA columns (optional)
    for (col in c("wp_credit", "wp_disp_credit", "wp_recv_credit")) {
      if (col %in% names(.SD)) out[[col]] <- round(sum(get(col), na.rm = TRUE), 3)
    }
    # PSV columns (optional)
    for (col in c("psv", "osv", "dsv", "torp_value")) {
      if (col %in% names(.SD)) out[[col]] <- round(sum(get(col), na.rm = TRUE), 1)
    }
    out
  }, by = .(season, player_name, player_id, team_id)]

  # Compute p80 variants
  epv_cols <- intersect(c("epv", "recv_epv", "disp_epv", "spoil_epv", "hitout_epv"), names(result))
  wp_cols <- intersect(c("wp_credit", "wp_disp_credit", "wp_recv_credit"), names(result))
  psv_cols <- intersect(c("psv", "osv", "dsv", "torp_value"), names(result))
  all_val_cols <- c(epv_cols, wp_cols, psv_cols)

  for (col in all_val_cols) {
    p80_col <- paste0(col, "_p80")
    result[, (p80_col) := round(get(col) / (games * avg_tog), 1)]
  }

  # Select and order columns
  if (per80) {
    val_select <- paste0(all_val_cols, "_p80")
  } else {
    val_select <- all_val_cols
  }

  col_order <- c(
    "season", "player_name", "position_group", "team", "games", "avg_tog",
    val_select,
    "player_id", "team_id"
  )
  col_order <- intersect(col_order, names(result))

  sort_col <- if (per80) {
    if ("torp_value_p80" %in% names(result)) "torp_value_p80" else "epv_p80"
  } else {
    if ("torp_value" %in% names(result)) "torp_value" else "epv"
  }

  result <- result[, col_order, with = FALSE]
  data.table::setorderv(result, sort_col, order = -1L)
  result
}


#' Get player season stats
#'
#' Aggregates per-game box-score stats from [load_player_stats()] into
#' season totals or per-80-minute averages. Useful for seeing stat leaders.
#'
#' @param season_val Season year(s). Default is current season.
#' @param per80 Logical. If `TRUE`, return per-80-minute averages.
#'   Default `FALSE` (season totals).
#' @param sort_by Column name to sort by (descending). Default `"disposals"`.
#'
#' @return A data.table with season-aggregated stats.
#' @export
player_season_stats <- function(season_val = get_afl_season(),
                                per80 = FALSE,
                                sort_by = "disposals") {
  ps <- data.table::as.data.table(load_player_stats(season_val, use_disk_cache = TRUE))

  # Derive team from home/away + team_status
  if (!"team" %in% names(ps) && all(c("home_team_name", "away_team_name", "team_status") %in% names(ps))) {
    ps[, team := data.table::fifelse(team_status == "home", home_team_name, away_team_name)]
  }

  # TOG fraction
  if (!"tog" %in% names(ps) && "time_on_ground_percentage" %in% names(ps)) {
    ps[, tog := pmax(as.numeric(time_on_ground_percentage) / 100, 0.1)]
  }

  # Round number
  if (!"round" %in% names(ps) && "round_number" %in% names(ps)) {
    ps[, round := as.integer(round_number)]
  }

  # Stat columns to aggregate (count stats that make sense to sum)
  count_stats <- c(
    "goals", "behinds", "kicks", "handballs", "disposals",
    "marks", "tackles", "hitouts", "contested_possessions",
    "uncontested_possessions", "inside50s", "marks_inside50",
    "contested_marks", "clearances", "centre_clearances",
    "stoppage_clearances", "one_percenters", "clangers",
    "frees_for", "frees_against", "rebound50s", "turnovers",
    "intercepts", "spoils", "ground_ball_gets", "bounces",
    "tackles_inside50", "shots_at_goal", "score_involvements",
    "metres_gained", "goal_assists", "pressure_acts",
    "def_half_pressure_acts", "effective_kicks", "effective_disposals",
    "intercept_marks", "f50_ground_ball_gets", "score_launches",
    "marks_on_lead", "hitouts_to_advantage", "ruck_contests",
    "centre_bounce_attendances", "contest_def_one_on_ones",
    "contest_off_one_on_ones", "contest_off_wins", "contest_def_losses",
    "kickins", "dream_team_points", "rating_points"
  )
  stat_cols <- intersect(count_stats, names(ps))

  # Ensure numeric
  for (col in stat_cols) {
    if (!is.numeric(ps[[col]])) ps[, (col) := as.numeric(get(col))]
  }

  # Aggregate
  result <- ps[, {
    out <- list(
      team = get_mode(team),
      position = get_mode(position),
      games = .N,
      avg_tog = round(mean(tog, na.rm = TRUE), 2)
    )
    for (col in stat_cols) out[[col]] <- sum(get(col), na.rm = TRUE)
    out
  }, by = .(season, player_name, player_id)]

  # Per-80 mode: divide by (games * avg_tog) to get per-full-game rates
  if (per80) {
    for (col in stat_cols) {
      result[, (col) := round(get(col) / (games * avg_tog), 1)]
    }
  }

  # Sort
  if (!sort_by %in% names(result)) {
    cli::cli_warn("Column {.val {sort_by}} not found, sorting by disposals")
    sort_by <- "disposals"
  }
  data.table::setorderv(result, sort_by, order = -1L)

  # Column order: info first, then stats, then ID
  col_order <- c("season", "player_name", "position", "team", "games", "avg_tog",
                 stat_cols, "player_id")
  result[, intersect(col_order, names(result)), with = FALSE]
}
