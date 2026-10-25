#' Get game ratings
#'
#' Convenience wrapper around [load_player_game_ratings()] with filtering
#' by season, round, match, or team. Both pull from the same pre-computed
#' release, but this wrapper returns a fixed display subset of its columns:
#' season, round, player, position group, team, opponent, time on ground, the
#' EPV / WPA-credit / PSV / TORP value columns (totals, or per-80 when
#' `per80 = TRUE`) and the player, team and match ids. The released columns
#' `lineup_position`, `net_points`, the `wpa_*` ledger and the `np_*` parts are
#' NOT returned here; use [load_player_game_ratings()] for those.
#'
#' @param season_val The season to get ratings for. Default is the current season.
#' @param round_val The round number to get ratings for. Default is the current round.
#' @param matchid The match ID to filter by. Default is NULL (no filtering).
#' @param team The team to filter by. Default is NULL (no filtering).
#' @param per80 Logical. If `TRUE`, return per-80-minute rates instead of
#'   totals. Default `FALSE` (totals).
#' @param round_num Deprecated. Use \code{round_val} instead.
#'
#' @return A data frame containing the display subset of player game ratings
#'   described above, sorted by TORP value (or EPV when PSV is absent).
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

  if (isTRUE(round_val)) round_val <- AFL_ALL_ROUNDS
  if (!is.numeric(round_val) && !is.na(round_val)) {
    cli::cli_abort("round_val must be numeric (e.g., 1, 2, 3...) or TRUE for all rounds")
  }

  max_season <- get_afl_season() + 1L
  if (any(season_val < 1990 | season_val > max_season)) {
    cli::cli_abort("All seasons must be between 1990 and {max_season}")
  }

  # Validate reasonable round range
  if (is.numeric(round_val) && (any(round_val < 0) || any(round_val > max(AFL_ALL_ROUNDS)))) {
    cli::cli_abort("round_val must be between 0 and {max(AFL_ALL_ROUNDS)}")
  }

  df <- load_player_game_ratings(season_val)
  if ("epv_recv_raw" %in% names(df)) {
    df <- .center_epv_raw(df)
  }
  df <- filter_game_data(df, season_val, round_val, matchid, team)

  # Compute PSV p80s if missing
  dt <- data.table::as.data.table(df)
  for (col in c("psv", "osv", "dsv")) {
    p80_col <- paste0(col, "_p80")
    if (col %in% names(dt) && !p80_col %in% names(dt)) {
      dt[, (p80_col) := round(get(col) / .tog_floor(tog), 1)]
    }
  }

  if (per80) {
    val_cols <- c("epv_p80", "epv_recv_p80", "epv_disp_p80", "epv_spoil_p80", "epv_hitout_p80",
                  "wp_credit_p80", "wp_disp_credit_p80", "wp_recv_credit_p80",
                  "psv_p80", "osv_p80", "dsv_p80", "torp_value_p80")
  } else {
    val_cols <- c("epv", "epv_recv", "epv_disp", "epv_spoil", "epv_hitout",
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

# One floor for every per-80 division. A player on for 0% of a game (or a stray
# 0 in an old release) would otherwise give Inf/NaN, and the three places that
# floored had used 0.01, 0.1 and nothing. 0.1 matches the credit pipeline
# (R/player_credit.R) and the tog_frac in .compute_player_game_ratings().
.PGR_TOG_FLOOR <- 0.1

#' Floor a time-on-ground fraction before dividing by it
#'
#' NA stays NA (an unknown time on ground is not the same as a short one).
#' @param tog Time on ground as a fraction (0-1).
#' @return `tog` with values below 0.1 raised to 0.1.
#' @keywords internal
.tog_floor <- function(tog) pmax(tog, .PGR_TOG_FLOOR)

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

  # Prefer the REAL ledger net_points when the frame carries one -- v4's
  # create_player_game_data() already attaches it and asserts epv == net_points
  # to 1e-8 (R/player_credit.R). Re-deriving from epv instead would have worked
  # only by leaning on that invariant holding from 80 lines away in a different
  # file; passing the real column through means a future engine that loosens
  # the invariant, or a v2/v3 frame that never had it, cannot silently mislabel
  # something as net_points that is not actually the ledger's own value.
  #
  # It is optional only so a partial frame (a test fixture, a caller
  # assembling channels by hand, a v2/v3 frame) still works -- but its absence
  # is said out loud and the column is NA, never filled from `epv`: a raw epv
  # is not the ledger value, and labelling it net_points is exactly the
  # mislabel the paragraph above rules out.
  has_np <- "net_points" %in% names(df)
  if (has_np && "epv" %in% names(df)) {
    np_gap <- max(abs(df$epv - df$net_points), na.rm = TRUE)
    if (is.finite(np_gap) && np_gap > 1e-6) {
      cli::cli_abort(c(
        "{.field epv} and {.field net_points} disagree by up to {signif(np_gap, 3)}.",
        "x" = "They are supposed to be the same value (v4's own 1e-8 assertion); publishing net_points from a source that disagrees with epv would be worse than not publishing it."
      ))
    }
  }
  if (!has_np) {
    cli::cli_alert_warning(
      "No {.field net_points} column on the input: {.field net_points} (the value that sums to a team's own margin) will be NA in the result. {.field epv} is not substituted for it.")
  }

  # After the epv/net_points gate above (its message is the more useful one when
  # they disagree), before anything below centres the channels.
  df <- .np_raw_parts(df)
  if (!has_np) df$net_points <- NA_real_

  # Use _oadj (opponent-adjusted) columns when available, fall back to raw
  has_oadj <- all(c("epv_recv_oadj", "epv_disp_oadj",
                     "epv_spoil_oadj", "epv_hitout_oadj") %in% names(df))
  recv_col <- if (has_oadj) "epv_recv_oadj" else "epv_recv"
  disp_col <- if (has_oadj) "epv_disp_oadj" else "epv_disp"
  spoil_col <- if (has_oadj) "epv_spoil_oadj" else "epv_spoil"
  hitout_col <- if (has_oadj) "epv_hitout_oadj" else "epv_hitout"

  df <- df |>
    dplyr::arrange(-.data$epv_adj) |>
    dplyr::mutate(
      position_group = dplyr::if_else(.data$position_group == "MIDFIELDER_FORWARD",
                                      "MEDIUM_FORWARD", .data$position_group),
      tog_frac = .tog_floor(dplyr::coalesce(.data$time_on_ground_percentage / 100, .PGR_TOG_FLOOR)),
      epv_recv_c = round(.data[[recv_col]] -
        sum(.data[[recv_col]]) / sum(.data$tog_frac) * .data$tog_frac, 1),
      epv_disp_c = round(.data[[disp_col]] -
        sum(.data[[disp_col]]) / sum(.data$tog_frac) * .data$tog_frac, 1),
      epv_spoil_c = round(.data[[spoil_col]] -
        sum(.data[[spoil_col]]) / sum(.data$tog_frac) * .data$tog_frac, 1),
      epv_hitout_c = round(.data[[hitout_col]] -
        sum(.data[[hitout_col]]) / sum(.data$tog_frac) * .data$tog_frac, 1),
      epv_c = round(.data$epv_recv_c + .data$epv_disp_c +
        .data$epv_spoil_c + .data$epv_hitout_c, 1),
      .by = c("season", "lineup_position")
    ) |>
    dplyr::mutate(
      epv_p80 = round(.data$epv_c / .data$tog_frac, 1),
      epv_recv_p80 = round(.data$epv_recv_c / .data$tog_frac, 1),
      epv_disp_p80 = round(.data$epv_disp_c / .data$tog_frac, 1),
      epv_spoil_p80 = round(.data$epv_spoil_c / .data$tog_frac, 1),
      epv_hitout_p80 = round(.data$epv_hitout_c / .data$tog_frac, 1)
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
      # The ledger value, carried through uncentred and unadjusted. `epv` below
      # is centred within (season, lineup_position) and, where the columns
      # exist, opponent-adjusted -- both right for a RATING and both fatal to a
      # number displayed beside Score, because they stop a team's players
      # summing to that team's margin. Measured 2026-09-09: this column sums to
      # the own margin exactly (mean gap 0.00), `epv` misses by 9.56 on average.
      # See docs/plans/EPV-DISPLAY-COLUMN.md. Additive on purpose: nothing that
      # already reads `epv` changes. Sourced from the real net_points column
      # when the frame has one, NA when it does not -- never re-derived from epv.
      net_points = "net_points",
      # The WPA ledger (docs/plans/WPA-NET-LEDGER.md), uncentred for the same
      # reason as net_points: wpa_net sums per team to the result minus the
      # pre-match forecast, and centring would break that. wpa_neutral starts
      # every match at an even 50/50 chance instead of the
      # forecast. Optional so frames built before the ledger still work.
      dplyr::any_of(c("wpa_net", "wpa_neutral", "wpa_own", "wpa_won", "wpa_team",
                      "wpa_neutral_own", "wpa_neutral_won", "wpa_neutral_team")),
      # net_points split into four parts, uncentred: sums of the play-type
      # breakdown's families (.np_raw_parts(), .NP_PLAY_TYPE_FAMILY). These add
      # up to net_points, where the centred epv_* channels below add up to the
      # rating `epv` instead.
      dplyr::any_of(.NP_FAMILY_COLS),
      epv = "epv_c", epv_recv = "epv_recv_c", epv_disp = "epv_disp_c",
      epv_spoil = "epv_spoil_c", epv_hitout = "epv_hitout_c",
      epv_p80 = "epv_p80", epv_recv_p80 = "epv_recv_p80", epv_disp_p80 = "epv_disp_p80",
      epv_spoil_p80 = "epv_spoil_p80", epv_hitout_p80 = "epv_hitout_p80",
      dplyr::any_of(c(
        wp_credit = "wp_credit_c", wp_disp_credit = "wp_disp_credit_c",
        wp_recv_credit = "wp_recv_credit_c",
        "wp_credit_p80", "wp_disp_credit_p80", "wp_recv_credit_p80"
      )),
      player_id = "player_id", team_id = "team_id", match_id = "match_id"
    )
}

#' The net points parts, captured before any centring
#'
#' The site shows `net_points` as EPV beside four parts that must add up to it:
#' `np_disp`, `np_recv`, `np_contest`, `np_team` (torp#263). Each is the sum of
#' its play types in the published `np_breakdown_<season>`
#' (`.NP_PLAY_TYPE_FAMILY`). `create_player_game_data()` builds them from the
#' breakdown and gates them against it, so a player game data frame that
#' carries all four is used as it is, after checking they still sum to
#' `net_points`.
#'
#' A frame without them (player game data built before torp#263, or by an
#' engine with no breakdown: every season before 2026 as released on
#' 2026-10-25) falls back to the raw channels, which split the same value
#' three ways, not four. `create_player_game_data()` publishes those as
#' `epv_disp` (own acts, INCLUDING receiving and errors), `epv_recv` (won back:
#' turnovers, contests, stoppages) and `epv_spoil` + `epv_hitout` (the pools).
#' They map onto the four columns as: `np_disp` = own acts, `np_recv` = NA
#' (the raw channels cannot separate receiving from disposing; it sits inside
#' `np_disp`), `np_contest` = won back, `np_team` = the pools. These are the
#' same three values the columns `np_own` / `np_won` / `np_team` carried
#' before torp#263, and `np_disp + np_contest + np_team` still equals
#' `net_points`. The fallback `np_team` is the whole pool (it includes the
#' defensive pool), so it is NOT the breakdown's team family.
#'
#' The `_raw` columns are used when an older frame has them. Call it on the
#' player game DATA (`.compute_player_game_ratings()`), never on the published
#' ratings, whose channels are already centred.
#' @keywords internal
.np_raw_parts <- function(df) {
  sum_gate <- function(df, cols, rows) {
    if (!"net_points" %in% names(df) || !any(rows)) return(invisible(NULL))
    tot <- Reduce(`+`, lapply(cols, function(v) df[[v]][rows]))
    gap <- suppressWarnings(max(abs(tot - df$net_points[rows]), na.rm = TRUE))
    if (is.finite(gap) && gap > 1e-6) {
      cli::cli_abort(c(
        "The net points parts do not add up to {.field net_points} (max gap {signif(gap, 3)}).",
        "x" = "{paste(cols, collapse = ' + ')} would sit beside net_points on the site and not sum to it."
      ))
    }
    invisible(NULL)
  }
  # Rows built from the play-type breakdown (create_player_game_data() fills
  # all four, never NA): keep them. Rebuilding from the channels would be
  # exactly the mismatch torp#263 fixed. Checked per row, not per frame: a
  # multi-season load binds a season that has the columns to ones that do
  # not, and fills theirs with NA.
  bd_row <- if (all(.NP_FAMILY_COLS %in% names(df))) {
    Reduce(`&`, lapply(.NP_FAMILY_COLS, function(v) !is.na(df[[v]])))
  } else {
    rep(FALSE, nrow(df))
  }
  sum_gate(df, .NP_FAMILY_COLS, bd_row)
  if (nrow(df) > 0 && all(bd_row)) return(df)
  raw <- function(n) {
    r <- paste0(n, "_raw")
    if (r %in% names(df)) df[[r]] else if (n %in% names(df)) df[[n]] else NULL
  }
  own <- raw("epv_disp"); won <- raw("epv_recv"); spoil <- raw("epv_spoil")
  if (is.null(own) || is.null(won) || is.null(spoil)) return(df)
  hit <- raw("epv_hitout"); if (is.null(hit)) hit <- 0
  hit <- dplyr::coalesce(hit + 0 * own, 0)
  fb <- !bd_row
  if (any(fb)) cli::cli_alert_info(
    "No play-type breakdown for {sum(fb)} player-match{?es} (of {nrow(df)}): {.field np_disp} holds all own acts (receiving included), {.field np_recv} is NA, {.field np_contest} is what he won back, {.field np_team} the whole pool.")
  keep <- function(v) if (v %in% names(df)) df[[v]] else rep(NA_real_, nrow(df))
  np_disp <- keep("np_disp"); np_recv <- keep("np_recv")
  np_contest <- keep("np_contest"); np_team <- keep("np_team")
  np_disp[fb] <- own[fb]
  np_recv[fb] <- NA_real_
  np_contest[fb] <- won[fb]
  np_team[fb] <- spoil[fb] + hit[fb]
  df$np_disp <- np_disp
  df$np_recv <- np_recv
  df$np_contest <- np_contest
  df$np_team <- np_team
  sum_gate(df, c("np_disp", "np_contest", "np_team"), fb)
  df
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
      epv_recv = round(.data$epv_recv_raw -
        dplyr::if_else(.data$.total_tog > 0, sum(.data$epv_recv_raw) / .data$.total_tog * .data$tog, 0), 1),
      epv_disp = round(.data$epv_disp_raw -
        dplyr::if_else(.data$.total_tog > 0, sum(.data$epv_disp_raw) / .data$.total_tog * .data$tog, 0), 1),
      epv_spoil = round(.data$epv_spoil_raw -
        dplyr::if_else(.data$.total_tog > 0, sum(.data$epv_spoil_raw) / .data$.total_tog * .data$tog, 0), 1),
      epv_hitout = round(.data$epv_hitout_raw -
        dplyr::if_else(.data$.total_tog > 0, sum(.data$epv_hitout_raw) / .data$.total_tog * .data$tog, 0), 1),
      epv = round(.data$epv_recv + .data$epv_disp +
        .data$epv_spoil + .data$epv_hitout, 1),
      .by = c("season", "lineup_position")
    ) |>
    dplyr::select(-".total_tog") |>
    dplyr::mutate(
      .safe_tog = .tog_floor(.data$tog),
      epv_p80 = round(.data$epv / .data$.safe_tog, 1),
      epv_recv_p80 = round(.data$epv_recv / .data$.safe_tog, 1),
      epv_disp_p80 = round(.data$epv_disp / .data$.safe_tog, 1),
      epv_spoil_p80 = round(.data$epv_spoil / .data$.safe_tog, 1),
      epv_hitout_p80 = round(.data$epv_hitout / .data$.safe_tog, 1)
    ) |>
    dplyr::select(-".safe_tog") |>
    dplyr::select(-"epv_raw", -"epv_recv_raw", -"epv_disp_raw",
                   -"epv_spoil_raw", -"epv_hitout_raw")
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
  if ("epv_recv_raw" %in% names(pgr)) {
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
      # games with a known time on ground: the p80 denominator below. A game
      # with NA tog must not be counted at the player's average tog.
      n_tog = sum(!is.na(tog)),
      avg_tog = round(mean(tog, na.rm = TRUE), 2)
    )
    # EPV columns (always present)
    for (col in c("epv", "epv_recv", "epv_disp", "epv_spoil", "epv_hitout")) {
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
  epv_cols <- intersect(c("epv", "epv_recv", "epv_disp", "epv_spoil", "epv_hitout"), names(result))
  wp_cols <- intersect(c("wp_credit", "wp_disp_credit", "wp_recv_credit"), names(result))
  psv_cols <- intersect(c("psv", "osv", "dsv", "torp_value"), names(result))
  all_val_cols <- c(epv_cols, wp_cols, psv_cols)

  for (col in all_val_cols) {
    p80_col <- paste0(col, "_p80")
    result[, (p80_col) := round(get(col) / (n_tog * avg_tog), 1)]
  }
  result[, n_tog := NULL]

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
    ps[, tog := .tog_floor(as.numeric(time_on_ground_percentage) / 100)]
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
      n_tog = sum(!is.na(tog)),
      avg_tog = round(mean(tog, na.rm = TRUE), 2)
    )
    for (col in stat_cols) out[[col]] <- sum(get(col), na.rm = TRUE)
    out
  }, by = .(season, player_name, player_id)]

  # Per-80 mode: divide by (games * avg_tog) to get per-full-game rates
  if (per80) {
    for (col in stat_cols) {
      result[, (col) := round(get(col) / (n_tog * avg_tog), 1)]
    }
  }

  result[, n_tog := NULL]

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

#' Add PSV and per-game TORP value to player game ratings
#'
#' `.compute_player_game_ratings()` builds the EPV side only. PSV (box-score
#' value) and `torp_value` (50/50 EPV and PSV) were added by the ratings
#' pipeline alone (`run_ratings_pipeline.R`), so the daily release, which also
#' writes `player_game_ratings_<season>`, published the file with no PSV until
#' the ratings workflow ran again. The blog's game-logs coverage gate then stopped
#' every AFL blog build (2026-09-27; it had shipped blank PSV silently before the
#' gate existed). Both writers call this now.
#'
#' @param pgr Output of `.compute_player_game_ratings()` for one season.
#' @param pstats_season Player stats for the same season (`load_player_stats()`).
#' @param season Season, for messages.
#' @param strict If `TRUE`, abort when PSV could not be computed, or left any
#'   row with an NA `psv`, instead of returning `pgr` without it. Default `FALSE`, which keeps the old behaviour
#'   of returning the EPV-only frame, but now says so immediately and tags the
#'   result (see Value).
#' @return `pgr` with psv/osv/dsv, their per-80 columns and torp_value(_p80)
#'   when PSV could be computed. When it could not (stats opponent adjustment
#'   or PSV failed, no player stats, or no usable PSV columns), `pgr` comes back
#'   without them and carries `attr(, "psv_missing") == TRUE`; a release
#'   pipeline should check that attribute (or pass `strict = TRUE`) before
#'   publishing.
#' @keywords internal
.add_psv_to_game_ratings <- function(pgr, pstats_season, season, strict = FALSE) {
  psv_fail <- function(why) {
    # cli_warn() is deferred to the end of a long run, by which time the partial
    # frame has usually been published; an alert prints now.
    if (isTRUE(strict)) {
      cli::cli_abort("PSV missing from the {season} game ratings: {why}")
    }
    cli::cli_alert_danger("PSV missing from the {season} game ratings ({why}); returning EPV-only ratings.")
  }
  psv_failed <- FALSE
  pstats_season <- data.table::as.data.table(pstats_season)
  if (nrow(pstats_season) > 0) {
    # PSV expects tog as a fraction 0-1
    if (!"tog" %in% names(pstats_season) && "time_on_ground_percentage" %in% names(pstats_season)) {
      pstats_season[, tog := .tog_floor(time_on_ground_percentage / 100)]
    }
    # Carry position_group from EPV so PSV can centre by position
    if (!"position_group" %in% names(pstats_season) && "position_group" %in% names(pgr)) {
      pg_map <- unique(data.table::as.data.table(pgr)[, .(player_id, match_id, position_group)])
      pstats_season <- merge(pstats_season, pg_map,
                             by = intersect(c("player_id", "match_id"), names(pstats_season)),
                             all.x = TRUE)
    }
    pstats_season <- tryCatch(adjust_stats_for_opponents(pstats_season), error = function(e) {
      # Not fatal on its own (PSV can run on unadjusted stats), but it changes
      # the numbers, so say so now.
      cli::cli_alert_warning("Stat opponent adjustment failed for {season}: {conditionMessage(e)}; PSV uses unadjusted stats.")
      pstats_season
    })
    psv_err <- NULL
    psv_result <- tryCatch(.compute_psv(pstats_season), error = function(e) {
      psv_err <<- conditionMessage(e)
      NULL
    })
    if (is.null(psv_result)) {
      psv_failed <- TRUE
      psv_fail(paste0("PSV computation failed: ", if (is.null(psv_err)) "no result" else psv_err))
    } else {
      # Per-game (psv/osv/dsv) + centred per-80, both from calculate_psv() (#80)
      psv_cols <- intersect(c("psv", "osv", "dsv", "psv_p80", "osv_p80", "dsv_p80"), names(psv_result))
      if (length(psv_cols) > 0 && all(c("player_id", "match_id") %in% names(psv_result))) {
        psv_slim <- psv_result[, c("player_id", "match_id", psv_cols), with = FALSE]
        pgr <- merge(pgr, psv_slim, by = c("player_id", "match_id"), all.x = TRUE)
        cli::cli_inform("  Added PSV columns to game ratings ({sum(!is.na(pgr$psv))} matched)")
      } else {
        psv_failed <- TRUE
        psv_fail("PSV result lacks psv columns or the player_id/match_id keys")
      }
    }
  } else {
    psv_failed <- TRUE
    psv_fail("no player stats for the season")
  }
  # Per-game TORP value: 50% EPV + 50% PSV (parallels career TORP = 50% EPR + 50% PSR)
  if (all(c("epv", "psv") %in% names(pgr))) {
    pgr$torp_value <- round(TORP_EPR_WEIGHT * pgr$epv + (1 - TORP_EPR_WEIGHT) * pgr$psv, 1)
    pgr$torp_value_p80 <- round(pgr$torp_value / .tog_floor(pgr$tog), 1)
  }
  if (psv_failed || !"psv" %in% names(pgr)) attr(pgr, "psv_missing") <- TRUE
  # PSV can compute yet leave rows unmatched (the merge above keeps every pgr
  # row). Strict callers publish, and the blog's game-logs gate refuses any NA
  # psv, so a partial gap aborts too. Every published season 2021-2026 had PSV
  # on all rows (58,324 of 58,324, checked 2026-10-18).
  if (isTRUE(strict)) {
    # psv_fail() covers the whole-failure paths; this catches a result that
    # merged other columns (osv/dsv) but no psv, and rows left unmatched.
    if (!"psv" %in% names(pgr)) {
      cli::cli_abort("PSV missing from the {season} game ratings: the PSV result had no psv column")
    }
    n_na <- sum(is.na(pgr$psv))
    if (n_na > 0) {
      cli::cli_abort("PSV missing on {n_na} of {nrow(pgr)} rows of the {season} game ratings")
    }
  }
  pgr
}
