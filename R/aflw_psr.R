# AFLW PSR/PSV Data Preparation
# ==============================
# .prepare_stat_rating_data() (player_skills_data.R) is fully reusable for
# AFLW -- its logic is box-score stats + roster/position scaffolding, none of
# it chain-derived. The reason it can't be called with the usual men's
# arguments is narrower than an earlier audit this session assumed: its
# `player_game_data` input normally comes from load_player_game_data(), which
# IS chain-derived and has no AFLW equivalent (no chains -- see
# docs/plans/AFLW-MIGRATION-PLAN.md Sec 0). This file supplies an AFLW-shaped
# substitute assembled entirely from load_player_stats(comp = "AFLW"), which
# independently carries everything .prepare_stat_rating_data() needs --
# box-score stats, time_on_ground_percentage, and match context -- sourced
# from the AFL API's own playerStats/match endpoint, not torp's chain
# pipeline. calculate_psr()/calculate_psv() themselves need zero changes.

#' Assemble the AFLW stat-rating input table
#'
#' AFLW substitute for the load_player_game_data(TRUE) + load_player_stats(TRUE)
#' pair .prepare_stat_rating_data() normally takes -- both become the SAME
#' AFLW player_stats table here, since that single source already carries
#' everything the function needs.
#'
#' @param seasons Numeric vector of seasons, or TRUE for all available AFLW
#'   seasons.
#' @return A data.table, same shape .prepare_stat_rating_data() produces for
#'   men's data (one row per player-match with pos_group, tog, stat columns).
#' @keywords internal
.prepare_aflw_stat_rating_data <- function(seasons = TRUE) {
  ps <- data.table::as.data.table(load_player_stats(seasons, comp = "AFLW"))
  if (nrow(ps) == 0) {
    cli::cli_abort("No AFLW player_stats returned for seasons {paste(seasons, collapse = ', ')}")
  }

  # .prepare_stat_rating_data() expects `round`, not the raw API's
  # `round_number` (men's chain-derived player_game_data already uses `round`).
  if ("round_number" %in% names(ps) && !"round" %in% names(ps)) {
    data.table::setnames(ps, "round_number", "round")
  }

  # Derive team/opponent -- load_player_stats() carries home/away team names
  # + team_status ("home"/"away"), not a direct team/opponent pair (mirrors
  # the derivation psr.R's explain_player_game() already does for men's data).
  ps[, team := data.table::fifelse(team_status == "home", home_team_name, away_team_name)]
  ps[, opponent := data.table::fifelse(team_status == "home", away_team_name, home_team_name)]

  teams <- data.table::as.data.table(load_teams(seasons, comp = "AFLW"))
  fixtures <- data.table::as.data.table(load_fixtures(seasons, comp = "AFLW"))

  # .prepare_stat_rating_data()'s `rosters` arg expects player_id/season/team
  # (team, not team_name) plus a `position` column resolved via
  # .map_position_group() -- which already maps lineup_position values via
  # LINEUP_POSITION_GROUP_MAP, so passing lineup_position AS position here is
  # correct, not a hack.
  rosters_aflw <- unique(teams[, .(
    player_id, season,
    team = team_name,
    position = lineup_position
  )])

  .prepare_stat_rating_data(
    player_game_data = ps,
    player_stats = ps,
    rosters = rosters_aflw,
    fixtures = fixtures,
    teams = teams
  )
}

#' Build AFLW per-round stat ratings
#'
#' The SCORING half of what \code{data-raw/06-stat-ratings/aflw_run_pipeline.R}
#' does end-to-end. That script is a TRAINING script -- it also fits glmnet
#' models and rewrites \code{inst/extdata/*_coefficients_aflw.csv}, i.e. the
#' rating DEFINITION. This function deliberately stops short of that: the
#' per-round Bayesian estimate is a pure function of box-score data plus
#' \code{default_stat_rating_params()}, so it can run on every pipeline pass
#' without re-deriving what a rating means. Retraining on a daily cadence is
#' exactly what the vintage system exists to prevent.
#'
#' @param seasons Numeric vector of seasons, or TRUE for all available AFLW
#'   seasons.
#' @return A data.table of player-round stat ratings (one row per player per
#'   played round), or NULL when no round could be estimated.
#' @keywords internal
.build_aflw_stat_ratings <- function(seasons = TRUE) {
  stat_rating_data <- .prepare_aflw_stat_rating_data(seasons)
  if (nrow(stat_rating_data) == 0) {
    cli::cli_warn(".build_aflw_stat_ratings: no AFLW stat-rating rows -- returning NULL.")
    return(NULL)
  }

  fixtures <- data.table::as.data.table(load_fixtures(TRUE, comp = "AFLW"))
  season_vec <- sort(unique(stat_rating_data$season))

  # PLAYED rounds only. load_fixtures() returns the full scheduled list, and
  # estimating at a future ref_date yields drifting phantom rounds that reorder
  # any leaderboard read off max(round) -- and would publish them to a release.
  ref_date_map <- .played_round_ref_dates(fixtures, seasons = season_vec)
  if (nrow(ref_date_map) == 0) {
    cli::cli_warn(".build_aflw_stat_ratings: no PLAYED rounds for seasons {paste(season_vec, collapse = ', ')} -- returning NULL.")
    return(NULL)
  }

  # Cross-feed check -- see 03_estimate_stat_ratings.R for the full reasoning.
  # The map is built from FIXTURE scores; the rows being rated come from the
  # player-stats feed. If results lag stats (a live race on the pre-game
  # schedule this runs on), a played round exists in stat_rating_data but not
  # in the map and is silently dropped -- every count below derives from the
  # already-filtered map, so nothing else would notice. The played-rounds guard
  # above catches phantom FUTURE rounds; only this catches a MISSING one.
  .assert_ref_date_coverage(ref_date_map, stat_rating_data, label = "AFLW")

  batch_results <- .estimate_stat_ratings_batch(
    stat_rating_data,
    ref_dates = ref_date_map$ref_date,
    params = default_stat_rating_params(),
    compute_ci = FALSE
  )

  out <- vector("list", nrow(ref_date_map))
  counter <- 0L
  for (i in seq_len(nrow(ref_date_map))) {
    rd_key <- as.character(ref_date_map$ref_date[i])
    if (!rd_key %in% names(batch_results)) next
    res <- batch_results[[rd_key]]
    if (is.null(res) || nrow(res) == 0) next
    res[, `:=`(season = ref_date_map$season[i], round = ref_date_map$round[i])]
    counter <- counter + 1L
    out[[counter]] <- res
  }

  if (counter == 0L) {
    cli::cli_alert_danger(".build_aflw_stat_ratings: all {nrow(ref_date_map)} round estimations failed -- returning NULL.")
    return(NULL)
  }
  if (counter < nrow(ref_date_map)) {
    # cli_alert_danger FIRST, not a bare cli_warn: warnings are deferred to the
    # end of an Rscript run and silently dropped past getOption("nwarnings").
    # See save_to_release() (R/load_data.R) -- that is exactly how the
    # 2026-07-29 CSV divergence left no trace in the log.
    n_missing <- nrow(ref_date_map) - counter
    cli::cli_alert_danger(
      ".build_aflw_stat_ratings: {n_missing} of {nrow(ref_date_map)} season-rounds could not be estimated.")
    cli::cli_warn(".build_aflw_stat_ratings: {n_missing} of {nrow(ref_date_map)} season-rounds could not be estimated.")
  }

  built <- data.table::rbindlist(out[seq_len(counter)], fill = TRUE)

  # A season present in the rating data but absent from the built output is
  # invisible downstream: Stage 7's own check groups the RESULT, so a wholly
  # missing season has no row to be counted against. Mirrors the same abort in
  # aflw_run_pipeline.R and 03_estimate_stat_ratings.R.
  built_seasons <- unique(built$season)
  empty_seasons <- setdiff(season_vec, built_seasons)
  if (length(empty_seasons) > 0) {
    cli::cli_abort(c(
      "{length(empty_seasons)} AFLW season{?s} with rating data produced ZERO checkpoints: {.val {as.character(empty_seasons)}}.",
      "x" = "Aborting rather than returning an artifact that silently omits them."
    ))
  }

  built
}

#' Compute AFLW PSR from frozen coefficients
#'
#' Scores AFLW player-rounds against the committed
#' \code{inst/extdata/psr_coefficients_aflw.csv} (plus the osr/dsr pair when
#' present). The coefficients are an input here, never an output -- see
#' \code{.build_aflw_stat_ratings()} for why that separation matters.
#'
#' @param seasons Numeric vector of seasons, or TRUE for all available AFLW
#'   seasons.
#' @param stat_ratings Optional pre-built stat ratings (skips the rebuild).
#' @return A data.table with \code{psr} (and \code{osr}/\code{dsr} when the
#'   component coefficients are available), or NULL when scoring is not
#'   possible.
#' @keywords internal
.compute_aflw_psr <- function(seasons = TRUE, stat_ratings = NULL) {
  if (is.null(stat_ratings)) stat_ratings <- .build_aflw_stat_ratings(seasons)
  if (is.null(stat_ratings) || nrow(stat_ratings) == 0) return(NULL)

  .compute_psr_from_stat_ratings(stat_ratings, comp = "AFLW")
}
