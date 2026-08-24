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
