# AFLW Team Elo Rating
# =====================
# AFLW analog of team_elo.R. build_team_elo() itself is already comp-agnostic
# (takes a plain match_id/date/season/home_team/away_team/home_margin frame) and
# needed ZERO changes -- the only new code here is sourcing that frame from
# AFLW's own results instead of the men's chain-derived team_mdl_df, and a
# separate set of AFLW-tuned Elo constants (constants_aflw.R) so a men's-tuned
# K/HGA/carryover is never silently reused on a different competition.
#
# Do not brand this "TORP for AFLW" or treat it as comparable to men's team Elo
# -- same algorithm, different competition, different tuning, not validated
# against the same backtest window.

# .matches_from_aflw_results ----

#' Build a one-row-per-match table from AFLW results for Elo construction
#'
#' AFLW analog of \code{.matches_from_team_mdl_df()}, sourced directly from
#' \code{load_results(comp = "AFLW")} rather than the chain-derived
#' \code{team_mdl_df} men's Elo reads (AFLW has no chain data to build that
#' from -- see docs/plans/AFLW-MIGRATION-PLAN.md §0).
#'
#' @param seasons Numeric vector of seasons, or \code{TRUE} for all available
#'   AFLW seasons. Passed straight to \code{load_results()}.
#' @return data.frame(match_id, date, season, home_team, away_team,
#'   home_margin), completed matches only, sorted by date then match_id --
#'   identical shape to \code{.matches_from_team_mdl_df()}'s output, so it
#'   feeds \code{build_team_elo()} unchanged.
#' @keywords internal
.matches_from_aflw_results <- function(seasons = TRUE) {
  res <- load_results(seasons, comp = "AFLW")
  if (nrow(res) == 0) {
    return(data.frame(
      match_id = character(0), date = as.Date(character(0)),
      season = integer(0), home_team = character(0), away_team = character(0),
      home_margin = integer(0), stringsAsFactors = FALSE
    ))
  }

  data.frame(
    match_id    = res$match_id,
    date        = as.Date(substr(res$utc_start_time, 1, 10)),
    season      = res$season,
    home_team   = as.character(res$home_team_name),
    away_team   = as.character(res$away_team_name),
    home_margin = res$home_score - res$away_score,
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(date, match_id)
}

# build_aflw_team_elo ----

#' Sequential AFLW team-Elo rating, pre-match ratings only
#'
#' Thin AFLW wrapper around \code{build_team_elo()} -- same algorithm (538-style
#' margin-of-victory multiplier, season carryover), AFLW-tuned constants,
#' AFLW-sourced match data.
#'
#' @param seasons Numeric vector of seasons, or \code{TRUE} for all available
#'   AFLW seasons. Passed to \code{.matches_from_aflw_results()}.
#' @inheritParams build_team_elo
#' @return Same shape as \code{build_team_elo()}: list(by_match, current).
#' @keywords internal
build_aflw_team_elo <- function(seasons = TRUE, k = AFLW_ELO_K, hga = AFLW_ELO_HGA,
                                carryover = AFLW_ELO_CARRYOVER, mov_mult = TRUE) {
  matches <- .matches_from_aflw_results(seasons)
  build_team_elo(matches, k = k, hga = hga, carryover = carryover, mov_mult = mov_mult)
}
