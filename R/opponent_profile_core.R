# Opponent-Profile Core
# =====================
# The two arithmetic rules that every rolling opponent profile in this package
# shares. Extracted 2026-08-11 from three copies of the shrinkage line and four
# of the decay line, spread across `opponent_adjustment.R` (stat ratings) and
# `epv_opponent_adjustment.R` (EPV).
#
# DELIBERATELY SMALL. The architecture review proposed merging the profile
# builders into one module with an `additive`/`multiplicative` switch. That is
# not safely available: the builders differ in shape (one value vs a matrix of
# columns, the stat path vectorised for speed) and, more importantly, in
# semantics -- they divide/subtract against DIFFERENT league references. The
# stat path uses the same decay-weighted league mean it shrank toward; the EPV
# path uses `mean(epv_allowed_avg), by = match_id`, the unweighted mean of team
# profiles (`epv_opponent_adjustment.R:106`). Unifying those would change
# published ratings, which is a gate decision, not a refactor.
#
# What IS safely shared is the arithmetic itself. These helpers preserve each
# caller's expression exactly -- same operations, same order, same operands --
# so every published number is bit-identical, and the formulae now have one
# definition to change instead of three.

#' Exponential recency weight
#'
#' `exp(-lambda_decay * days_ago)`. Appeared identically at four call sites.
#'
#' @param days_ago Numeric days between the reference date and the match.
#'   Callers pass `as.numeric(ref - date)`; this does not coerce, so a
#'   difftime slipping through stays visible rather than being silently
#'   reinterpreted in different units.
#' @param lambda_decay Decay rate per day. 0 disables decay (every weight 1),
#'   which is what the tests use to keep the arithmetic checkable by hand.
#' @return Numeric vector of weights in (0, 1].
#' @keywords internal
.decay_weight <- function(days_ago, lambda_decay) {
  exp(-lambda_decay * days_ago)
}

#' Shrink a team's weighted mean toward the league average
#'
#' `(wt_sum * wt_mean + prior_games * league_avg) / (wt_sum + prior_games)`.
#' Appeared identically at three call sites.
#'
#' The shrinkage is on the cell's own WEIGHT, not its game count: a team whose
#' prior games are all heavily decayed carries little evidence, and weight says
#' that where a count does not. `prior_games = 0` reproduces the unshrunk
#' weighted mean exactly, PROVIDED `wt_sum > 0`.
#'
#' A cell with no weight and no prior (`wt_sum + prior_games == 0`), or one
#' whose mean is itself `NaN` because it was built as `0 / 0`, has nothing to
#' report. The raw formula would give `NaN` there; this returns `league_avg`,
#' the only defensible value for "no evidence". Every finite result is left
#' bit-identical (the guard only touches elements that would be `NaN`).
#'
#' Note the ordering with the cap that callers apply afterwards: shrinkage runs
#' FIRST and does most of the work. On a two-team fixture with a 1-vs-10000
#' spread, `prior_games = 5` alone keeps the resulting factor inside
#' `OPP_ADJ_FACTOR_CAP` -- the cap is a backstop behind this, not the primary
#' control (see test-opponent-adjustment-profiles.R).
#'
#' @param wt_sum Total weight of the team's prior games.
#' @param wt_mean The team's weighted mean over those games.
#' @param league_avg The weighted league mean to shrink toward.
#' @param prior_games Strength of the prior, in units of weight.
#' @return The shrunk value.
#' @keywords internal
.shrink_to_league <- function(wt_sum, wt_mean, league_avg, prior_games) {
  out <- (wt_sum * wt_mean + prior_games * league_avg) / (wt_sum + prior_games)
  empty <- rep_len(!is.na(wt_sum) & wt_sum == 0, length(out)) & is.nan(out)
  if (any(empty)) out[empty] <- rep_len(league_avg, length(out))[empty]
  out
}

#' Time-on-ground weight from a percentage
#'
#' `pmax(tog / 100, 0.1)` with a missing time-on-ground treated as a full game
#' (100). The expression was written out by hand at five call sites
#' (`player_ratings.R`, `psr.R`, `epv_opponent_adjustment.R`); this is its one
#' definition. It changes no number: `NA` becomes 1, everything else is the
#' fraction of the game on the ground, floored at 0.1 so a one-minute cameo
#' cannot receive a near-zero (or negative) weight.
#'
#' @param tog Time on ground as a PERCENTAGE (0-100), as in the
#'   `time_on_ground_percentage` column. Integer or double. Not a 0-1 fraction:
#'   a fraction would be divided by 100 a second time and floor at 0.1.
#' @return Numeric vector of weights in [0.1, Inf), 1 for a full game.
#' @keywords internal
.tog_weight <- function(tog) {
  pmax(data.table::fifelse(is.na(tog), 100, as.numeric(tog)) / 100, 0.1)
}
