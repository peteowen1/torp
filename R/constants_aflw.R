# AFLW Rating Constants
# ======================
# Tunables for the AFLW box-score rating module (docs/plans/AFLW-MIGRATION-PLAN.md
# Phase 3). Deliberately NOT folded into constants_ratings.R -- AFLW ratings are a
# different method (box-score, not chain-derived EPV/PSV) with their own tuning
# surface, and must never be assumed comparable to men's TORP/EPR/PSR.

# AFLW Elo
# --------

#' AFLW Elo update rate
#'
#' Seeded from men's ELO_K as a starting prior -- NOT yet retuned on an AFLW-specific
#' grid search. Re-tune once enough AFLW seasons of results exist to run the same
#' walk-forward backtest used for the men's constant, then update this comment.
#' @keywords internal
AFLW_ELO_K <- 20

#' AFLW Elo home-ground advantage, in Elo points
#' Seeded from men's ELO_HGA -- not yet retuned. See AFLW_ELO_K.
#' @keywords internal
AFLW_ELO_HGA <- 45

#' AFLW Elo season-boundary carryover (fraction of rating retained)
#' Seeded from men's ELO_CARRYOVER -- not yet retuned. See AFLW_ELO_K.
#' @keywords internal
AFLW_ELO_CARRYOVER <- 0.75


# AFLW RAPM
# ---------

#' Minimum total time-on-ground minutes (summed across a player's matches)
#' below which a player does NOT get their own RAPM design-matrix column, and
#' instead pools into a shared "replacement-level" column per position.
#'
#' Exists because daisychain's whole-match-row AFLW RAPM matrix is
#' overparameterized (1,409 player columns against 690 match rows -- more
#' parameters than observations, see docs/plans/AFLW-MIGRATION-PLAN.md §6.2).
#' TOG-weighting alone does not fix this; column count has to come down to
#' fit the row count that exists.
#'
#' Empirically swept 2026-08-24 against \code{build_aflw_rapm_net()} /
#' \code{fit_aflw_rapm_net()} on all AFLW history (n=485 matches, 2021-2026):
#' CV R^2 rises from 0.4929 (threshold 0, p=844) to a peak of **0.5147 at
#' threshold 900** (p=474, n/p=1.023), then falls back down through 1200/1500/
#' 2000/3000 -- both too little pruning (noisy free parameters) and too much
#' (real per-player signal pooled away) cost accuracy; 900 is the measured
#' sweet spot, not a round-number guess. All of 0-3000 clear the pre-rework
#' 0.4765 baseline once the lambda-grid bug documented in
#' \code{fit_aflw_rapm_net()} is accounted for -- see that function's comment,
#' the grid choice mattered far more than this threshold does.
#' @keywords internal
AFLW_RAPM_MIN_TOG_MINUTES <- 900

#' First AFLW season
#'
#' AFLW's inaugural season. Three years before the men's
#' \code{AFL_MIN_SEASON} (2021), which is floored where torp's men's CHAIN
#' data starts -- a constraint AFLW does not share, since its ratings are
#' box-score derived (no chains exist for AFLW at all). Loaders over AFLW
#' releases must validate against this, not \code{AFL_MIN_SEASON}, or they
#' abort on the three earliest seasons.
#' @keywords internal
AFLW_MIN_SEASON <- 2018L
