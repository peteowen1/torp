# Team RAPM/SPM Constants (AFLM)
# =================================
# Deliberately a separate file from constants_ratings.R -- this is a
# different, box-score-derived rating philosophy (see R/team_rapm.R's
# header), built to test whether it adds signal on top of torp's
# chain-native EPV/EPR/PSR/TORP, not to be confused with or tuned
# alongside them. AFLW's equivalent pruning constant
# (AFLW_RAPM_MIN_TOG_MINUTES) lives in constants_aflw.R alongside its other
# AFLW-specific tunables -- kept separate rather than merged here, since the
# two comps use genuinely different pruning UNITS (see team_rapm.R's
# .team_rapm_prune_columns() header for why).

#' Minimum game appearances for a player to get their own RAPM design-matrix
#' column, below which they pool into a shared replacement-level column per
#' position.
#'
#' Same unit (games, not TOG-minutes) as the validated
#' data-raw/04-analysis/archive-2026-09-07/rapm_general.R prototype used (docs/plans/
#' FABLE-DEFENDER-VALUE-PLAN.md sec7.22), which found MIN_GAMES=10 gave
#' 888 individually-rated players from 1,182 matches. Swept fresh against
#' current data volume (docs/plans/AFLM-RAPM-SPM-PLAN.md) rather than
#' assumed to still be optimal.
#' @keywords internal
TEAM_RAPM_MIN_GAMES <- 10L

#' Elastic-net mixing parameter for the team SPM fit (0 = ridge, 1 = lasso).
#' Defaults of `fit_team_spm()` / `build_team_spm_expanding()`.
#' @keywords internal
TEAM_SPM_ALPHA <- 0.5

#' Cross-validation folds for the team SPM fit (capped by training rows).
#' @keywords internal
TEAM_SPM_NFOLDS <- 10L

#' RNG seed for the team SPM CV fold assignment.
#' @keywords internal
TEAM_SPM_SEED <- 20260825

#' Shrinkage strength (games-equivalent) pulling team RAPM toward the SPM prior.
#' A documented placeholder, not swept for either comp.
#' @keywords internal
TEAM_SPM_PRIOR_GAMES <- 10

#' Earliest round that can be a season's FINAL xRAPM checkpoint, per comp. Used
#' to tell a fresh prior-season-final snapshot at round 1 from a snapshot that
#' stopped early in the prior season.
#' @keywords internal
TEAM_RAPM_MIN_FINAL_ROUND <- c(AFLM = 20L, AFLW = 10L)
