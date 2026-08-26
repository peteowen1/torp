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
#' data-raw/04-analysis/rapm_general.R prototype used (docs/plans/
#' FABLE-DEFENDER-VALUE-PLAN.md sec7.22), which found MIN_GAMES=10 gave
#' 888 individually-rated players from 1,182 matches. Swept fresh against
#' current data volume (docs/plans/AFLM-RAPM-SPM-PLAN.md) rather than
#' assumed to still be optimal.
#' @keywords internal
TEAM_RAPM_MIN_GAMES <- 10L
