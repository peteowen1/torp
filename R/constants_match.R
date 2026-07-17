# Match Prediction Model Constants
# ================================
# Constants for the match prediction GAM pipeline (xPoints / xScore diff /
# conversion / score diff / WP), in-play live WP scoring, and field-position
# zoning used by both the prediction model and PBP feature engineering.

# Win Probability Constants
# -------------------------

#' Final 5 minutes threshold (seconds remaining)
#' @keywords internal
WP_FINAL_5_MINS_SECONDS <- 300

#' Final 2 minutes threshold (seconds remaining)
#' @keywords internal
WP_FINAL_2_MINS_SECONDS <- 120

#' Score scaling factor for WP baseline model
#' @keywords internal
WP_SCORE_SCALING <- 20

#' Time scaling factor for WP baseline model
#' @keywords internal
WP_TIME_SCALING <- 15


# Field Position Constants
# ------------------------

#' Scoring zone boundary at 30m from goal
#' @keywords internal
FIELD_ZONE_SCORING_30 <- 30

#' Scoring zone boundary at 50m from goal
#' @keywords internal
FIELD_ZONE_SCORING_50 <- 50

#' Scoring zone boundary at 80m from goal
#' @keywords internal
FIELD_ZONE_SCORING_80 <- 80


# Match Model Constants
# ---------------------

#' Phase groupings for match model position columns
#' Maps lineup_position to broad phase categories
#' @keywords internal
MATCH_PHASE_MAP <- list(
  def = c("BPL", "BPR", "FB", "CHB", "HBFL", "HBFR"),
  mid = c("C", "WL", "WR", "R", "RR", "RK"),
  fwd = c("FPL", "FPR", "FF", "CHF", "HFFL", "HFFR"),
  int = c("INT", "SUB")
)

#' Position group aggregation map for match models
#' @keywords internal
MATCH_POS_GROUP_MAP <- list(
  backs         = c("BPL", "BPR", "FB"),
  half_backs    = c("HBFL", "HBFR", "CHB"),
  midfielders   = c("WL", "WR", "C"),
  followers     = c("R", "RR", "RK"),
  half_forwards = c("HFFL", "HFFR", "CHF"),
  forwards      = c("FPL", "FPR", "FF")
)

#' Individual field position codes for match models (18 on-field positions)
#' @keywords internal
MATCH_INDIVIDUAL_POS <- c(
  "BPL", "BPR", "FB", "HBFL", "HBFR", "CHB",
  "WL", "WR", "C", "R", "RR", "RK",
  "HFFL", "HFFR", "CHF", "FPL", "FPR", "FF"
)

#' Combined position map for match models (structural pairs/trios)
#' @keywords internal
MATCH_COMBO_POS_MAP <- list(
  CB   = c("CHB", "FB"),
  BP   = c("BPL", "BPR"),
  HBF  = c("HBFL", "HBFR"),
  W    = c("WL", "WR"),
  MIDS = c("C", "R", "RR"),
  HFF  = c("HFFL", "HFFR"),
  FP   = c("FPL", "FPR"),
  CF   = c("FF", "CHF")
)

#' Listed position map for match models (position_group from torp ratings)
#' Combines MEDIUM_FORWARD and MIDFIELDER_FORWARD into med_fwd.
#' Entries are vectors so matching must use %in%, not ==.
#' @keywords internal
MATCH_LISTED_POS_MAP <- list(
  key_def  = "KEY_DEFENDER",
  med_def  = "MEDIUM_DEFENDER",
  midfield = "MIDFIELDER",
  med_fwd  = c("MEDIUM_FORWARD", "MIDFIELDER_FORWARD"),
  key_fwd  = "KEY_FORWARD",
  rucks    = "RUCK"
)

#' All generated position column names for match model aggregation
#' @keywords internal
MATCH_POS_COLS <- c(
  names(MATCH_PHASE_MAP), names(MATCH_POS_GROUP_MAP), MATCH_INDIVIDUAL_POS,
  names(MATCH_COMBO_POS_MAP), names(MATCH_LISTED_POS_MAP), "other_pos"
)

#' Exponential decay half-life in days for match model time-weighting
#' @keywords internal
MATCH_WEIGHT_DECAY_DAYS <- 1000

#' Offset added to Haversine distance (metres) before log-transform
#' Prevents log(0) for home games and dampens the curve for short trips
#' @keywords internal
MATCH_LOG_DIST_OFFSET <- 10000

#' Default log-distance value when venue coordinates are missing
#' @keywords internal
MATCH_LOG_DIST_DEFAULT <- 16

#' Earliest season with reliable TORP + xG data for match modelling
#' @keywords internal
MATCH_MIN_DATA_SEASON <- 2021

#' Earliest round in MATCH_MIN_DATA_SEASON with reliable data
#' @keywords internal
MATCH_MIN_DATA_ROUND <- 14


# XGBoost Training Constants
# --------------------------

#' Thread cap for every xgboost::xgb.train()/xgb.cv() call in match model
#' training (2026-07, reproducibility fix). XGBoost's `tree_method = "hist"`
#' is not deterministic across different thread counts even with a fixed
#' seed (confirmed empirically -- see docs/plans/FABLE-MATCH-MAE-PLAN.md §8)
#' -- left unset, a retrain on a machine/CI runner with a different core
#' count than whatever last built the shipped model can shift the resulting
#' trees for reasons unrelated to data or code changes. Matches
#' mgcv::bam()'s existing `nthreads = 4` convention used throughout
#' match_train.R.
#' @keywords internal
MATCH_XGB_NTHREAD <- 4L


# Team Elo Constants
# ------------------
# Feature added to the match model (2026-07, FABLE-MATCH-MAE-PLAN.md WS2/WS5
# "C6" candidate) -- a dynamic, results-based team-strength signal the
# player-rating-derived features (epr/psr/torp_diff) structurally lack.
# Hyperparameters tuned via grid search on pre-2025 data only (walk-forward,
# no leakage into the confirmation window): k in {15,20,30}, hga in
# {25,35,45}, carryover in {0.6,0.75,0.9}, scored on in-sample MAE of the
# resulting Elo-implied margin (honest since elo_pre is inherently
# point-in-time).

#' Elo update rate (K-factor)
#' @keywords internal
ELO_K <- 20

#' Elo home-ground advantage, in Elo points
#' @keywords internal
ELO_HGA <- 45

#' Elo season-boundary carryover (fraction of rating retained; 1 = no
#' regression to the 1500 mean, 0 = full reset)
#' @keywords internal
ELO_CARRYOVER <- 0.75


# Margin Recalibration Constants
# ------------------------------
# Post-hoc scaling of the blended match-model margin prediction (2026-07,
# FABLE-MATCH-MAE-PLAN.md WS1 "V1a" -- a leak-safe expanding-window slope
# fit on temporal-holdout OOS predictions). Mirrors the WP calibration
# sidecar pattern (wp_calibration.rds): fit once per retrain, applied at
# serve time with an identity (b=1) fallback when absent or when there's
# insufficient OOS history.

#' Minimum OOS history MATCH count required before fitting a real
#' recalibration scale (below this, use the identity fallback b=1 -- matches
#' WS1's cold-start guard). Compared against
#' \code{fit_match_margin_calibration()}'s \code{n_oos}, which is a match
#' count (that function's underlying \code{team_mdl_df} is long -- two rows
#' per match -- so it halves the raw OOS row count before comparing here).
#' @keywords internal
MATCH_RECAL_MIN_N <- 30

#' Safe band for the fitted margin recalibration slope AND the raw (pre-
#' calibration) rolling-OOS margin slope -- a production retrain aborts if
#' either leaves this band (mirrors the WP temporal slope gate). Chosen per
#' FABLE-MATCH-MAE-PLAN.md's expected end-state (slope 0.93-1.05 was the
#' target; widened slightly for a release gate vs. a research target).
#' @keywords internal
MATCH_MARGIN_SLOPE_GATE <- c(0.85, 1.10)


# Directed Matchup Table Constants
# ---------------------------------
# build_matchup_table() (torp#108) fabricates hypothetical fixture rows for
# every (host, visitor, venue-tier) combination and scores them with the
# real GAM+XGBoost match model. Only `days_rest_diff_fac` (the ROUNDED
# DIFFERENCE between the two teams' days_rest) is a model feature -- no
# formula uses raw days_rest.x/.y -- so any constant works numerically as
# long as it is EQUAL for both teams (days_rest_diff = 0). The value below
# is chosen only for documentation/plausibility (a finals matchup is never a
# season-opener), not because its magnitude affects predictions.

#' Assumed days' rest for BOTH teams in a fabricated matchup-table row
#'
#' A "sensible GF-era constant" (torp#108): the historical gap from a
#' Preliminary Final to the Grand Final under standard AFL finals scheduling.
#' Applied symmetrically (both host and visitor), so it only ever produces
#' \code{days_rest_diff_fac = "0"} -- the actual number is cosmetic.
#' @keywords internal
MATCHUP_TABLE_DAYS_REST <- 13
