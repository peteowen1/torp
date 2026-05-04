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
