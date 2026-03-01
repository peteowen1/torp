# TORP Package Constants
# ======================
# Centralized constants for the torp package to avoid magic numbers

# AFL Field and Game Constants
# ----------------------------

#' AFL goal post width in meters
#' @keywords internal
AFL_GOAL_WIDTH <- 6.4

#' Duration of AFL quarter in game seconds
#' Median max period_seconds per quarter is ~1862 across 2021-2025 data.
#' Set to 2000 as a safe upper bound covering P95 (~2100) without outliers.
#' @keywords internal
AFL_QUARTER_DURATION <- 2000

#' Total game duration in game seconds (4 quarters)
#' @keywords internal
AFL_TOTAL_GAME_SECONDS <- 8000

#' Number of quarters in a standard AFL match
#' @keywords internal
AFL_MAX_PERIODS <- 4

#' Maximum value for time left scaler calculation
#' @keywords internal
AFL_TIME_SCALER_MAX <- 4


# Rating System Constants
# -----------------------

#' Decay factor (in days) for PBP-level game recency weighting in add_epv_vars()
#' @keywords internal
EPV_WEIGHT_DECAY_DAYS <- 365

#' Default decay factor (in days) for TORP rating historical weighting
#' @keywords internal
RATING_DECAY_DEFAULT_DAYS <- 513

#' Loading factor for TORP calculations
#' @keywords internal
RATING_LOADING_DEFAULT <- 1.0000

#' Prior games constant for receiving ratings
#' @keywords internal
RATING_PRIOR_GAMES_RECV <- 6.0442

#' Prior games constant for disposal ratings
#' @keywords internal
RATING_PRIOR_GAMES_DISP <- 8.0334

#' Prior games constant for spoil ratings
#' @keywords internal
RATING_PRIOR_GAMES_SPOIL <- 3.0000

#' Prior games constant for hitout ratings
#' @keywords internal
RATING_PRIOR_GAMES_HITOUT <- 5.3162


# Credit Assignment Constants
# ----------------------------

#' Disposal EPV offset when defending (pos_team == -1)
#' @keywords internal
CREDIT_DISP_NEG_OFFSET <- -0.4459

#' Disposal EPV offset when possessing (pos_team == 1)
#' @keywords internal
CREDIT_DISP_POS_OFFSET <- 0.1189

#' Disposal scaling factor
#' @keywords internal
CREDIT_DISP_SCALE <- 0.7798

#' Bounce penalty per bounce
#' @keywords internal
CREDIT_BOUNCE_PENALTY <- 1.0000

#' Reception multiplier when defending (pos_team == -1)
#' @keywords internal
CREDIT_RECV_NEG_MULT <- 1.2841

#' Reception offset when defending
#' @keywords internal
CREDIT_RECV_NEG_OFFSET <- 0.5000

#' Reception multiplier when possessing (pos_team == 1)
#' @keywords internal
CREDIT_RECV_POS_MULT <- 1.1283

#' Reception offset when possessing
#' @keywords internal
CREDIT_RECV_POS_OFFSET <- 0.2231

#' Reception scaling factor
#' @keywords internal
CREDIT_RECV_SCALE <- 0.4234

#' Spoil weight per spoil
#' @keywords internal
CREDIT_SPOIL_WT <- 1.0531

#' Tackle weight per tackle
#' @keywords internal
CREDIT_TACKLE_WT <- 1.1408

#' Pressure act weight
#' @keywords internal
CREDIT_PRESSURE_WT <- 0.3072

#' Defensive half pressure act weight (subtracted)
#' @keywords internal
CREDIT_DEF_PRESSURE_WT <- 1.0312

#' Hitout weight per hitout
#' @keywords internal
CREDIT_HITOUT_WT <- 0.4528

#' Hitout to advantage weight
#' @keywords internal
CREDIT_HITOUT_ADV_WT <- 0.2523

#' Ruck contest weight (subtracted)
#' @keywords internal
CREDIT_RUCK_CONTEST_WT <- 0.0300

#' Position adjustment quantile for reception
#' @keywords internal
CREDIT_POS_ADJ_QUANTILE_RECV <- 0.3500

#' Position adjustment quantile for disposal
#' @keywords internal
CREDIT_POS_ADJ_QUANTILE_DISP <- 0.3500

#' Position adjustment quantile for spoil/tackle
#' @keywords internal
CREDIT_POS_ADJ_QUANTILE_SPOIL <- 0.3500

#' Position adjustment quantile for hitout
#' @keywords internal
CREDIT_POS_ADJ_QUANTILE_HITOUT <- 0.5100


# Simulation Constants
# --------------------

#' Noise standard deviation for match simulation
#' @keywords internal
SIM_NOISE_SD <- 26

#' Win probability scaling factor for simulations
#' @keywords internal
SIM_WP_SCALING_FACTOR <- 50

#' Home ground advantage in points
#' @keywords internal
SIM_HOME_ADVANTAGE <- 6


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


# Skill Estimation Constants
# ---------------------------

#' Default exponential decay rate for rate stats (per day)
#' Half-life = ln(2) / 0.0019 ~ 365 days
#' @keywords internal
SKILL_LAMBDA_RATE_DEFAULT <- 0.0019

#' Default exponential decay rate for efficiency stats (per day)
#' Half-life = ln(2) / 0.0013 ~ 533 days
#' @keywords internal
SKILL_LAMBDA_EFFICIENCY_DEFAULT <- 0.0013

#' Prior pseudo-games for Gamma-Poisson rate stats
#' @keywords internal
SKILL_PRIOR_GAMES_DEFAULT <- 5

#' Prior pseudo-attempts for Beta-Binomial efficiency stats
#' @keywords internal
SKILL_PRIOR_ATTEMPTS_DEFAULT <- 30

#' Minimum weighted games for a player to appear in skill output
#' @keywords internal
SKILL_MIN_GAMES <- 3

#' Credible interval width (0.80 = 80% CI)
#' @keywords internal
SKILL_CREDIBLE_LEVEL <- 0.80


# Data Validation Constants
# -------------------------

#' High missing data threshold (proportion)
#' @keywords internal
VALIDATION_HIGH_MISSING_THRESHOLD <- 0.5

#' Minimum observations for calibration bins
#' @keywords internal
VALIDATION_MIN_CALIBRATION_BIN_SIZE <- 10
