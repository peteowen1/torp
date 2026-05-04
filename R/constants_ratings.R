# Player Rating Constants
# =======================
# Constants for player rating composition: EPR (Expected Possession Rating),
# EPV (Expected Possession Value) component weights, PSR (Player Skill Rating),
# TORP blending weights, WP credit allocation, stat-rating estimation, and
# position-based TOG (time on ground) priors.

# EPR (Expected Possession Rating) System Constants
# -------------------------------------------------

#' Default total predicted TOG for league-wide centering (18 players per team x 18 teams).
#' Used as fallback; actual centering adapts to the number of teams present.
#' @keywords internal
TOTAL_PRED_TOG <- 324L

#' Decay factor (in days) for PBP-level game recency weighting in add_epv_vars()
#' @keywords internal
EPV_WEIGHT_DECAY_DAYS <- 365

#' Decay factor (in days) for receiving component weighting
#' @keywords internal
EPR_DECAY_RECV <- 273

#' Decay factor (in days) for disposal component weighting
#' @keywords internal
EPR_DECAY_DISP <- 630

#' Decay factor (in days) for spoil component weighting
#' @keywords internal
EPR_DECAY_SPOIL <- 523

#' Decay factor (in days) for hitout component weighting
#' @keywords internal
EPR_DECAY_HITOUT <- 545

#' Default decay factor (in days) — legacy alias for backwards compatibility
#' @keywords internal
EPR_DECAY_DEFAULT_DAYS <- EPR_DECAY_RECV

#' Loading factor for TORP calculations
#' @keywords internal
EPR_LOADING_DEFAULT <- 1.0000

#' Prior games constant for receiving ratings
#' @keywords internal
EPR_PRIOR_GAMES_RECV <- 3.0000

#' Prior games constant for disposal ratings
#' @keywords internal
EPR_PRIOR_GAMES_DISP <- 3.0000

#' Prior games constant for spoil ratings
#' @keywords internal
EPR_PRIOR_GAMES_SPOIL <- 3.0000

#' Prior games constant for hitout ratings
#' @keywords internal
EPR_PRIOR_GAMES_HITOUT <- 3.0000

#' Prior rate for receiving component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_RECV <- -0.7000

#' Prior rate for disposal component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_DISP <- -0.7000

#' Prior rate for spoil component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_SPOIL <- -0.3000

#' Prior rate for hitout component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_HITOUT <- -0.3000

#' Decay factor (in days) for contest component weighting
#' @keywords internal
EPR_DECAY_CONTEST <- EPR_DECAY_RECV

#' Prior games constant for contest ratings
#' @keywords internal
EPR_PRIOR_GAMES_CONTEST <- 3.0000

#' Prior rate for contest component (shrinkage target per weighted game).
#' Zero because contest credit is zero-sum — average player should be 0.
#' @keywords internal
EPR_PRIOR_RATE_CONTEST <- 0.0000

#' Weight of EPR in TORP blend (0.5 = equal blend of EPR + PSR)
#' @keywords internal
TORP_EPR_WEIGHT <- 0.5

#' PSR prior rate for replacement-level players
#'
#' Players without enough skill history to compute PSR are assigned this
#' replacement-level value. Based on empirical mean PSR of players with
#' 1-10 career games (~-1.5 to -2.0).
#' @keywords internal
PSR_PRIOR_RATE <- -2

# EPV (Expected Possession Value) Assignment Constants
# ----------------------------------------------------

#' Bounce weight in disposal credit
#' @keywords internal
EPV_BOUNCE_WT <- 0.0000

#' Disposal EPV offset when defending (pos_team == -1)
#' @keywords internal
EPV_DISP_NEG_OFFSET <- 0.0000

#' Disposal EPV offset when possessing (pos_team == 1)
#' @keywords internal
EPV_DISP_POS_OFFSET <- 0.0000

#' Disposal scaling factor
#' @keywords internal
EPV_DISP_SCALE <- 0.5000

#' Reception multiplier when defending (pos_team == -1)
#' @keywords internal
EPV_RECV_NEG_MULT <- 1.0000

#' Reception offset when defending
#' @keywords internal
EPV_RECV_NEG_OFFSET <- 0.0000

#' Reception multiplier when possessing (pos_team == 1)
#' @keywords internal
EPV_RECV_POS_MULT <- 1.0000

#' Reception offset when possessing
#' @keywords internal
EPV_RECV_POS_OFFSET <- 0.0000

#' Reception scaling factor
#' @keywords internal
EPV_RECV_SCALE <- 0.5000

#' Reception scaling factor for intercept marks (pos_team == -1 AND mark in PBP)
#' @keywords internal
EPV_RECV_INTERCEPT_MARK_SCALE <- 1.0000

#' Penalty scale for failed aerial contest receptions (target who lost)
#' Applied per failed contest; negative credit = share of kicker's lost EPV
#' @keywords internal
EPV_RECV_FAILED_CONTEST_WT <- -0.3000

#' Spoil weight per spoil
#' @keywords internal
EPV_SPOIL_WT <- 0.0737

#' Tackle weight per tackle
#' @keywords internal
EPV_TACKLE_WT <- 0.2980

#' Pressure act weight
#' @keywords internal
EPV_PRESSURE_WT <- -0.0024

#' Defensive half pressure act weight (spoil component)
#' @keywords internal
EPV_DEF_PRESSURE_WT <- -0.1882

#' Hitout weight per hitout
#' @keywords internal
EPV_HITOUT_WT <- 0.0510

#' Hitout to advantage weight
#' @keywords internal
EPV_HITOUT_ADV_WT <- 0.1748

#' Ruck contest weight (hitout component)
#' @keywords internal
EPV_RUCK_CONTEST_WT <- 0.0232

#' Contested possessions weight (recv component)
#' @keywords internal
EPV_CONTESTED_POSS_WT <- 0.1642

#' Contested marks weight (recv component)
#' @keywords internal
EPV_CONTESTED_MARKS_WT <- 0.0259

#' Ground ball gets weight (recv component)
#' @keywords internal
EPV_GROUND_BALL_GETS_WT <- 0.2165

#' Marks inside 50 weight (recv component)
#' @keywords internal
EPV_MARKS_INSIDE50_WT <- 0.3464

#' Inside 50s weight (disp component)
#' @keywords internal
EPV_INSIDE50S_WT <- 0.2429

#' Clangers weight (disp component)
#' @keywords internal
EPV_CLANGERS_WT <- -0.0094

#' Score involvements weight (disp component)
#' @keywords internal
EPV_SCORE_INVOLVEMENTS_WT <- 0.2916

#' Intercepts weight (spoil component)
#' @keywords internal
EPV_INTERCEPTS_WT <- 0.0166

#' One percenters weight (spoil component)
#' @keywords internal
EPV_ONE_PERCENTERS_WT <- 0.1260

#' Rebound 50s weight (spoil component)
#' @keywords internal
EPV_REBOUND50S_WT <- -0.1763

#' Frees against weight (spoil component)
#' @keywords internal
EPV_FREES_AGAINST_WT <- 0.0428

#' Frees for weight (recv component)
#' @keywords internal
EPV_FREES_FOR_WT <- 0.2331

#' Goals weight (disp component)
#' @keywords internal
EPV_GOALS_WT <- 0.4262

#' Behinds weight (disp component)
#' @keywords internal
EPV_BEHINDS_WT <- 1.0899

#' Total marks weight (recv component)
#' @keywords internal
EPV_MARKS_WT <- 0.0160

#' Uncontested possessions weight (recv component)
#' @keywords internal
EPV_UNCONTESTED_POSS_WT <- 0.0344

#' Shots at goal weight (disp component)
#' @keywords internal
EPV_SHOTS_AT_GOAL_WT <- 0.4419

#' Kicks weight (disp component)
#' @keywords internal
EPV_KICKS_WT <- 0.0680

#' Handballs weight (disp component)
#' @keywords internal
EPV_HANDBALLS_WT <- 0.0629

#' Metres gained weight (disp component)
#' @keywords internal
EPV_METRES_GAINED_WT <- 0.0010

#' Turnovers weight (disp component)
#' @keywords internal
EPV_TURNOVERS_WT <- -0.0856

#' Goal assists weight (disp component)
#' @keywords internal
EPV_GOAL_ASSISTS_WT <- 0.2240

#' L2 (ridge) regularization lambda for count-based stat weights
#' @keywords internal
STAT_WEIGHT_LAMBDA <- 0.5


# WP Credit Constants
# -------------------

#' Default disposer share of WPA in win probability credit assignment
#' @keywords internal
WP_CREDIT_DISP_SHARE <- 0.5


# Stat Rating Estimation Constants
# --------------------------------

#' Default exponential decay rate for rate stats (per day)
#' Half-life = ln(2) / 0.0019 ~ 365 days
#' @keywords internal
STAT_RATING_LAMBDA_RATE_DEFAULT <- 0.0019

#' Default exponential decay rate for efficiency stats (per day)
#' Half-life = ln(2) / 0.0013 ~ 533 days
#' @keywords internal
STAT_RATING_LAMBDA_EFFICIENCY_DEFAULT <- 0.0013

#' Prior pseudo-games for Gamma-Poisson rate stats
#' @keywords internal
STAT_RATING_PRIOR_GAMES_DEFAULT <- 5

#' Prior pseudo-attempts for Beta-Binomial efficiency stats
#' @keywords internal
STAT_RATING_PRIOR_ATTEMPTS_DEFAULT <- 30

#' Minimum weighted games for a player to appear in stat rating output
#' @keywords internal
STAT_RATING_MIN_GAMES <- 0

#' Credible interval width (0.80 = 80% CI)
#' @keywords internal
STAT_RATING_CREDIBLE_LEVEL <- 0.80

# Backward compatibility aliases
#' @rdname STAT_RATING_LAMBDA_RATE_DEFAULT
#' @keywords internal
SKILL_LAMBDA_RATE_DEFAULT <- STAT_RATING_LAMBDA_RATE_DEFAULT

#' @rdname STAT_RATING_LAMBDA_EFFICIENCY_DEFAULT
#' @keywords internal
SKILL_LAMBDA_EFFICIENCY_DEFAULT <- STAT_RATING_LAMBDA_EFFICIENCY_DEFAULT

#' @rdname STAT_RATING_PRIOR_GAMES_DEFAULT
#' @keywords internal
SKILL_PRIOR_GAMES_DEFAULT <- STAT_RATING_PRIOR_GAMES_DEFAULT

#' @rdname STAT_RATING_PRIOR_ATTEMPTS_DEFAULT
#' @keywords internal
SKILL_PRIOR_ATTEMPTS_DEFAULT <- STAT_RATING_PRIOR_ATTEMPTS_DEFAULT

#' @rdname STAT_RATING_MIN_GAMES
#' @keywords internal
SKILL_MIN_GAMES <- STAT_RATING_MIN_GAMES

#' @rdname STAT_RATING_CREDIBLE_LEVEL
#' @keywords internal
SKILL_CREDIBLE_LEVEL <- STAT_RATING_CREDIBLE_LEVEL


# Position-Based TOG Constants
# ----------------------------

#' Average time-on-ground fraction by lineup_position (from load_teams())
#' Computed from historical data (2021-2025). Used to estimate per-player TOG
#' when lineups are announced but games haven't started.
#' EMERG/SUB are currently filtered upstream but kept here for future use.
#' Unknown positions fall back to 0.75 with a warning.
#' Run data-raw/debug/compute_position_tog.R to regenerate from current data.
#' @keywords internal
POSITION_AVG_TOG <- c(
  FB = 0.91, BPL = 0.86, CHB = 0.86, BPR = 0.85,
  FF = 0.84, CHF = 0.82, HBFL = 0.82, HBFR = 0.82, WR = 0.82,
  FPL = 0.81, FPR = 0.81, WL = 0.81,
  C = 0.80, HFFL = 0.80, R = 0.80, RR = 0.80,
  HFFR = 0.79, RK = 0.79,
  INT = 0.73, SUB = 0.33, EMERG = 0.05
)
