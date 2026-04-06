# TORP Package Constants
# ======================
# Centralized constants for the torp package to avoid magic numbers

# AFL Field and Game Constants
# ----------------------------

#' Earliest season with data available in torpdata
#' @keywords internal
AFL_MIN_SEASON <- 2021L

#' Nominal playing time per AFL quarter in seconds (excludes stoppages)
#' @keywords internal
AFL_PLAY_QUARTER_SECONDS <- 1200L

#' Nominal total playing time per AFL match in seconds (4 x 1200)
#' @keywords internal
AFL_PLAY_GAME_SECONDS <- 4800L

#' Descriptions after which the clock stops (scoring resets, ball out of play)
#' @keywords internal
CLOCK_STOPPAGE_TRIGGERS <- c("Goal", "Behind", "Rushed",
                              "Out of Bounds", "Out On Full After Kick", "Out On Full")

#' Descriptions that always follow dead time (restarts after clock stoppage)
#' @keywords internal
CLOCK_RESTART_EVENTS <- c("Centre Bounce", "Ball Up Call",
                           "Kickin play on", "Kickin short", "Kickin long",
                           "OOF Kick In",
                           "Kick In Ineffective", "Kick In Clanger",
                           "Kick In Long To Adv.")

#' Maximum seconds between consecutive plays counted as playing time.
#' Gaps larger than this indicate unrecorded stoppages (injuries, reviews, etc.)
#' @keywords internal
CLOCK_DELTA_CAP <- 30L

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


# EPR (Expected Possession Rating) System Constants
# --------------------------------------------------

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
EPR_PRIOR_GAMES_HITOUT <- 3.0013

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
# -----------------------------------------------------

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

#' Clearances weight (hitout component, currently disabled pending re-optimization)
#' @keywords internal
EPV_CLEARANCES_WT <- 0.1094

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

#' Fallback mean combined score per game
#' @keywords internal
SIM_AVG_TOTAL <- 160

#' SD of combined score noise
#' @keywords internal
SIM_TOTAL_SD <- 28

#' Grand Final venue (standardised name via torp_replace_venues)
#' @keywords internal
SIM_GF_VENUE <- "M.C.G."

#' Scaling factor for familiarity-based Grand Final advantage (points).
#' Multiplied by familiarity difference (home - away) to produce a
#' venue-specific GF advantage. E.g., a Melbourne MCG tenant with 0.50
#' familiarity vs an interstate team with 0.05 gives a diff of 0.45,
#' yielding 0.45 * 6 = ~2.7 points advantage.
#' @keywords internal
SIM_GF_FAMILIARITY_SCALE <- 6

#' Victorian teams whose home finals are played at the MCG.
#' When a Victorian team is "home" in a final, the match is at the MCG
#' and venue familiarity applies instead of standard home advantage.
#' @keywords internal
SIM_VICTORIAN_TEAMS <- c(
  "Carlton", "Collingwood", "Essendon", "Geelong Cats",
  "Hawthorn", "Melbourne", "North Melbourne", "Richmond",
  "St Kilda", "Western Bulldogs"
)

#' Per-team per-round injury/disruption noise SD (points)
#' Represents week-to-week variation in effective team strength due to
#' player availability, minor injuries, and roster changes.
#' @keywords internal
SIM_INJURY_SD <- 3

#' Mean reversion rate per round
#' Fraction of the gap between a team's rating and the league mean that is
#' closed each round. 0.01 = ~21% total reversion over 24 rounds.
#' @keywords internal
SIM_MEAN_REVERSION <- 0.01

#' Rating shift fraction per round
#' After each simulated game, team ratings are shifted by this fraction of
#' (actual_result - expected_result). Controls how much in-season results
#' update the "hot" rating used for subsequent rounds.
#' @keywords internal
SIM_RATING_SHIFT <- 0.1

#' TBC/unknown injuries: assume back in ~3 rounds
#' @keywords internal
SIM_INJURY_TBC_BUFFER <- 3L

#' "Mid-season" estimated return maps to round 12
#' @keywords internal
SIM_INJURY_SEASON_MID <- 12L

#' "Late season" estimated return maps to round 18
#' @keywords internal
SIM_INJURY_SEASON_LATE <- 18L

#' "Second half of YYYY" estimated return maps to round 13
#' @keywords internal
SIM_INJURY_SECOND_HALF <- 13L

#' Reduced injury noise SD when known injuries are excluded (points)
#' When specific injured players are removed from team ratings, the remaining
#' per-round noise can be smaller since a major source of variation is gone.
#' @keywords internal
SIM_INJURY_SD_KNOWN <- 2

#' Injury discount for teams with known injuries excluded
#' Lighter than SIM_INJURY_DISCOUNT (0.95) because major absences are already
#' reflected by removing those players from the rating sum.
#' @keywords internal
INJURY_KNOWN_DISCOUNT <- 0.99

#' Floor for injury discount scaling
#' Discount drops 0.01 per week from INJURY_KNOWN_DISCOUNT, floored here.
#' @keywords internal
INJURY_DISCOUNT_FLOOR <- 0.90

#' Default number of simulations
#' @keywords internal
SIM_DEFAULT_N <- 1000

#' Minimum combined score floor for simulated matches (points).
#' Prevents unrealistically low totals from extreme rnorm draws.
#' @keywords internal
SIM_MIN_TOTAL <- 40

# Match-Level Simulation Constants
# ---------------------------------

#' Dirichlet concentration parameters for quarter scoring proportions.
#' Derived from empirical AFL quarter-by-quarter scoring distributions:
#' Q1 ~22%, Q2 ~27%, Q3 ~25%, Q4 ~26%.
#' @keywords internal
MATCH_SIM_QUARTER_ALPHA <- c(22, 27, 25, 26)

#' Default number of Monte Carlo simulations for match simulation
#' @keywords internal
MATCH_SIM_DEFAULT_N <- 10000L

#' Average shots per team per quarter (empirical from 2021-2025 PBP)
#' @keywords internal
MATCH_SIM_AVG_SHOTS_PER_QTR <- 7.5

#' Average goal conversion rate (goals / shots, empirical 2021-2025)
#' @keywords internal
MATCH_SIM_AVG_CONV_RATE <- 0.52


# Opponent Adjustment Constants
# ------------------------------

#' Decay rate (per day) for opponent defensive profile recency weighting.
#' ~231-day half-life, slightly faster than stat rating decay.
#' @keywords internal
OPP_ADJ_LAMBDA_DECAY <- 0.003

#' Floor and ceiling for opponent adjustment factor.
#' Prevents extreme adjustments from small samples.
#' @keywords internal
OPP_ADJ_FACTOR_CAP <- c(0.7, 1.4)

#' Decay rate (per day) for EPV opponent defensive profile weighting.
#' ~231-day half-life, matching the stat rating opponent decay.
#' @keywords internal
EPV_OPP_LAMBDA_DECAY <- 0.003

#' Pseudo-games at league average for EPV opponent profile shrinkage.
#' Pulls early-season teams toward league average.
#' @keywords internal
EPV_OPP_PRIOR_GAMES <- 5

#' Regular season rounds by year (excludes finals)
#' 2022 and earlier: 23 rounds; 2023 onwards: 24 rounds
#' @keywords internal
AFL_REGULAR_SEASON_ROUNDS <- c(
  "2021" = 23L,
  "2022" = 23L,
  "2023" = 24L,
  "2024" = 24L,
  "2025" = 24L,
  "2026" = 24L
)

#' Top N players per team for rating aggregation
#' @keywords internal
SIM_TOP_N_PLAYERS <- 21

#' Injury discount applied to team rating sums
#' Accounts for typical player availability (injuries, rest, rotation).
#' @keywords internal
SIM_INJURY_DISCOUNT <- 0.95


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


# Stat Rating Estimation Constants
# ---------------------------------

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


# Data Validation Constants
# -------------------------

#' High missing data threshold (proportion)
#' @keywords internal
VALIDATION_HIGH_MISSING_THRESHOLD <- 0.5

#' Minimum observations for calibration bins
#' @keywords internal
VALIDATION_MIN_CALIBRATION_BIN_SIZE <- 10


# EPV Model Constants
# -------------------

#' Descriptions relevant for EPV modeling (the 27-description whitelist)
#' Used in clean_model_data_epv_dt() and filter_relevant_descriptions()
#' @keywords internal
EPV_RELEVANT_DESCRIPTIONS <- c(
  "Ball Up Call", "Bounce", "Centre Bounce", "Contested Knock On", "Contested Mark",
  "Free Advantage", "Free For", "Free For: Before the Bounce", "Free For: In Possession",
  "Free For: Off The Ball", "Gather", "Gather From Hitout", "Gather from Opposition",
  "Ground Kick", "Handball", "Handball Received", "Hard Ball Get", "Hard Ball Get Crumb",
  "Kick", "Knock On", "Loose Ball Get", "Loose Ball Get Crumb", "Mark On Lead",
  "Out of Bounds", "Out On Full After Kick", "Ruck Hard Ball Get", "Uncontested Mark"
)


# Coordinate Cleaning Constants
# ------------------------------

#' Maximum reasonable Euclidean distance between consecutive PBP coordinates (metres).
#' Jumps exceeding this in pitch-relative space are smoothed via neighbor interpolation.
#' An AFL field is ~165m long and ~135m wide; 100m covers most realistic play distances.
#' @keywords internal
COORD_JUMP_THRESHOLD <- 100

#' Maximum distance (metres) after sign-flipping for a row to be considered
#' a sign-flip error. If negating a row's coordinates puts it within this
#' distance of the predecessor, the coordinates are likely in the wrong frame.
#' Set to 70m to cover the longest realistic kick distances (~65m displacement).
#' Safe because the 100m jump threshold already filters out all legitimate plays.
#' @keywords internal
COORD_FLIP_TOLERANCE <- 70


# Contest Extraction Constants
# ----------------------------
# Based on diagnostic analysis of raw CHAINS data (not cleaned PBP).
# Chains data has 87 unique descriptions including Spoil, Contest Target,
# Tackle, etc. that get stripped by clean_pbp(). Contests are identified by
# matching x,y coordinates on consecutive rows from opposing teams.

#' Chains descriptions for the contest target / kick result side
#' These appear in the row BEFORE the contest outcome (Spoil, Mark, etc.)
#' at the same x,y coordinates.
#' @keywords internal
CHAINS_CONTEST_TARGET_DESCS <- c("Contest Target", "Kick Inside 50 Result")

#' Chains descriptions indicating the opponent won the mark
#' @keywords internal
CHAINS_MARK_WIN_DESCS <- c("Contested Mark", "Uncontested Mark", "Mark On Lead")

#' Descriptions for ground ball contests (present in both chains and PBP)
#' Adjacent ground ball rows from opposing teams at same x,y indicate a contest
#' @keywords internal
CONTEST_GROUND_BALL_DESCS <- c(
  "Hard Ball Get", "Loose Ball Get",
  "Hard Ball Get Crumb", "Loose Ball Get Crumb",
  "Ruck Hard Ball Get"
)


# Position-Based TOG Constants
# -----------------------------

#' Average time-on-ground fraction by field position (position.x from load_teams())
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


# Match Model Constants
# ----------------------

#' Phase groupings for match model position columns
#' Maps field position (position.x) to broad phase categories
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

#' Listed position map for match models (position.y from load_teams())
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


# AFL Structural Constants
# -------------------------

#' Number of on-field players per team in a standard AFL match
#' @keywords internal
AFL_TEAM_SIZE <- 18L

#' Default days rest when rest data is unavailable
#' @keywords internal
MATCH_DEFAULT_REST_DAYS <- 21

#' Default TOG fraction for unknown field positions
#' @keywords internal
POSITION_AVG_TOG_DEFAULT <- 0.75

#' Venues with a closed roof (weather features set to neutral)
#' @keywords internal
AFL_ROOF_VENUES <- c("Docklands")

#' Default timezone for AFL matches
#' @keywords internal
AFL_DEFAULT_TIMEZONE <- "Australia/Melbourne"

#' Maximum round number fallback when AFL_REGULAR_SEASON_ROUNDS lookup fails
#' @keywords internal
AFL_MAX_REGULAR_ROUNDS <- 24L


# API & External Service Constants
# ---------------------------------

#' AFL public API base URL (v2)
#' @keywords internal
AFL_API_BASE_URL <- "https://aflapi.afl.com.au/afl/v2/"

#' AFL CFS API base URL (fixtures, results, players)
#' @keywords internal
AFL_CFS_API_BASE_URL <- "https://api.afl.com.au/cfs/afl/"

#' AFL chain/play data API base URL
#' @keywords internal
AFL_SAPI_BASE_URL <- "https://sapi.afl.com.au/afl/"

#' Rate limit delay in seconds for Open-Meteo API calls
#' @keywords internal
OPEN_METEO_RATE_LIMIT_SECONDS <- 0.3

#' Default disk cache max age in days
#' @keywords internal
DISK_CACHE_DEFAULT_AGE_DAYS <- 7


# Team Name Constants
# --------------------

#' Canonical AFL team lookup table
#'
#' Data frame with 18 rows mapping canonical short name, full name, and
#' AFL API abbreviation for each current AFL team.
#' @export
AFL_TEAMS <- data.frame(
  name = c(
    "Adelaide Crows", "Brisbane Lions", "Carlton Blues", "Collingwood Magpies",
    "Essendon Bombers", "Fremantle Dockers", "Geelong Cats", "Gold Coast Suns",
    "GWS Giants", "Hawthorn Hawks", "Melbourne Demons", "North Melbourne Kangaroos",
    "Port Adelaide Power", "Richmond Tigers", "St Kilda Saints", "Sydney Swans",
    "West Coast Eagles", "Western Bulldogs"
  ),
  full = c(
    "Adelaide Crows", "Brisbane Lions", "Carlton Blues", "Collingwood Magpies",
    "Essendon Bombers", "Fremantle Dockers", "Geelong Cats", "Gold Coast Suns",
    "GWS Giants", "Hawthorn Hawks", "Melbourne Demons", "North Melbourne Kangaroos",
    "Port Adelaide Power", "Richmond Tigers", "St Kilda Saints", "Sydney Swans",
    "West Coast Eagles", "Western Bulldogs"
  ),
  abbr = c(
    "ADEL", "BL", "CARL", "COLL", "ESS",
    "FRE", "GEEL", "GCFC", "GWS", "HAW",
    "MELB", "NMFC", "PA", "RICH",
    "STK", "SYD", "WCE", "WB"
  ),
  stringsAsFactors = FALSE
)

#' Named vector mapping team name variants to canonical names
#'
#' Maps abbreviations, full names, nicknames, Indigenous round names,
#' and other variants to the canonical short name used in `AFL_TEAMS$name`.
#' @export
AFL_TEAM_ALIASES <- c(

  # --- Abbreviations (AFL API + common) ---
  "ADEL"  = "Adelaide Crows",
  "BL"    = "Brisbane Lions",
  "CARL"  = "Carlton Blues",
  "COLL"  = "Collingwood Magpies",
  "ESS"   = "Essendon Bombers",
  "FRE"   = "Fremantle Dockers",
  "GEEL"  = "Geelong Cats",
  "GCFC"  = "Gold Coast Suns",
  "GWS"   = "GWS Giants",
  "HAW"   = "Hawthorn Hawks",
  "MELB"  = "Melbourne Demons",
  "NMFC"  = "North Melbourne Kangaroos",
  "NM"    = "North Melbourne Kangaroos",
  "PA"    = "Port Adelaide Power",
  "PORT"  = "Port Adelaide Power",
  "RICH"  = "Richmond Tigers",
  "STK"   = "St Kilda Saints",
  "SYD"   = "Sydney Swans",
  "WCE"   = "West Coast Eagles",
  "WB"    = "Western Bulldogs",

  # --- Full names (identity mappings) ---
  "Adelaide Crows"             = "Adelaide Crows",
  "Brisbane Lions"             = "Brisbane Lions",
  "Brisbane Bears"             = "Brisbane Lions",
  "Carlton Blues"              = "Carlton Blues",
  "Collingwood Magpies"        = "Collingwood Magpies",
  "Essendon Bombers"           = "Essendon Bombers",
  "Fremantle Dockers"          = "Fremantle Dockers",
  "Geelong Cats"               = "Geelong Cats",
  "Gold Coast Suns"            = "Gold Coast Suns",
  "GWS Giants"                 = "GWS Giants",
  "Greater Western Sydney"     = "GWS Giants",
  "Greater Western Sydney Giants" = "GWS Giants",
  "GW Sydney Giants"           = "GWS Giants",
  "Hawthorn Hawks"             = "Hawthorn Hawks",
  "Melbourne Demons"           = "Melbourne Demons",
  "North Melbourne Kangaroos"  = "North Melbourne Kangaroos",
  "Port Adelaide Power"        = "Port Adelaide Power",
  "Richmond Tigers"            = "Richmond Tigers",

  "St Kilda Saints"            = "St Kilda Saints",
  "Sydney Swans"               = "Sydney Swans",
  "South Melbourne"            = "Sydney Swans",
  "South Melbourne Swans"      = "Sydney Swans",
  "West Coast Eagles"          = "West Coast Eagles",
  "Western Bulldogs"           = "Western Bulldogs",
  "Footscray"                  = "Western Bulldogs",
  "Footscray Bulldogs"         = "Western Bulldogs",

  # --- Old short names (legacy TORP model output) ---
  "Adelaide"       = "Adelaide Crows",
  "Carlton"        = "Carlton Blues",
  "Collingwood"    = "Collingwood Magpies",
  "Essendon"       = "Essendon Bombers",
  "Fremantle"      = "Fremantle Dockers",
  "Geelong"        = "Geelong Cats",
  "Gold Coast"     = "Gold Coast Suns",
  "Hawthorn"       = "Hawthorn Hawks",
  "Melbourne"      = "Melbourne Demons",
  "North Melbourne" = "North Melbourne Kangaroos",
  "Port Adelaide"  = "Port Adelaide Power",
  "Richmond"       = "Richmond Tigers",
  "St Kilda"       = "St Kilda Saints",
  "Sydney"         = "Sydney Swans",
  "West Coast"     = "West Coast Eagles",

  # --- Nicknames ---
  "Crows"     = "Adelaide Crows",
  "Lions"     = "Brisbane Lions",
  "Bears"     = "Brisbane Lions",

  "Blues"     = "Carlton Blues",
  "Magpies"   = "Collingwood Magpies",
  "Pies"      = "Collingwood Magpies",
  "Bombers"   = "Essendon Bombers",
  "Dockers"   = "Fremantle Dockers",
  "Cats"      = "Geelong Cats",
  "Suns"      = "Gold Coast Suns",
  "SUNS"      = "Gold Coast Suns",
  "Giants"    = "GWS Giants",
  "GIANTS"    = "GWS Giants",
  "Hawks"     = "Hawthorn Hawks",
  "Demons"    = "Melbourne Demons",
  "Kangaroos" = "North Melbourne Kangaroos",
  "Roos"      = "North Melbourne Kangaroos",
  "Power"     = "Port Adelaide Power",
  "Tigers"    = "Richmond Tigers",
  "Saints"    = "St Kilda Saints",
  "Swans"     = "Sydney Swans",
  "Eagles"    = "West Coast Eagles",
  "Bulldogs"  = "Western Bulldogs",

  # --- Indigenous round names ---
  "Kuwarna"          = "Adelaide Crows",
  "Narrm"            = "Melbourne Demons",
  "Walyalup"         = "Fremantle Dockers",
  "Yartapuulti"      = "Port Adelaide Power",
  "Euro-Yroke"       = "St Kilda Saints",
  "Waalitj Marawar"  = "West Coast Eagles",
  "Wallitj Marawar"  = "West Coast Eagles",

  # --- ALL CAPS mascot variants (AFL API team names) ---
  "Adelaide CROWS"             = "Adelaide Crows",
  "Brisbane LIONS"             = "Brisbane Lions",
  "Carlton BLUES"              = "Carlton Blues",
  "Collingwood MAGPIES"        = "Collingwood Magpies",
  "Essendon BOMBERS"           = "Essendon Bombers",
  "Fremantle DOCKERS"          = "Fremantle Dockers",
  "Geelong CATS"               = "Geelong Cats",
  "Gold Coast SUNS"            = "Gold Coast Suns",
  "GWS GIANTS"                 = "GWS Giants",
  "Hawthorn HAWKS"             = "Hawthorn Hawks",
  "Melbourne DEMONS"           = "Melbourne Demons",
  "North Melbourne KANGAROOS"  = "North Melbourne Kangaroos",
  "Port Adelaide POWER"        = "Port Adelaide Power",
  "Richmond TIGERS"            = "Richmond Tigers",
  "St Kilda SAINTS"            = "St Kilda Saints",
  "Sydney SWANS"               = "Sydney Swans",
  "West Coast EAGLES"          = "West Coast Eagles",
  "Western BULLDOGS"           = "Western Bulldogs"
)

#' Primary team colours (hex) for AFL teams
#'
#' Named character vector mapping canonical team names to primary brand colour.
#' @export
AFL_TEAM_COLORS <- c(
  "Adelaide Crows"            = "#002B5C",
  "Brisbane Lions"            = "#A30046",
  "Carlton Blues"              = "#002B5C",

  "Collingwood Magpies"       = "#000000",
  "Essendon Bombers"          = "#CC2031",

  "Fremantle Dockers"         = "#2A0D45",
  "Geelong Cats"              = "#001F3D",
  "Gold Coast Suns"           = "#D4A843",
  "GWS Giants"                = "#F15A22",
  "Hawthorn Hawks"            = "#4D2004",
  "Melbourne Demons"          = "#CC2031",
  "North Melbourne Kangaroos" = "#003D8E",
  "Port Adelaide Power"       = "#008AAB",
  "Richmond Tigers"           = "#FED102",
  "St Kilda Saints"           = "#ED1C24",
  "Sydney Swans"              = "#ED171F",

  "West Coast Eagles"         = "#002B5C",
  "Western Bulldogs"          = "#014896"
)

#' Secondary team colours (hex) for AFL teams
#'
#' Named character vector mapping canonical team names to secondary brand colour.
#' Useful for contrast in two-tone plots (e.g., home vs away fills).
#' @export
AFL_TEAM_COLORS2 <- c(
  "Adelaide Crows"            = "#E21937",
  "Brisbane Lions"            = "#FFB81C",
  "Carlton Blues"              = "#FFFFFF",
  "Collingwood Magpies"       = "#FFFFFF",
  "Essendon Bombers"          = "#000000",
  "Fremantle Dockers"         = "#FFFFFF",
  "Geelong Cats"              = "#FFFFFF",
  "Gold Coast Suns"           = "#E21937",
  "GWS Giants"                = "#363636",
  "Hawthorn Hawks"            = "#FBBF15",
  "Melbourne Demons"          = "#002B5C",
  "North Melbourne Kangaroos" = "#FFFFFF",
  "Port Adelaide Power"       = "#000000",
  "Richmond Tigers"           = "#000000",
  "St Kilda Saints"           = "#000000",
  "Sydney Swans"              = "#FFFFFF",
  "West Coast Eagles"         = "#F2A900",
  "Western Bulldogs"          = "#E21937"
)
