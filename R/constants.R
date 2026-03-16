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
EPR_DECAY_RECV <- 271

#' Decay factor (in days) for disposal component weighting
#' @keywords internal
EPR_DECAY_DISP <- 530

#' Decay factor (in days) for spoil component weighting
#' @keywords internal
EPR_DECAY_SPOIL <- 536

#' Decay factor (in days) for hitout component weighting
#' @keywords internal
EPR_DECAY_HITOUT <- 700

#' Default decay factor (in days) — legacy alias for backwards compatibility
#' @keywords internal
EPR_DECAY_DEFAULT_DAYS <- EPR_DECAY_RECV

#' Loading factor for TORP calculations
#' @keywords internal
EPR_LOADING_DEFAULT <- 1.0000

#' Prior games constant for receiving ratings
#' @keywords internal
EPR_PRIOR_GAMES_RECV <- 4.0122

#' Prior games constant for disposal ratings
#' @keywords internal
EPR_PRIOR_GAMES_DISP <- 3.0000

#' Prior games constant for spoil ratings
#' @keywords internal
EPR_PRIOR_GAMES_SPOIL <- 3.0000

#' Prior games constant for hitout ratings
#' @keywords internal
EPR_PRIOR_GAMES_HITOUT <- 15.0000

#' Prior rate for receiving component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_RECV <- -2.0950

#' Prior rate for disposal component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_DISP <- -0.0000

#' Prior rate for spoil component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_SPOIL <- -3.0701

#' Prior rate for hitout component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_HITOUT <- -4.0000

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

#' Disposal EPV offset when defending (pos_team == -1)
#' @keywords internal
EPV_DISP_NEG_OFFSET <- 0.0000

#' Disposal EPV offset when possessing (pos_team == 1)
#' @keywords internal
EPV_DISP_POS_OFFSET <- 0.0000

#' Disposal scaling factor
#' @keywords internal
EPV_DISP_SCALE <- 0.5000

#' Bounce weight per bounce (disp component)
#' @keywords internal
EPV_BOUNCE_WT <- -0.5865

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

#' Spoil weight per spoil
#' @keywords internal
EPV_SPOIL_WT <- 0.0928

#' Tackle weight per tackle
#' @keywords internal
EPV_TACKLE_WT <- 0.2309

#' Pressure act weight
#' @keywords internal
EPV_PRESSURE_WT <- -0.0106

#' Defensive half pressure act weight (spoil component)
#' @keywords internal
EPV_DEF_PRESSURE_WT <- -0.2083

#' Hitout weight per hitout
#' @keywords internal
EPV_HITOUT_WT <- 0.0309

#' Hitout to advantage weight
#' @keywords internal
EPV_HITOUT_ADV_WT <- 0.1221

#' Ruck contest weight (hitout component)
#' @keywords internal
EPV_RUCK_CONTEST_WT <- 0.0183

#' Contested possessions weight (recv component)
#' @keywords internal
EPV_CONTESTED_POSS_WT <- 0.1188

#' Contested marks weight (recv component)
#' @keywords internal
EPV_CONTESTED_MARKS_WT <- 0.0715

#' Ground ball gets weight (recv component)
#' @keywords internal
EPV_GROUND_BALL_GETS_WT <- 0.1596

#' Marks inside 50 weight (recv component)
#' @keywords internal
EPV_MARKS_INSIDE50_WT <- 0.1875

#' Inside 50s weight (disp component)
#' @keywords internal
EPV_INSIDE50S_WT <- 0.1829

#' Clangers weight (disp component)
#' @keywords internal
EPV_CLANGERS_WT <- -0.0744

#' Score involvements weight (disp component)
#' @keywords internal
EPV_SCORE_INVOLVEMENTS_WT <- 0.2389

#' Intercepts weight (spoil component)
#' @keywords internal
EPV_INTERCEPTS_WT <- 0.0023

#' One percenters weight (spoil component)
#' @keywords internal
EPV_ONE_PERCENTERS_WT <- 0.1653

#' Rebound 50s weight (spoil component)
#' @keywords internal
EPV_REBOUND50S_WT <- -0.1682

#' Frees against weight (spoil component)
#' @keywords internal
EPV_FREES_AGAINST_WT <- 0.0564

#' Clearances weight (hitout component)
#' @keywords internal
EPV_CLEARANCES_WT <- 0.0787

#' Frees for weight (recv component)
#' @keywords internal
EPV_FREES_FOR_WT <- -0.0018

#' Goals weight (disp component)
#' @keywords internal
EPV_GOALS_WT <- 0.2159

#' Behinds weight (disp component)
#' @keywords internal
EPV_BEHINDS_WT <- 0.8992

#' Total marks weight (recv component)
#' @keywords internal
EPV_MARKS_WT <- -0.0241

#' Uncontested possessions weight (recv component)
#' @keywords internal
EPV_UNCONTESTED_POSS_WT <- 0.0271

#' Shots at goal weight (disp component)
#' @keywords internal
EPV_SHOTS_AT_GOAL_WT <- 0.3041

#' Kicks weight (disp component)
#' @keywords internal
EPV_KICKS_WT <- 0.0195

#' Handballs weight (disp component)
#' @keywords internal
EPV_HANDBALLS_WT <- 0.0659

#' Metres gained weight (disp component)
#' @keywords internal
EPV_METRES_GAINED_WT <- 0.0006

#' Turnovers weight (disp component)
#' @keywords internal
EPV_TURNOVERS_WT <- -0.0911

#' Goal assists weight (disp component)
#' @keywords internal
EPV_GOAL_ASSISTS_WT <- -0.0440

#' L2 (ridge) regularization lambda for count-based stat weights
#' @keywords internal
STAT_WEIGHT_LAMBDA <- 0.5

#' Position-group quantile adjustment for receiving component
#' @keywords internal
EPV_POS_ADJ_QUANTILE_RECV <- 0.2942

#' Position-group quantile adjustment for disposal component
#' @keywords internal
EPV_POS_ADJ_QUANTILE_DISP <- 0.3344

#' Position-group quantile adjustment for spoil component
#' @keywords internal
EPV_POS_ADJ_QUANTILE_SPOIL <- 0.3500

#' Position-group quantile adjustment for hitout component
#' @keywords internal
EPV_POS_ADJ_QUANTILE_HITOUT <- 0.3500

#' Position-group quantile adjustment — legacy alias for backwards compatibility
#' @keywords internal
EPV_POS_ADJ_QUANTILE <- EPV_POS_ADJ_QUANTILE_RECV


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

#' Reduced injury noise SD when known injuries are excluded (points)
#' When specific injured players are removed from team ratings, the remaining
#' per-round noise can be smaller since a major source of variation is gone.
#' @keywords internal
SIM_INJURY_SD_KNOWN <- 2

#' Injury discount for teams with known injuries excluded
#' Lighter than SIM_INJURY_DISCOUNT (0.95) because major absences are already
#' reflected by removing those players from the rating sum.
#' @keywords internal
INJURY_KNOWN_DISCOUNT <- 0.98

#' Default number of simulations
#' @keywords internal
SIM_DEFAULT_N <- 1000

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


# Team Name Constants
# --------------------

#' Canonical AFL team lookup table
#'
#' Data frame with 18 rows mapping canonical short name, full name, and
#' AFL API abbreviation for each current AFL team.
#' @export
AFL_TEAMS <- data.frame(
  name = c(
    "Adelaide", "Brisbane Lions", "Carlton", "Collingwood", "Essendon",
    "Fremantle", "Geelong", "Gold Coast", "GWS", "Hawthorn",
    "Melbourne", "North Melbourne", "Port Adelaide", "Richmond",
    "St Kilda", "Sydney", "West Coast", "Western Bulldogs"
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
  "ADEL"  = "Adelaide",
  "BL"    = "Brisbane Lions",
  "CARL"  = "Carlton",
  "COLL"  = "Collingwood",
  "ESS"   = "Essendon",
  "FRE"   = "Fremantle",
  "GEEL"  = "Geelong",
  "GCFC"  = "Gold Coast",
  "GWS"   = "GWS",
  "HAW"   = "Hawthorn",
  "MELB"  = "Melbourne",
  "NMFC"  = "North Melbourne",
  "NM"    = "North Melbourne",
  "PA"    = "Port Adelaide",
  "PORT"  = "Port Adelaide",
  "RICH"  = "Richmond",
  "STK"   = "St Kilda",
  "SYD"   = "Sydney",
  "WCE"   = "West Coast",
  "WB"    = "Western Bulldogs",

  # --- Full names ---
  "Adelaide Crows"             = "Adelaide",
  "Brisbane Lions"             = "Brisbane Lions",
  "Brisbane Bears"             = "Brisbane Lions",
  "Carlton Blues"              = "Carlton",
  "Collingwood Magpies"        = "Collingwood",
  "Essendon Bombers"           = "Essendon",
  "Fremantle Dockers"          = "Fremantle",
  "Geelong Cats"               = "Geelong",
  "Gold Coast Suns"            = "Gold Coast",
  "GWS Giants"                 = "GWS",
  "Greater Western Sydney"     = "GWS",
  "Greater Western Sydney Giants" = "GWS",
  "GW Sydney Giants"           = "GWS",
  "Hawthorn Hawks"             = "Hawthorn",
  "Melbourne Demons"           = "Melbourne",
  "North Melbourne Kangaroos"  = "North Melbourne",
  "Port Adelaide Power"        = "Port Adelaide",
  "Richmond Tigers"            = "Richmond",

  "St Kilda Saints"            = "St Kilda",
  "Sydney Swans"               = "Sydney",
  "South Melbourne"            = "Sydney",
  "South Melbourne Swans"      = "Sydney",
  "West Coast Eagles"          = "West Coast",
  "Western Bulldogs"           = "Western Bulldogs",
  "Footscray"                  = "Western Bulldogs",
  "Footscray Bulldogs"         = "Western Bulldogs",

  # --- Nicknames ---
  "Crows"     = "Adelaide",
  "Lions"     = "Brisbane Lions",
  "Bears"     = "Brisbane Lions",

  "Blues"     = "Carlton",
  "Magpies"   = "Collingwood",
  "Pies"      = "Collingwood",
  "Bombers"   = "Essendon",
  "Dockers"   = "Fremantle",
  "Cats"      = "Geelong",
  "Suns"      = "Gold Coast",
  "SUNS"      = "Gold Coast",
  "Giants"    = "GWS",
  "GIANTS"    = "GWS",
  "Hawks"     = "Hawthorn",
  "Demons"    = "Melbourne",
  "Kangaroos" = "North Melbourne",
  "Roos"      = "North Melbourne",
  "Power"     = "Port Adelaide",
  "Tigers"    = "Richmond",
  "Saints"    = "St Kilda",
  "Swans"     = "Sydney",
  "Eagles"    = "West Coast",
  "Bulldogs"  = "Western Bulldogs",

  # --- Indigenous round names ---
  "Kuwarna"          = "Adelaide",
  "Narrm"            = "Melbourne",
  "Walyalup"         = "Fremantle",
  "Yartapuulti"      = "Port Adelaide",
  "Euro-Yroke"       = "St Kilda",
  "Waalitj Marawar"  = "West Coast",
  "Wallitj Marawar"  = "West Coast",

  # --- ALL CAPS mascot variants (AFL API team names) ---
  "Adelaide CROWS"             = "Adelaide",
  "Brisbane LIONS"             = "Brisbane Lions",
  "Carlton BLUES"              = "Carlton",
  "Collingwood MAGPIES"        = "Collingwood",
  "Essendon BOMBERS"           = "Essendon",
  "Fremantle DOCKERS"          = "Fremantle",
  "Geelong CATS"               = "Geelong",
  "Gold Coast SUNS"            = "Gold Coast",
  "GWS GIANTS"                 = "GWS",
  "Hawthorn HAWKS"             = "Hawthorn",
  "Melbourne DEMONS"           = "Melbourne",
  "North Melbourne KANGAROOS"  = "North Melbourne",
  "Port Adelaide POWER"        = "Port Adelaide",
  "Richmond TIGERS"            = "Richmond",
  "St Kilda SAINTS"            = "St Kilda",
  "Sydney SWANS"               = "Sydney",
  "West Coast EAGLES"          = "West Coast",
  "Western BULLDOGS"           = "Western Bulldogs"
)
