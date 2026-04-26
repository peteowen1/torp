# Simulation Constants
# ====================
# Constants for season Monte Carlo simulation, single-match simulation,
# injury modelling, and opponent-quality adjustment of stat ratings.

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
#' Set to match `SIM_INJURY_SD` because scraped injury lists only capture
#' officially-listed absences — form slumps, minor niggles, and game-day
#' late-outs still contribute meaningful week-to-week jitter.
#' @keywords internal
SIM_INJURY_SD_KNOWN <- 3

#' Multiplier applied to team-quality residual SE before per-sim sampling.
#' Widens season-long team uncertainty drawn from the xscore_diff GAM random
#' effects. Values > 1 make the ladder and finals distribution less confident
#' (e.g. fewer 80%+ Premier probabilities, wider Top-N bands). Calibrated
#' empirically — the raw GAM SE under-states true team uncertainty because
#' random effects are shrunk toward the league mean.
#' @keywords internal
SIM_RESIDUAL_SE_MULT <- 1.5

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

#' Top N players per team for rating aggregation
#' @keywords internal
SIM_TOP_N_PLAYERS <- 21

#' Injury discount applied to team rating sums
#' Accounts for typical player availability (injuries, rest, rotation).
#' @keywords internal
SIM_INJURY_DISCOUNT <- 0.95


# Match-Level Simulation Constants
# --------------------------------

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
# -----------------------------

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
