# Player Skill Estimation Configuration
# ======================================
# Stat definitions, position mapping, and default hyperparameters
# for Bayesian skill estimation pipeline.


#' Stat definitions for player skill estimation
#'
#' Returns a data.frame describing every stat to estimate. Each row specifies
#' how to extract the raw value from player data and whether it's a rate stat
#' (Gamma-Poisson, scaled by TOG) or an efficiency stat (Beta-Binomial).
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{stat_name}{Short name used in output columns}
#'     \item{type}{"rate" or "efficiency"}
#'     \item{source_col}{Column name in player_game_data / player_stats for the raw count (rate stats)}
#'     \item{category}{Grouping for display purposes}
#'     \item{success_col}{Column or expression for successes (efficiency stats only)}
#'     \item{attempts_col}{Column or expression for attempts (efficiency stats only)}
#'   }
#' @export
skill_stat_definitions <- function() {
  rate_stats <- data.frame(
    stat_name = c(
      # --- Existing stats ---
      "goals", "behinds", "shots_at_goal", "score_involvements", "goal_assists",
      "kicks", "handballs", "disposals",
      "marks", "contested_possessions", "uncontested_possessions",
      "contested_marks", "ground_ball_gets",
      "clearances",
      "inside50s", "marks_inside50", "rebound50s", "metres_gained",
      "tackles", "spoils", "intercepts", "one_percenters",
      "pressure_acts",
      "hitouts", "hitouts_to_advantage",
      "frees_for", "frees_against",
      "clangers", "turnovers",
      # --- New stats ---
      "bounces", "def_half_pressure_acts",
      "centre_clearances", "stoppage_clearances",
      "effective_kicks", "effective_disposals",
      "intercept_marks", "f50_ground_ball_gets",
      "score_launches", "marks_on_lead",
      "tackles_inside50", "kickins",
      "centre_bounce_attendances", "ruck_contests",
      "contest_def_one_on_ones", "contest_off_one_on_ones",
      "contest_off_wins", "contest_def_losses",
      "dream_team_points", "rating_points"
    ),
    source_col = c(
      # --- Existing stats ---
      "goals", "behinds", "shots_at_goal", "score_involvements", "goal_assists",
      "kicks", "handballs", "disposals",
      "marks", "contested_possessions", "uncontested_possessions",
      "contested_marks", "extended_stats_ground_ball_gets",
      "clearances_total_clearances",
      "inside50s", "marks_inside50", "rebound50s", "metres_gained",
      "tackles", "extended_stats_spoils", "intercepts", "one_percenters",
      "extended_stats_pressure_acts",
      "hitouts", "extended_stats_hitouts_to_advantage",
      "frees_for", "frees_against",
      "clangers", "turnovers",
      # --- New stats ---
      "bounces", "extended_stats_def_half_pressure_acts",
      "clearances_centre_clearances", "clearances_stoppage_clearances",
      "extended_stats_effective_kicks", "extended_stats_effective_disposals",
      "extended_stats_intercept_marks", "extended_stats_f50ground_ball_gets",
      "extended_stats_score_launches", "extended_stats_marks_on_lead",
      "tackles_inside50", "extended_stats_kickins",
      "extended_stats_centre_bounce_attendances", "extended_stats_ruck_contests",
      "extended_stats_contest_def_one_on_ones", "extended_stats_contest_off_one_on_ones",
      "extended_stats_contest_off_wins", "extended_stats_contest_def_losses",
      "dream_team_points", "rating_points"
    ),
    category = c(
      # --- Existing stats ---
      "scoring", "scoring", "scoring", "scoring", "scoring",
      "disposal", "disposal", "disposal",
      "possession", "possession", "possession",
      "contested", "contested",
      "clearance",
      "territory", "territory", "territory", "territory",
      "defensive", "defensive", "defensive", "defensive",
      "pressure",
      "ruck", "ruck",
      "discipline", "discipline",
      "negative", "negative",
      # --- New stats ---
      "disposal", "pressure",
      "clearance", "clearance",
      "disposal", "disposal",
      "defensive", "contested",
      "territory", "possession",
      "defensive", "territory",
      "ruck", "ruck",
      "contested", "contested",
      "contested", "contested",
      "general", "general"
    ),
    type = "rate",
    success_col = NA_character_,
    attempts_col = NA_character_,
    tog_adjusted = c(
      # --- Existing stats (all TOG-adjusted) ---
      rep(TRUE, 29),
      # --- New stats ---
      TRUE, TRUE,           # bounces, def_half_pressure_acts
      TRUE, TRUE,           # centre/stoppage clearances
      TRUE, TRUE,           # effective kicks/disposals
      TRUE, TRUE,           # intercept_marks, f50_ground_ball_gets
      TRUE, TRUE,           # score_launches, marks_on_lead
      TRUE, TRUE,           # tackles_inside50, kickins
      TRUE, TRUE,           # centre_bounce_attendances, ruck_contests
      TRUE, TRUE,           # contest_def/off_one_on_ones
      TRUE, TRUE,           # contest_off_wins, contest_def_losses
      FALSE, FALSE          # dream_team_points, rating_points
    ),
    stringsAsFactors = FALSE
  )

  efficiency_stats <- data.frame(
    stat_name = c(
      "disposal_efficiency", "goal_accuracy",
      "contested_poss_rate", "hitout_win_pct",
      "kick_efficiency", "time_on_ground"
    ),
    source_col = NA_character_,
    category = c("disposal", "scoring", "possession", "ruck", "disposal", "general"),
    type = "efficiency",
    success_col = c(
      "disposal_efficiency_pct_x_disposals", "goals",
      "contested_possessions", "extended_stats_hitouts_to_advantage",
      "extended_stats_effective_kicks", "tog"
    ),
    attempts_col = c(
      "disposals", "shots_at_goal",
      "contested_possessions+uncontested_possessions", "hitouts",
      "kicks", "tog_denominator"
    ),
    tog_adjusted = NA,
    stringsAsFactors = FALSE
  )

  rbind(rate_stats, efficiency_stats)
}


#' Position group mapping for AFL skill estimation
#'
#' Maps AFL listed positions to 4 simplified groups used for computing
#' position-specific priors.
#'
#' @return A named list mapping group names to character vectors of
#'   AFL position strings.
#' @export
skill_position_map <- function() {
  list(
    DEF  = c("KEY_DEFENDER", "MEDIUM_DEFENDER"),
    MID  = c("MIDFIELDER", "MIDFIELDER_FORWARD"),
    FWD  = c("KEY_FORWARD", "MEDIUM_FORWARD"),
    RUCK = c("RUCK")
  )
}


#' Default hyperparameters for skill estimation
#'
#' Returns optimized defaults for the Bayesian skill estimation pipeline.
#' Per-category lambda and prior values come from
#' \code{data-raw/06-skills/02_optimize_params.R}. The global \code{lambda_rate}
#' and \code{prior_games} are used as fallbacks for any category not in
#' \code{category_params}.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{lambda_rate}{Fallback decay rate for rate stats (per day).}
#'     \item{lambda_efficiency}{Decay rate for efficiency stats (per day).}
#'     \item{prior_games}{Fallback prior pseudo-games for Gamma-Poisson rate stats.}
#'     \item{prior_attempts}{Prior pseudo-attempts for Beta-Binomial efficiency stats.}
#'     \item{min_games}{Minimum weighted games to appear in output.}
#'     \item{credible_level}{Width of credible interval (e.g. 0.80 for 80 pct).}
#'     \item{category_params}{Per-category lambda and prior_strength overrides.}
#'   }
#' @export
default_skill_params <- function() {
  list(
    lambda_rate       = SKILL_LAMBDA_RATE_DEFAULT,
    lambda_efficiency = SKILL_LAMBDA_EFFICIENCY_DEFAULT,
    prior_games       = SKILL_PRIOR_GAMES_DEFAULT,
    prior_attempts    = SKILL_PRIOR_ATTEMPTS_DEFAULT,
    min_games         = SKILL_MIN_GAMES,
    credible_level    = SKILL_CREDIBLE_LEVEL,
    stat_params       = .skill_stat_params()
  )
}

#' Optimized per-stat hyperparameters
#'
#' Baked-in results from \code{data-raw/06-skills/02_optimize_params.R}.
#' Rate stats optimized via TOG-weighted MSE, efficiency stats via
#' attempt-weighted log-loss. Each entry has \code{lambda} (decay per day)
#' and \code{prior_strength}.
#' @keywords internal
.skill_stat_params <- function() {
  list(
    # Rate stats (Gamma-Poisson, optimized via multi-start MSE)
    goals                   = list(lambda = 0.00307, prior_strength = 1.08),
    behinds                 = list(lambda = 0.00234, prior_strength = 2.26),
    shots_at_goal           = list(lambda = 0.00401, prior_strength = 0.58),
    score_involvements      = list(lambda = 0.00291, prior_strength = 1.23),
    goal_assists            = list(lambda = 0.00187, prior_strength = 5.02),
    kicks                   = list(lambda = 0.00580, prior_strength = 0.41),
    handballs               = list(lambda = 0.00499, prior_strength = 0.34),
    disposals               = list(lambda = 0.00617, prior_strength = 0.24),
    marks                   = list(lambda = 0.00327, prior_strength = 2.14),
    contested_possessions   = list(lambda = 0.00451, prior_strength = 0.60),
    uncontested_possessions = list(lambda = 0.00507, prior_strength = 0.44),
    contested_marks         = list(lambda = 0.00191, prior_strength = 1.70),
    ground_ball_gets        = list(lambda = 0.00316, prior_strength = 0.88),
    clearances              = list(lambda = 0.00663, prior_strength = 0.26),
    inside50s               = list(lambda = 0.00375, prior_strength = 1.00),
    marks_inside50          = list(lambda = 0.00317, prior_strength = 0.77),
    rebound50s              = list(lambda = 0.00692, prior_strength = 0.40),
    metres_gained           = list(lambda = 0.00555, prior_strength = 0.40),
    tackles                 = list(lambda = 0.00400, prior_strength = 0.95),
    spoils                  = list(lambda = 0.00387, prior_strength = 0.52),
    intercepts              = list(lambda = 0.00511, prior_strength = 0.57),
    one_percenters          = list(lambda = 0.00346, prior_strength = 0.72),
    pressure_acts           = list(lambda = 0.00610, prior_strength = 0.36),
    hitouts                 = list(lambda = 0.00804, prior_strength = 0.10),
    hitouts_to_advantage    = list(lambda = 0.00503, prior_strength = 0.10),
    frees_for               = list(lambda = 0.00158, prior_strength = 5.27),
    frees_against           = list(lambda = 0.00121, prior_strength = 5.24),
    clangers                = list(lambda = 0.00209, prior_strength = 3.25),
    turnovers               = list(lambda = 0.00298, prior_strength = 2.30),
    # Efficiency stats (Beta-Binomial, optimized via multi-start log-loss)
    disposal_efficiency     = list(lambda = 0.00235, prior_strength = 32.67),
    goal_accuracy           = list(lambda = 0.00010, prior_strength = 67.73),
    contested_poss_rate     = list(lambda = 0.00303, prior_strength = 20.00),
    hitout_win_pct          = list(lambda = 0.00135, prior_strength = 100.00)
  )
}
