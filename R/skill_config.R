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
      "clangers", "turnovers"
    ),
    source_col = c(
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
      "clangers", "turnovers"
    ),
    category = c(
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
      "negative", "negative"
    ),
    type = "rate",
    success_col = NA_character_,
    attempts_col = NA_character_,
    stringsAsFactors = FALSE
  )

  efficiency_stats <- data.frame(
    stat_name = c(
      "disposal_efficiency", "goal_accuracy",
      "contested_poss_rate", "hitout_win_pct"
    ),
    source_col = NA_character_,
    category = c("disposal", "scoring", "possession", "ruck"),
    type = "efficiency",
    success_col = c(
      "disposal_efficiency_pct_x_disposals", "goals",
      "contested_possessions", "extended_stats_hitouts_to_advantage"
    ),
    attempts_col = c(
      "disposals", "shots_at_goal",
      "contested_possessions+uncontested_possessions", "hitouts"
    ),
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
#' Returns sensible defaults for the Bayesian skill estimation pipeline.
#' These can be overridden by optimized values from
#' \code{data-raw/06-skills/03_optimize_params.R}.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{lambda_rate}{Exponential decay rate for rate stats (per day).
#'       Default 0.0019 gives about 365-day half-life.}
#'     \item{lambda_efficiency}{Decay rate for efficiency stats.
#'       Default 0.0013 gives about 533-day half-life.}
#'     \item{prior_games}{Prior pseudo-games for Gamma-Poisson rate stats.}
#'     \item{prior_attempts}{Prior pseudo-attempts for Beta-Binomial efficiency stats.}
#'     \item{min_games}{Minimum weighted games to appear in output.}
#'     \item{credible_level}{Width of credible interval (e.g. 0.80 for 80 pct).}
#'   }
#' @export
default_skill_params <- function() {
  list(
    lambda_rate       = SKILL_LAMBDA_RATE_DEFAULT,
    lambda_efficiency = SKILL_LAMBDA_EFFICIENCY_DEFAULT,
    prior_games       = SKILL_PRIOR_GAMES_DEFAULT,
    prior_attempts    = SKILL_PRIOR_ATTEMPTS_DEFAULT,
    min_games         = SKILL_MIN_GAMES,
    credible_level    = SKILL_CREDIBLE_LEVEL
  )
}
