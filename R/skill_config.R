# Player Stat Rating Estimation Configuration
# =============================================
# Stat definitions, position mapping, and default hyperparameters
# for Bayesian stat rating estimation pipeline.


#' Stat definitions for player stat rating estimation
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
#' @keywords internal
stat_rating_definitions <- function() {
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
    higher_is_better = c(
      # --- Existing stats ---
      TRUE, FALSE, TRUE, TRUE, TRUE,          # goals, behinds(miss), shots, SIs, assists
      TRUE, TRUE, TRUE,                        # kicks, handballs, disposals
      TRUE, TRUE, TRUE,                        # marks, CP, UP
      TRUE, TRUE,                              # contested marks, GBGs
      TRUE,                                    # clearances
      TRUE, TRUE, TRUE, TRUE,                  # I50s, MI50, rebound50s, metres_gained
      TRUE, TRUE, TRUE, TRUE,                  # tackles, spoils, intercepts, 1%ers
      TRUE,                                    # pressure_acts
      TRUE, TRUE,                              # hitouts, HTA
      TRUE, FALSE,                             # frees_for, frees_against
      FALSE, FALSE,                            # clangers, turnovers
      # --- New stats ---
      TRUE, TRUE,                              # bounces, def_half_pressure_acts
      TRUE, TRUE,                              # centre/stoppage clearances
      TRUE, TRUE,                              # effective kicks/disposals
      TRUE, TRUE,                              # intercept_marks, f50_GBGs
      TRUE, TRUE,                              # score_launches, marks_on_lead
      TRUE, TRUE,                              # tackles_inside50, kickins
      TRUE, TRUE,                              # CBA, ruck_contests
      TRUE, TRUE,                              # contest_def_one_on_ones, contest_off_one_on_ones
      TRUE, FALSE,                             # contest_off_wins, contest_def_losses
      TRUE, TRUE                               # DT points, rating points
    ),
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
    pos_adjusted = TRUE,
    stringsAsFactors = FALSE
  )

  efficiency_stats <- data.frame(
    stat_name = c(
      "disposal_efficiency", "goal_accuracy",
      "contested_poss_rate", "hitout_win_pct",
      "kick_efficiency", "cond_tog", "squad_selection"
    ),
    source_col = NA_character_,
    category = c("disposal", "scoring", "possession", "ruck", "disposal", "general", "general"),
    type = "efficiency",
    higher_is_better = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    success_col = c(
      "disposal_efficiency_pct_x_disposals", "goals",
      "contested_possessions", "hitouts_to_advantage",
      "effective_kicks", "tog", "played"
    ),
    attempts_col = c(
      "disposals", "shots_at_goal",
      "contested_possessions+uncontested_possessions", "hitouts",
      "kicks", "tog_denominator", "tog_denominator"
    ),
    tog_adjusted = NA,
    pos_adjusted = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    played_only = c(NA, NA, NA, NA, NA, TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  # Add played_only column to rate_stats so rbind works
  rate_stats$played_only <- NA

  rbind(rate_stats, efficiency_stats)
}


#' Position group mapping for AFL stat rating estimation
#'
#' Maps AFL listed positions to 6 position groups for computing
#' position-specific priors. MIDFIELDER_FORWARD is combined with
#' MEDIUM_FORWARD.
#'
#' @return A named list mapping group names to character vectors of
#'   AFL position strings.
#' @keywords internal
stat_rating_position_map <- function() {
  list(
    KEY_DEFENDER    = "KEY_DEFENDER",
    MEDIUM_DEFENDER = "MEDIUM_DEFENDER",
    MIDFIELDER      = "MIDFIELDER",
    MEDIUM_FORWARD  = c("MEDIUM_FORWARD", "MIDFIELDER_FORWARD"),
    KEY_FORWARD     = "KEY_FORWARD",
    RUCK            = "RUCK"
  )
}


#' Default hyperparameters for stat rating estimation
#'
#' Returns optimized defaults for the Bayesian stat rating estimation pipeline.
#' Per-category lambda and prior values come from
#' \code{data-raw/06-stat-ratings/02_optimize_skill_params.R}. The global \code{lambda_rate}
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
#' @keywords internal
default_stat_rating_params <- function() {
  list(
    lambda_rate       = STAT_RATING_LAMBDA_RATE_DEFAULT,
    lambda_efficiency = STAT_RATING_LAMBDA_EFFICIENCY_DEFAULT,
    prior_games       = STAT_RATING_PRIOR_GAMES_DEFAULT,
    prior_attempts    = STAT_RATING_PRIOR_ATTEMPTS_DEFAULT,
    min_games         = STAT_RATING_MIN_GAMES,
    credible_level    = STAT_RATING_CREDIBLE_LEVEL,
    stat_params       = .stat_rating_params()
  )
}

#' Optimized per-stat hyperparameters
#'
#' Baked-in results from \code{data-raw/06-stat-ratings/02_optimize_skill_params.R}.
#' Rate stats optimized via TOG-weighted MSE, efficiency stats via
#' attempt-weighted log-loss. Each entry has \code{lambda} (decay per day)
#' and \code{prior_strength}.
#' @keywords internal
.stat_rating_params <- function() {
  list(
    # Rate stats (Gamma-Poisson, optimized via multi-start MSE)
    goals                     = list(lambda = 0.0029151, prior_strength = 2.88780),
    behinds                   = list(lambda = 0.0020082, prior_strength = 6.28490),
    shots_at_goal             = list(lambda = 0.0037853, prior_strength = 1.53900),
    score_involvements        = list(lambda = 0.0027323, prior_strength = 2.48420),
    goal_assists              = list(lambda = 0.0018394, prior_strength = 10.89400),
    kicks                     = list(lambda = 0.0058879, prior_strength = 0.67203),
    handballs                 = list(lambda = 0.0045937, prior_strength = 0.74770),
    disposals                 = list(lambda = 0.0059614, prior_strength = 0.49403),
    marks                     = list(lambda = 0.0032172, prior_strength = 2.68830),
    contested_possessions     = list(lambda = 0.0040034, prior_strength = 0.97830),
    uncontested_possessions   = list(lambda = 0.0052543, prior_strength = 0.92417),
    contested_marks           = list(lambda = 0.0021273, prior_strength = 5.27100),
    ground_ball_gets          = list(lambda = 0.0027696, prior_strength = 1.86050),
    clearances                = list(lambda = 0.0059740, prior_strength = 0.49046),
    inside50s                 = list(lambda = 0.0036968, prior_strength = 1.92570),
    marks_inside50            = list(lambda = 0.0030758, prior_strength = 2.93300),
    rebound50s                = list(lambda = 0.0065586, prior_strength = 1.11530),
    metres_gained             = list(lambda = 0.0054050, prior_strength = 0.80680),
    tackles                   = list(lambda = 0.0035553, prior_strength = 1.49550),
    spoils                    = list(lambda = 0.0043568, prior_strength = 1.62190),
    intercepts                = list(lambda = 0.0054460, prior_strength = 1.75240),
    one_percenters            = list(lambda = 0.0038341, prior_strength = 2.14420),
    pressure_acts             = list(lambda = 0.0060850, prior_strength = 0.79341),
    hitouts                   = list(lambda = 0.0057511, prior_strength = 0.31052),
    hitouts_to_advantage      = list(lambda = 0.0026451, prior_strength = 1.15480),
    frees_for                 = list(lambda = 0.0016070, prior_strength = 7.54370),
    frees_against             = list(lambda = 0.0008640, prior_strength = 7.22780),
    clangers                  = list(lambda = 0.0016913, prior_strength = 5.11760),
    turnovers                 = list(lambda = 0.0026643, prior_strength = 4.43670),
    bounces                   = list(lambda = 0.0044189, prior_strength = 1.70870),
    def_half_pressure_acts    = list(lambda = 0.0045918, prior_strength = 1.52960),
    centre_clearances         = list(lambda = 0.0052948, prior_strength = 0.80215),
    stoppage_clearances       = list(lambda = 0.0041970, prior_strength = 1.19720),
    effective_kicks           = list(lambda = 0.0057009, prior_strength = 0.83494),
    effective_disposals       = list(lambda = 0.0055639, prior_strength = 0.66762),
    intercept_marks           = list(lambda = 0.0034041, prior_strength = 2.70930),
    f50_ground_ball_gets      = list(lambda = 0.0035451, prior_strength = 3.92520),
    score_launches            = list(lambda = 0.0014319, prior_strength = 6.17850),
    marks_on_lead             = list(lambda = 0.0036591, prior_strength = 3.72130),
    tackles_inside50          = list(lambda = 0.0035032, prior_strength = 4.18340),
    kickins                   = list(lambda = 0.0105200, prior_strength = 0.25693),
    centre_bounce_attendances = list(lambda = 0.0128680, prior_strength = 0.06439),
    ruck_contests             = list(lambda = 0.0087614, prior_strength = 0.17053),
    contest_def_one_on_ones   = list(lambda = 0.0051497, prior_strength = 2.13480),
    contest_off_one_on_ones   = list(lambda = 0.0053110, prior_strength = 1.67780),
    contest_off_wins          = list(lambda = 0.0031213, prior_strength = 4.94890),
    contest_def_losses        = list(lambda = 0.0035192, prior_strength = 10.32900),
    dream_team_points         = list(lambda = 0.0166231, prior_strength = 0.28573),
    rating_points             = list(lambda = 0.0144069, prior_strength = 0.53531),
    # Efficiency stats (Beta-Binomial, optimized via multi-start log-loss)
    disposal_efficiency       = list(lambda = 0.0025644, prior_strength = 112.53000),
    goal_accuracy             = list(lambda = 1.00e-07, prior_strength = 91.36700),
    contested_poss_rate       = list(lambda = 0.0030203, prior_strength = 39.27200),
    hitout_win_pct            = list(lambda = 0.0011221, prior_strength = 240.00000),
    kick_efficiency           = list(lambda = 0.0025202, prior_strength = 64.84900),
    cond_tog                  = list(lambda = 0.0076683, prior_strength = 1.19220),
    squad_selection           = list(lambda = 0.0161353, prior_strength = 0.56141)
  )
}


# ============================================================================
# Backward compatibility aliases
# ============================================================================

#' @rdname stat_rating_definitions
#' @export
skill_stat_definitions <- stat_rating_definitions

#' @rdname stat_rating_position_map
#' @export
skill_position_map <- stat_rating_position_map

#' @rdname default_stat_rating_params
#' @export
default_skill_params <- default_stat_rating_params

#' @rdname .stat_rating_params
#' @keywords internal
.skill_stat_params <- .stat_rating_params
