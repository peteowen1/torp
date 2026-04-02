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
    goals                     = list(lambda = 0.00291, prior_strength = 2.91),
    behinds                   = list(lambda = 0.00203, prior_strength = 6.31),
    shots_at_goal             = list(lambda = 0.00378, prior_strength = 1.55),
    score_involvements        = list(lambda = 0.00273, prior_strength = 2.49),
    goal_assists              = list(lambda = 0.00184, prior_strength = 10.95),
    kicks                     = list(lambda = 0.00591, prior_strength = 0.67),
    handballs                 = list(lambda = 0.00460, prior_strength = 0.75),
    disposals                 = list(lambda = 0.00596, prior_strength = 0.49),
    marks                     = list(lambda = 0.00327, prior_strength = 2.69),
    contested_possessions     = list(lambda = 0.00399, prior_strength = 0.98),
    uncontested_possessions   = list(lambda = 0.00527, prior_strength = 0.92),
    contested_marks           = list(lambda = 0.00214, prior_strength = 5.22),
    ground_ball_gets          = list(lambda = 0.00276, prior_strength = 1.85),
    clearances                = list(lambda = 0.00598, prior_strength = 0.50),
    inside50s                 = list(lambda = 0.00371, prior_strength = 1.92),
    marks_inside50            = list(lambda = 0.00310, prior_strength = 2.93),
    rebound50s                = list(lambda = 0.00653, prior_strength = 1.10),
    metres_gained             = list(lambda = 0.00541, prior_strength = 0.80),
    tackles                   = list(lambda = 0.00355, prior_strength = 1.50),
    spoils                    = list(lambda = 0.00442, prior_strength = 1.70),
    intercepts                = list(lambda = 0.00547, prior_strength = 1.78),
    one_percenters            = list(lambda = 0.00388, prior_strength = 2.22),
    pressure_acts             = list(lambda = 0.00606, prior_strength = 0.80),
    hitouts                   = list(lambda = 0.00581, prior_strength = 0.31),
    hitouts_to_advantage      = list(lambda = 0.00273, prior_strength = 1.14),
    frees_for                 = list(lambda = 0.00160, prior_strength = 7.60),
    frees_against             = list(lambda = 0.00086, prior_strength = 7.24),
    clangers                  = list(lambda = 0.00170, prior_strength = 5.09),
    turnovers                 = list(lambda = 0.00265, prior_strength = 4.34),
    bounces                   = list(lambda = 0.00449, prior_strength = 1.72),
    def_half_pressure_acts    = list(lambda = 0.00458, prior_strength = 1.53),
    centre_clearances         = list(lambda = 0.00530, prior_strength = 0.81),
    stoppage_clearances       = list(lambda = 0.00418, prior_strength = 1.20),
    effective_kicks           = list(lambda = 0.00573, prior_strength = 0.83),
    effective_disposals       = list(lambda = 0.00557, prior_strength = 0.66),
    intercept_marks           = list(lambda = 0.00340, prior_strength = 2.71),
    f50_ground_ball_gets      = list(lambda = 0.00353, prior_strength = 3.92),
    score_launches            = list(lambda = 0.00145, prior_strength = 6.19),
    marks_on_lead             = list(lambda = 0.00366, prior_strength = 3.77),
    tackles_inside50          = list(lambda = 0.00349, prior_strength = 4.18),
    kickins                   = list(lambda = 0.01052, prior_strength = 0.25),
    centre_bounce_attendances = list(lambda = 0.01289, prior_strength = 0.07),
    ruck_contests             = list(lambda = 0.00881, prior_strength = 0.21),
    contest_def_one_on_ones   = list(lambda = 0.00523, prior_strength = 2.19),
    contest_off_one_on_ones   = list(lambda = 0.00535, prior_strength = 1.68),
    contest_off_wins          = list(lambda = 0.00315, prior_strength = 4.89),
    contest_def_losses        = list(lambda = 0.00360, prior_strength = 10.38),
    dream_team_points         = list(lambda = 0.01649, prior_strength = 0.26),
    rating_points             = list(lambda = 0.01506, prior_strength = 0.47),
    # Efficiency stats (Beta-Binomial, optimized via multi-start log-loss)
    disposal_efficiency       = list(lambda = 0.00258, prior_strength = 112.69),
    goal_accuracy             = list(lambda = 1e-05, prior_strength = 92.33),
    contested_poss_rate       = list(lambda = 0.00303, prior_strength = 39.63),
    hitout_win_pct            = list(lambda = 0.00117, prior_strength = 245.42),
    kick_efficiency           = list(lambda = 0.00254, prior_strength = 65.13),
    cond_tog                  = list(lambda = 0.00757, prior_strength = 1.19),
    squad_selection           = list(lambda = 0.01607, prior_strength = 0.44)
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
