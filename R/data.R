#' Expected Points Model for AFL Game Situations
#'
#' A GAM (Generalized Additive Model) fitted model object for predicting expected points
#' from various game situations based on field position, time, and game context.
#'
#' @format A GAM model object fitted using mgcv package
#' @source Created using data from AFL match tracking data via fitzRoy package
"ep_model"


#' Player Shot Scoring Statistics
#'
#' Contains statistical analysis results for player shot performance
#' including effect sizes, confidence intervals, and grouping variables.
#'
#' @format A data frame with player shot analysis:
#' \describe{
#'   \item{effect}{Statistical effect measure}
#'   \item{group}{Grouping category}
#'   \item{group_var}{Grouping variable identifier}
#'   \item{lower_2.5}{Lower bound of 95% confidence interval}
#'   \item{n}{Sample size}
#'   \item{player_name_shot}{Name of the shooting player}
#'   \item{se}{Standard error}
#'   \item{upper_97.5}{Upper bound of 95% confidence interval}
#'   \item{value}{Statistical value/estimate}
#' }
#' @source Derived from AFL match tracking data statistical analysis
"player_shot_score"

#' Player Game-Level Statistics
#'
#' Comprehensive player statistics at the individual game level including
#' traditional AFL statistics, advanced metrics, team performance data,
#' and calculated ratings from the TORP system.
#'
#' @format A data frame with extensive player game statistics including
#'   traditional AFL stats (disposals, kicks, handballs, marks, goals, behinds, tackles),
#'   advanced metrics (contested possessions, pressure acts, intercepts),
#'   team context (home/away, opponent, venue), match details (season, round),
#'   player information (position, experience), and calculated performance ratings
#'   (TORP points for receiving, disposal, spoiling, hitouts).
#' @source AFL player statistics via fitzRoy package with TORP enhancements
"plyr_gm_df"


#' Shot Outcome Classification Model
#'
#' A machine learning model for predicting shot outcomes (goal, behind, miss)
#' based on shot location, angle, distance, and game context.
#'
#' @format A fitted classification model object (likely randomForest or similar)
#' @source Trained on AFL shot data from match tracking
"shot_ocat_mdl"

#' Shot Player Reference Data
#'
#' Reference dataset containing player identifiers and names for
#' linking shot events to specific players.
#'
#' @format A data frame with shot player references:
#' \describe{
#'   \item{player_id_shot}{Unique identifier for the shooting player}
#'   \item{player_name_shot}{Name of the shooting player}
#' }
#' @source AFL player reference data for shot analysis
"shot_player_df"


#' Complete TORP Analysis Dataset
#'
#' The comprehensive dataset containing all processed AFL data including
#' play-by-play events, expected points, win probabilities, and player metrics.
#'
#' @format A large data frame with processed AFL match data:
#' \describe{
#'   \item{match_id}{Unique identifier for each match}
#'   \item{period}{Quarter/period number (1-4)}
#'   \item{period_seconds}{Seconds elapsed in the period}
#'   \item{x}{X-coordinate on field (meters from goal)}
#'   \item{y}{Y-coordinate on field (meters from center)}
#'   \item{team}{Team in possession}
#'   \item{player_id}{Player identifier}
#'   \item{action_type}{Type of action (kick, handball, mark, etc.)}
#'   \item{ep_start}{Expected points at start of possession}
#'   \item{ep_end}{Expected points at end of possession}
#'   \item{ep_added}{Expected points added by the action}
#'   \item{wp}{Win probability for the team}
#' }
#' @source Processed AFL tracking and event data
"torp_df_total"

#' Win Probability Model
#'
#' A statistical model for calculating real-time win probabilities during AFL matches
#' based on score differential, time remaining, and field position.
#'
#' @format A fitted regression model object (likely GAM or logistic regression)
#' @source Trained on historical AFL match data with score and time states
"wp_model"

#' XGBoost Win Prediction Model
#'
#' An XGBoost machine learning model for predicting AFL match winners
#' based on team performance metrics, historical data, and game context.
#'
#' @format An XGBoost model object (xgb.Booster)
#' @source Trained on historical AFL match results and team statistics
"xgb_win_model"

