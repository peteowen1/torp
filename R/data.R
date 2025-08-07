#' Expected Points Model for AFL Game Situations
#'
#' A GAM (Generalized Additive Model) fitted model object for predicting expected points
#' from various game situations based on field position, time, and game context.
#'
#' @format A GAM model object fitted using mgcv package
#' @source Created using data from AFL match tracking data via fitzRoy package
"ep_model"

#' AFL Match Fixtures and Schedule Information
#'
#' Contains match fixture data including team matchups, venues, and scheduling information.
#'
#' @format A data frame with match fixture variables:
#' \describe{
#'   \item{providerId}{Unique identifier for the match}
#'   \item{date}{Date and time of the match}
#'   \item{home.team.name}{Home team name}
#'   \item{away.team.name}{Away team name}
#'   \item{venue.name}{Venue where the match is played}
#'   \item{round.name}{Round name/number}
#'   \item{status}{Match status (scheduled, completed, etc.)}
#' }
#' @source AFL fixture data via fitzRoy package
"fixtures"

#' Player Shot Scoring Statistics
#'
#' Contains aggregated shot scoring statistics for individual players including
#' accuracy metrics and shot location data.
#'
#' @format A data frame with player shot statistics:
#' \describe{
#'   \item{player_id}{Unique identifier for each player}
#'   \item{player_name}{Player's full name}
#'   \item{shots_attempted}{Total number of shots attempted}
#'   \item{goals_scored}{Number of goals scored}
#'   \item{behinds_scored}{Number of behinds scored}
#'   \item{shot_accuracy}{Percentage accuracy of shots}
#'   \item{avg_shot_distance}{Average distance of shots from goal}
#' }
#' @source Derived from AFL match tracking data
"player_shot_score"

#' Player Game-Level Statistics
#'
#' Comprehensive player statistics at the individual game level including
#' traditional AFL statistics and advanced metrics.
#'
#' @format A data frame with player game statistics:
#' \describe{
#'   \item{player_id}{Unique identifier for each player}
#'   \item{player_name}{Player's full name}
#'   \item{match_id}{Unique identifier for each match}
#'   \item{team}{Team the player represented}
#'   \item{disposals}{Total disposals (kicks + handballs)}
#'   \item{kicks}{Number of kicks}
#'   \item{handballs}{Number of handballs}
#'   \item{marks}{Number of marks taken}
#'   \item{tackles}{Number of tackles made}
#'   \item{goals}{Goals scored}
#'   \item{behinds}{Behinds scored}
#'   \item{fantasy_points}{Fantasy football points scored}
#' }
#' @source AFL player statistics via fitzRoy package
"plyr_gm_df"

#' AFL Match Results
#'
#' Contains final scores and match outcome information for AFL games.
#'
#' @format A data frame with match results:
#' \describe{
#'   \item{match_id}{Unique identifier for each match}
#'   \item{home.team.name}{Name of the home team}
#'   \item{away.team.name}{Name of the away team}
#'   \item{home.score.total.goals}{Home team goals scored}
#'   \item{home.score.total.behinds}{Home team behinds scored}
#'   \item{home.score.total.points}{Home team total points}
#'   \item{away.score.total.goals}{Away team goals scored}
#'   \item{away.score.total.behinds}{Away team behinds scored}
#'   \item{away.score.total.points}{Away team total points}
#'   \item{margin}{Winning margin in points}
#'   \item{venue.name}{Venue where match was played}
#' }
#' @source AFL match results via fitzRoy package
"results"

#' Shot Outcome Classification Model
#'
#' A machine learning model for predicting shot outcomes (goal, behind, miss)
#' based on shot location, angle, distance, and game context.
#'
#' @format A fitted classification model object (likely randomForest or similar)
#' @source Trained on AFL shot data from match tracking
"shot_ocat_mdl"

#' Individual Shot Event Data
#'
#' Detailed tracking data for individual shot attempts including location,
#' context, and outcome information.
#'
#' @format A data frame with individual shot events:
#' \describe{
#'   \item{player_id}{Unique identifier for the shooting player}
#'   \item{player_name}{Name of the shooting player}
#'   \item{match_id}{Unique identifier for the match}
#'   \item{x}{X-coordinate of shot location (meters from goal line)}
#'   \item{y}{Y-coordinate of shot location (meters from center)}
#'   \item{shot_distance}{Distance from goal in meters}
#'   \item{shot_angle}{Angle to goal in degrees}
#'   \item{outcome}{Shot outcome: 'goal', 'behind', 'miss'}
#'   \item{period}{Quarter/period of the shot}
#'   \item{time_remaining}{Time remaining in period (seconds)}
#' }
#' @source AFL match tracking data with shot locations
"shot_player_df"

#' AFL Team Information and Lineup Data
#'
#' Contains team roster information including player lineups and team details.
#'
#' @format A data frame with team and player lineup information:
#' \describe{
#'   \item{teamId}{Unique identifier for each team}
#'   \item{teamName}{Full team name}
#'   \item{teamAbbr}{Team abbreviation (3-4 letters)}
#'   \item{player.playerId}{Unique identifier for each player}
#'   \item{player.playerName.givenName}{Player's first name}
#'   \item{player.playerName.surname}{Player's surname}
#'   \item{player.jumperNumber}{Player's jumper/jersey number}
#'   \item{season}{AFL season year}
#'   \item{round}{Round number}
#' }
#' @source AFL team lineup data via fitzRoy package
"teams"

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
