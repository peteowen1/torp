#' Expected points model for game situations
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{variable1}{Description of variable1}
#'   \item{variable2}{Description of variable2}
#'   ...
#' }
#' @source \url{http://example.com}
"ep_model"

#' Match schedule or fixture information
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{date}{Date of the match}
#'   \item{home_team}{Home team name}
#'   \item{away_team}{Away team name}
#'   \item{venue}{Venue of the match}
#'   ...
#' }
#' @source \url{http://example.com}
"fixtures"

#' Player-specific shot scoring data
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{player_id}{Unique identifier for each player}
#'   \item{shot_type}{Type of shot taken}
#'   \item{score}{Score achieved}
#'   ...
#' }
#' @source \url{http://example.com}
"player_shot_score"

#' Player game-level data
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{player_id}{Unique identifier for each player}
#'   \item{game_id}{Unique identifier for each game}
#'   \item{stat1}{Description of stat1}
#'   \item{stat2}{Description of stat2}
#'   ...
#' }
#' @source \url{http://example.com}
"plyr_gm_df"

#' Game results data
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{game_id}{Unique identifier for each game}
#'   \item{home_team}{Home team name}
#'   \item{away_team}{Away team name}
#'   \item{home_score}{Score of the home team}
#'   \item{away_score}{Score of the away team}
#'   ...
#' }
#' @source \url{http://example.com}
"results"

#' Shot outcome model
#'
#' @format A model object or data frame with X rows and Y variables:
#' \describe{
#'   \item{variable1}{Description of variable1}
#'   \item{variable2}{Description of variable2}
#'   ...
#' }
#' @source \url{http://example.com}
"shot_ocat_mdl"

#' Player shot data
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{player_id}{Unique identifier for each player}
#'   \item{shot_id}{Unique identifier for each shot}
#'   \item{x_coordinate}{X-coordinate of the shot}
#'   \item{y_coordinate}{Y-coordinate of the shot}
#'   \item{outcome}{Outcome of the shot}
#'   ...
#' }
#' @source \url{http://example.com}
"shot_player_df"

#' Team information
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{team_id}{Unique identifier for each team}
#'   \item{team_name}{Full name of the team}
#'   \item{abbreviation}{Team abbreviation}
#'   \item{conference}{Conference the team belongs to}
#'   ...
#' }
#' @source \url{http://example.com}
"teams"

#' Total torpedo data frame
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{variable1}{Description of variable1}
#'   \item{variable2}{Description of variable2}
#'   ...
#' }
#' @source \url{http://example.com}
"torp_df_total"

#' Win probability model
#'
#' @format A model object or data frame with X rows and Y variables:
#' \describe{
#'   \item{variable1}{Description of variable1}
#'   \item{variable2}{Description of variable2}
#'   ...
#' }
#' @source \url{http://example.com}
"wp_model"
