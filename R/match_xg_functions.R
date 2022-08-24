#' add ep vars
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' add_epv_vars(df)
#' }
match_xgs <- function(season,round,match_id = NA){
  df <- load_chains(season,round)

  shots_df <- df %>%
    clean_pbp() %>%
    clean_model_data_epv() %>%
    clean_shots_data() %>%
    add_shot_vars() %>%
    dplyr::group_by(match_id) %>%
    dplyr::summarise(home_team = max(home_team),
                     # home_score = max(home_team_score),
                     home_shots_score = sum(ifelse(team==home_team,points_shot,0) , na.rm=T),
                     home_xscore = sum(ifelse(team==home_team,xscore,0), na.rm=T),
                     # home_goals = max(home_team_score_goals),
                     # home_behinds = max(home_team_score_behinds),
                     home_shots_goals = sum(ifelse(team==home_team,ifelse(points_shot==6,1,0),0) , na.rm=T),
                     home_shots_behinds = sum(ifelse(team==home_team,ifelse(points_shot==1,1,0),0) , na.rm=T),
                     away_team = max(away_team),
                     # away_score = max(away_team_score),
                     away_shots_score = sum(ifelse(team==away_team,points_shot,0), na.rm=T),
                     away_xscore = sum(ifelse(team==away_team,xscore,0), na.rm=T),
                     # away_goals = max(away_team_score_goals),
                     # away_behinds = max(away_team_score_behinds),
                     away_shots_goals = sum(ifelse(team==away_team,ifelse(points_shot==6,1,0),0) , na.rm=T),
                     away_shots_behinds = sum(ifelse(team==away_team,ifelse(points_shot==1,1,0),0) , na.rm=T),
                     )

  return(shots_df)
}

