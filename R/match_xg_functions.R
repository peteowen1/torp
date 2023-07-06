#' match_xgs
#'
#' @param season AFL season
#' @param round AFL round
#' @param match_id AFL match ID
#'
#' @export
#'
match_xgs <- function(season = get_afl_season(),round = get_afl_week(),match_id = NA){

  df <- load_pbp(seasons = season,rounds = round)

  shots_df <- df %>%
    dplyr::group_by(match_id) %>%
    dplyr::summarise(home_team = max(home_team_team_name),
                     # home_score = max(home_team_score),
                     home_shots_score = sum(dplyr::if_else(team==home_team_team_name ,points_shot,0) , na.rm=T),
                     home_xscore = sum(dplyr::if_else(team==home_team_team_name ,xscore,0), na.rm=T),
                     # home_goals = max(home_team_score_goals),
                     # home_behinds = max(home_team_score_behinds),
                     home_sG = sum(dplyr::if_else(team==home_team_team_name ,dplyr::if_else(points_shot==6,1,0),0) , na.rm=T),
                     home_sB = sum(dplyr::if_else(team==home_team_team_name ,dplyr::if_else(points_shot==1,1,0),0) , na.rm=T),
                     away_team = max(away_team_team_name ),
                     # away_score = max(away_team_score),
                     away_shots_score = sum(dplyr::if_else(team==away_team_team_name ,points_shot,0), na.rm=T),
                     away_xscore = sum(dplyr::if_else(team==away_team_team_name ,xscore,0), na.rm=T),
                     # away_goals = max(away_team_score_goals),
                     # away_behinds = max(away_team_score_behinds),
                     away_sG = sum(dplyr::if_else(team==away_team_team_name ,dplyr::if_else(points_shot==6,1,0),0) , na.rm=T),
                     away_sB = sum(dplyr::if_else(team==away_team_team_name ,dplyr::if_else(points_shot==1,1,0),0) , na.rm=T),
                     score_diff = home_shots_score - away_shots_score,
                     xscore_diff = home_xscore - away_xscore,
                     total_points = home_shots_score + away_shots_score,
                     total_xpoints = home_xscore + away_xscore
                     )

  return(shots_df)
}

