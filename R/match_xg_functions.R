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
                     )

  return(shots_df)
}

# xg_21 <- purrr::map_df(1:27,~match_xgs(2021,.))
# xg_22 <- purrr::map_df(1:27,~match_xgs(2022,.))
# xg_tot <- bind_rows(xg_21,xg_22)
