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
                     home_score = max(home_team_score),
                     home_shots_score = sum(ifelse(team==home_team,points_shot,0) , na.rm=T),
                     home_xg = sum(ifelse(team==home_team,xg,0), na.rm=T),
                     away_team = max(away_team),
                     away_score = max(away_team_score),
                     away_shots_score = sum(ifelse(team==away_team,points_shot,0), na.rm=T),
                     away_xg = sum(ifelse(team==away_team,xg,0), na.rm=T)
                     )

  return(shots_df)
}

get_shot_preds <- function(df){
  shots <- df %>%
    clean_pbp() %>%
    filter(shot_at_goal == TRUE) %>%
    clean_shots_df()

  goal_prob <- predict(shot_goal_mdl,shots,type="response")
  behind_prob <- predict(shot_behind_mdl,shots,type="response")

  final_df <- cbind(shots,goal_prob,behind_prob) %>%
    dplyr::mutate(xg = 6*goal_prob + behind_prob,
                  tot_prob = goal_prob + behind_prob )

  return(final_df)
}


