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
match_xgs <- function(season,round,match_id){
  df <- load_chains(season,round)
  shots_df <- df %>%
    get_shot_preds() %>%
    dplyr::group_by(match_id) %>%
    dplyr::summarise(home_team = max(home_team),
                     home_score = max(home_score),
                     home_shots_score = sum(points),
                     home_xg = sum(exp_g),
                     away_team = max(away_team),
                     away_score = max(away_score),
                     away_shots_score = sum(points),
                     away_xg = sum(exp_g)
                     )

  return(shots_df)
}

get_shot_preds <- function(df){
  shots <- df %>%
    clean_pbp() %>%
    filter(shot_at_goal == TRUE) %>%
    clean_shots_df()

  goal_prob <- predict(shot_goal_mdl,shots,type="response")
  clanger_prob <- predict(shot_clanger_mdl,shots,type="response")

  final_df <- cbind(shots,goal_prob,clanger_prob) %>%
    dplyr::mutate(xg = )

  return(final_df)
}

clean_shots_df <- function(df){
##### Direction Variales
goal_width <- 6.4

df$abs_y <- abs(df$y)
df$side_b <- sqrt((df$goal_x)^2 + (df$y + goal_width/2 )^2)
df$side_c <- sqrt((df$goal_x)^2 + (df$y - goal_width/2)^2)
df$angle <-  acos((df$side_b^2 + df$side_c^2 - goal_width^2)/(2*df$side_b*df$side_c))
df$distance <- ifelse(df$y >= -goal_width/2 & df$y <= goal_width/2,
                         df$goal_x, pmin(df$side_b ,df$side_c ))

df <- df %>% select(-side_b,-side_c)
return(df)
}
