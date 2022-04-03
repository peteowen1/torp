#' Clean PBP
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' clean_pbp(df)
#' }
clean_pbp <- function(df) {
  df <-
    df %>%
    dplyr::rename(
      team = team_team_name,
      home_team = home_team_team_name,
      away_team = away_team_team_name,
      home_team_score = home_team_score_total_score,
      away_team_score = away_team_score_total_score,
      home_team_direction = home_team_direction_qtr1
    )

  df <-
    df %>%
    dplyr::mutate(
      y = -y,
      home_away = as.factor(if_else(team_id == home_team_id, "Home", "Away")),
      goal_x = venue_length / 2 - x,
    ) %>%
    dplyr::group_by(match_id, chain_number) %>%
    dplyr::mutate(rn = row_number()) %>%
    dplyr::ungroup()


}
