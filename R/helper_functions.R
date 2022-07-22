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

  ### TOTAL VARIABLE CHANGE
  df <-
    df %>%
    dplyr::mutate(
      y = -y,
      x = ifelse(description == "Spoil", -x, x),
      y = ifelse(description == "Spoil", -y, y),
      home_away = as.factor(if_else(team_id == home_team_id, "Home", "Away")),
      goal_x = venue_length / 2 - x,
      play_type = fct_lump_min(case_when(
        str_starts(lag_desc, "Free") ~ "Set Shot",
        str_starts(lag_desc, "OOF") ~ "Set Shot",
        str_starts(lag_desc, "Out") ~ "Set Shot",
        str_detect(lag_desc, "ted Mark") ~ "Set Shot",
        str_detect(lag_desc, "Mark On") ~ "Set Shot",
        str_starts(lag_desc, "Loose Ball") ~ "Loose Ball",
        str_detect(lag_desc, "Hard Ball") ~ "Hard Ball", ###### maybe comment this out
        str_detect(lag_desc, "Handball") ~ "Handball",
        TRUE ~ "Hard Ball")
        ,1),
      throw_in = case_when(
        description == "Centre Bounce" ~ 1,
        description == "Out of Bounds" ~ 1,
        description == "Ball Up Call" ~ 1,
        TRUE ~ 0
      )
    )

  ### GAME VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id) %>%
    dplyr::mutate(rn = row_number(),
                  lag_desc = fct_lump_min(lag(description), 1),
                  lag_goal_x = (lag(goal_x)),
                  lag_y = (lag(y)),
                  ### not sure about top 3 decide later
                  home_points_row = case_when(
                    lag(chain_number) != chain_number &
                      lag(home_away) == "Away" &
                      lag(final_state) == "rushed" ~ 1,
                    lag(chain_number) != chain_number &
                      lag(home_away) == "Home" &
                      lag(final_state) == "behind" ~ 1,
                    lag(chain_number) != chain_number &
                      lag(home_away) == "Home" &
                      lag(final_state) == "goal" ~ 6,
                    TRUE ~ 0
                  ),
                  away_points_row = case_when(
                    lag(chain_number) != chain_number &
                      lag(home_away) == "Home" &
                      lag(final_state) == "rushed" ~ 1,
                    lag(chain_number) != chain_number &
                      lag(home_away) == "Away" &
                      lag(final_state) == "behind" ~ 1,
                    lag(chain_number) != chain_number &
                      lag(home_away) == "Away" &
                      lag(final_state) == "goal" ~ 6,
                    TRUE ~ 0),
                  home_points = cumsum(home_points_row),
                  away_points = cumsum(away_points_row),
                  pos_points = if_else(home_away == "Home", home_points, away_points),
                  opp_points = if_else(home_away == "Home", away_points, home_points),
                  points_diff = pos_points - opp_points
                  ) %>%
    dplyr::ungroup()

  ### QUARTER VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id,period) %>%
    dplyr::mutate(rn = row_number()
                  ) %>%
    dplyr::ungroup()



  ### GOAL/SCORE VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id,period,shot_id) %>%
    dplyr::mutate(rn = row_number()
                  ) %>%
    dplyr::ungroup()
}
