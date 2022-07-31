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
  if ("team_team_name" %in% colnames(df)) {
    df <-
      df %>%
      janitor::clean_names() %>%
      dplyr::rename(
        team = team_team_name,
        home_team = home_team_team_name,
        away_team = away_team_team_name,
        home_team_score = home_team_score_total_score,
        away_team_score = away_team_score_total_score,
        home_team_direction = home_team_direction_qtr1
      )
  }
  ### TOTAL VARIABLE CHANGE
  df <-
    df %>%
    dplyr::mutate(
      row_id = paste0(match_id, sprintf("%04d",display_order)),
      match_chain_id = paste0(match_id, chain_number),
      y = -y,
      x = ifelse(description == "Spoil", -x, x),
      y = ifelse(description == "Spoil", -y, y),
      home_away = as.factor(if_else(team_id == home_team_id, "Home", "Away")),
      goal_x = venue_length / 2 - x,
      throw_in = case_when(
        description == "Centre Bounce" ~ 1,
        description == "Out of Bounds" ~ 1,
        description == "Ball Up Call" ~ 1,
        TRUE ~ 0,
      ),
      shot_row = if_else(is.na(shot_at_goal), 0, 1),
      player_position_fac = forcats::fct_explicit_na(player_position),
      player_name = forcats::fct_explicit_na(paste(player_name_given_name, player_name_surname)),
      model_desc = forcats::fct_lump_min(description, 100),
      points_shot = ifelse(shot_at_goal == T,
                      ifelse(disposal == "effective", 6,
                             ifelse(disposal == "ineffective", 1, NA)
                      ), NA
      )
    )

  ### GAME VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id) %>%
    dplyr::mutate(
      rn = row_number(),
      lag_desc_tot = dplyr::lag(description, default = "First Bounce"),
      phase_of_play =
        case_when(
          stringr::str_starts(lag_desc_tot, "Free") ~ "Set Shot",
          stringr::str_starts(lag_desc_tot, "OOF") ~ "Set Shot",
          stringr::str_starts(lag_desc_tot, "Out on") ~ "Set Shot",
          stringr::str_detect(lag_desc_tot, "ted Mark") ~ "Set Shot",
          stringr::str_detect(lag_desc_tot, "Mark On") ~ "Set Shot",
          stringr::str_starts(description, "Free") ~ "Set Shot",
          # stringr::str_starts(description, "OOF") ~ "Set Shot",
          # stringr::str_starts(description, "Out on") ~ "Set Shot",
          stringr::str_detect(description, "ted Mark") ~ "Set Shot",
          stringr::str_detect(description, "Mark On") ~ "Set Shot",
          stringr::str_starts(lag_desc_tot, "Loose Ball") ~ "Loose Ball",
          stringr::str_starts(description, "Loose Ball") ~ "Loose Ball",
          stringr::str_starts(description, "Contested Knock On") ~ "Hard Ball",
          stringr::str_detect(lag_desc_tot, "Hard Ball") ~ "Hard Ball",
          stringr::str_detect(description, "Hard Ball") ~ "Hard Ball",
          stringr::str_detect(lag_desc_tot, "Handball") ~ "Handball Received",
          TRUE ~ "Hard Ball"
        ),
      phase_of_play = fct_lump_min(ifelse(lag_desc_tot=="Bounce",dplyr::lag(phase_of_play),phase_of_play),1),
      play_type = fct_lump_min(
        case_when(
          description == "Handball" ~ "Handball",
          description == "Kick" ~ "Kick",
          TRUE ~ "Reception"
        ), 1
      ),
      lag_goal_x = (lag(goal_x)),
      lag_x = (lag(x)),
      lag_y = (lag(y)),
      ### not sure about top 3 decide later
      points_row = case_when(
        chain_number != lead(chain_number, default = (last(chain_number) + 1)) &
          final_state == "rushed" ~ 1,
        description == "Behind" ~ 1,
        description == "Goal" ~ 6,
        TRUE ~ 0
      ),
      points_row_na = ifelse(points_row == 0, NA, points_row),
      home_points_row = dplyr::if_else(home_away == "Home", points_row, 0),
      away_points_row = dplyr::if_else(home_away == "Away", points_row, 0),
      is_goal_row = dplyr::if_else(description == "Goal", 1, 0),
      is_behind_row = dplyr::if_else(home_points_row == 1 | away_points_row == 1, 1, 0),
      tot_goals = cumsum(is_goal_row),
      scoring_team_id = ifelse(points_row != 0, team_id, NA),
      home_points = cumsum(home_points_row),
      away_points = cumsum(away_points_row),
      pos_points = dplyr::if_else(home_away == "Home", home_points, away_points),
      opp_points = dplyr::if_else(home_away == "Home", away_points, home_points),
      points_diff = pos_points - opp_points
    ) %>%
    dplyr::ungroup()

  ### QUARTER VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id, period) %>%
    dplyr::mutate(
      rn_qtr = dplyr::row_number(),
      pos_points = zoo::na.locf0(points_row_na, fromLast = TRUE),
      pos_points_team_id = zoo::na.locf0(scoring_team_id, fromLast = TRUE),
      team_id2 = zoo::na.locf0(team_id),
      model_points = dplyr::if_else(pos_points_team_id == team_id2, pos_points, -pos_points),
      pos_is_goal = dplyr::if_else(pos_points == 6, 1, 0),
      pos_team_shot = dplyr::if_else(model_points > 0, 1, 0),
      is_shot = dplyr::if_else(!is.na(model_points), 1, 0),
      next_score = case_when(
        model_points == -6 ~ 0,
        model_points == -1 ~ 1,
        model_points == 1 ~ 2,
        model_points == 6 ~ 3,
        TRUE ~ 4
      ),
      label = as.numeric(next_score)
    ) %>%
    dplyr::ungroup()



  ### GOAL/SCORE VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id, period, tot_goals) %>%
    dplyr::mutate(
      team_id_mdl = dplyr::case_when(
        throw_in == 1 ~ dplyr::lead(team_id, default = dplyr::last(team_id)),
        TRUE ~ team_id
      ),
      # x = dplyr::case_when(
      #   (throw_in == 1 & dplyr::lag(team_id_mdl, default = dplyr::first(team_id_mdl)) == team_id_mdl) ~ x,
      #   (throw_in == 1 & dplyr::lag(team_id_mdl, default = dplyr::first(team_id_mdl)) != team_id_mdl) ~ -x,
      #   TRUE ~ x
      # ),
      # goal_x = dplyr::case_when(
      #   (throw_in == 1 & dplyr::lag(team_id_mdl, default = dplyr::first(team_id_mdl)) == team_id_mdl) ~ x,
      #   (throw_in == 1 & dplyr::lag(team_id_mdl, default = dplyr::first(team_id_mdl)) != team_id_mdl) ~ -x,
      #   TRUE ~ x
      # ),
      rn_gls = row_number(),
      lag_x = ifelse(dplyr::lag(team_id_mdl) == team_id_mdl, dplyr::lag(x), -dplyr::lag(x)),
      lag_x = tidyr::replace_na(lag_x, dplyr::first(x)),
      lag_y = ifelse(dplyr::lag(team_id_mdl) == team_id_mdl, dplyr::lag(y), -dplyr::lag(y)),
      lag_y = tidyr::replace_na(lag_y, first(y)),
      lag_goal_x = venue_length / 2 - lag_x,
      lag_time5 = dplyr::lag(period_seconds, 5, default = dplyr::first(period_seconds)),
      lag_goal_x5 = dplyr::if_else(lag(team_id_mdl, 5) == team_id_mdl, dplyr::lag(goal_x, 5), venue_length - dplyr::lag(goal_x, 5)),
      lag_goal_x5 = tidyr::replace_na(lag_goal_x5, dplyr::first(goal_x)),
      speed5 = (lag_goal_x5 - goal_x) / pmax((period_seconds - lag_time5), 1),
      speed5 = tidyr::replace_na(speed5, 0),
      lead_desc = dplyr::lead(description, default = dplyr::last(description)),
      lead_x = dplyr::lead(x, default = dplyr::last(x)),
      lead_y = dplyr::lead(y, default = dplyr::last(y)),
      lead_goal_x = venue_length / 2 - lead_x,
      lag_model_desc = dplyr::lag(model_desc, default = "Centre Bounce")
    ) %>%
    dplyr::ungroup()

  return(df)
}
