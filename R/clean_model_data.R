#' Clean model data
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' clean_model_data(df)
#' }
clean_model_data_epv <- function(df) {
  model_data <- df %>%
    filter(
      description == "Ball Up Call" |
        description == "Bounce" |
        description == "Centre Bounce" |
        description == "Contested Knock On" |
        description == "Contested Mark" |
        description == "Free Advantage" |
        description == "Free For" |
        description == "Free For: Before the Bounce" |
        description == "Free For: In Possession" |
        description == "Free For: Off The Ball" |
        description == "Gather" |
        description == "Gather From Hitout" |
        description == "Gather from Opposition" |
        description == "Ground Kick" |
        description == "Handball" |
        description == "Handball Received" |
        description == "Hard Ball Get" |
        description == "Hard Ball Get Crumb" |
        description == "Kick" |
        description == "Knock On" |
        description == "Loose Ball Get" |
        description == "Loose Ball Get Crumb" |
        description == "Mark On Lead" |
        description == "Out of Bounds" |
        description == "Out On Full After Kick" |
        description == "Ruck Hard Ball Get" |
        description == "Uncontested Mark",
      # description != "Kick Into F50",
      # description != "Kick Inside 50 Result",
      # description != "Shot At Goal",
      # description != "Spoil",
      # description != "Behind",
      # description != "Goal",
      # description != "Kickin play on",
      # description != "OOF Kick In",
      # description != "Contest Target",
      # description != "Mark Fumbled",
      # description != "Mark Dropped",
      ### description != "Out of Bounds",
      ### description != "Out On Full After Kick",
      ### lead_desc != "Out of Bounds",
      ### lead_desc != "Out On Full After Kick",
      !(x == -lead_x_tot & y == -lead_y_tot & description != "Centre Bounce")
    ) %>%
    dplyr::group_by(match_id, period, tot_goals) %>%
    dplyr::mutate(
      x = dplyr::case_when(
        (throw_in == 1 & dplyr::lag(team_id_mdl, default = dplyr::first(team_id_mdl)) == team_id_mdl) ~ x,
        (throw_in == 1 & dplyr::lag(team_id_mdl, default = dplyr::first(team_id_mdl)) != team_id_mdl) ~ -x,
        # (description == "Mark Dropped" & dplyr::lag(team_id_mdl, default = dplyr::first(team_id_mdl)) != team_id_mdl) ~ -x,
        TRUE ~ x
      ),
      goal_x = venue_length / 2 - x,
      lag_x = ifelse(dplyr::lag(team_id_mdl) == team_id_mdl, dplyr::lag(x), -dplyr::lag(x)),
      lag_x = tidyr::replace_na(lag_x, dplyr::first(x)),
      lag_y = ifelse(dplyr::lag(team_id_mdl) == team_id_mdl, dplyr::lag(y), -dplyr::lag(y)),
      lag_y = tidyr::replace_na(lag_y, first(y)),
      lag_goal_x = dplyr::if_else(lag(team_id_mdl, 1) == team_id_mdl, dplyr::lag(goal_x, 1), venue_length - dplyr::lag(goal_x, 1)),
      lag_goal_x = tidyr::replace_na(lag_goal_x, dplyr::first(goal_x)),
      lag_goal_x5 = dplyr::if_else(lag(team_id_mdl, 5) == team_id_mdl, dplyr::lag(goal_x, 5), venue_length - dplyr::lag(goal_x, 5)),
      lag_goal_x5 = tidyr::replace_na(lag_goal_x5, dplyr::first(goal_x)),
      lag_time5 = dplyr::lag(period_seconds, 5, default = dplyr::first(period_seconds)),
      speed1 = (lag_goal_x - goal_x) / pmax((period_seconds - lag(period_seconds)), 1),
      speed1 = tidyr::replace_na(speed1, 0),
      speed5 = (lag_goal_x5 - goal_x) / pmax((period_seconds - lag_time5), 1),
      speed5 = tidyr::replace_na(speed5, 0),
      lag_desc = dplyr::lag(description, default = dplyr::first(description)),
      lag_player = dplyr::lag(player_name, default = dplyr::first(description)),
      lead_desc = dplyr::lead(description, default = dplyr::last(description)),
      lead_player = dplyr::lead(player_name, default = dplyr::last(player_name)),
      lead_x = dplyr::lead(x, default = dplyr::last(x)),
      lead_y = dplyr::lead(y, default = dplyr::last(y)),
      lead_goal_x = venue_length / 2 - lead_x
    ) %>%
    dplyr::ungroup() %>%
    fastDummies::dummy_cols(select_columns = c("play_type", "phase_of_play")) %>% #,remove_selected_columns = TRUE
    janitor::clean_names()

  return(model_data)
}

clean_model_data_wp <- function(df) {
  model_data <- df %>%
    filter(!is.na(label_wp)) %>%
    dplyr::mutate(
      xpoints_diff = points_diff + exp_pts,
      pos_lead_prob = case_when(
        points_diff > 6 ~ 1,
        points_diff == 6 ~ (opp_goal * 0.5) + opp_behind + no_score + behind + goal,
        points_diff > 1 ~ (opp_behind * 0.5) + no_score + behind + goal,
        points_diff == 0 ~ (no_score * 0.5) + behind + goal,
        points_diff == -1 ~ (behind * 0.5) + goal,
        points_diff > -6 ~ goal,
        points_diff == -6 ~ (0.5 * goal),
        points_diff < -6 ~ 0
      ),
      time_left_scaler = exp(pmin(((period - 1) * 2000 + period_seconds) / 2000, 4)),
      diff_time_ratio = xpoints_diff * time_left_scaler
    ) %>%
    fastDummies::dummy_cols(select_columns = c("play_type", "phase_of_play")) %>%
    janitor::clean_names()

  return(model_data)
}


select_epv_model_vars <- function(df) {
  df <-
    df %>%
    dplyr::select(
      goal_x, y, lag_goal_x, lag_goal_x5, lag_y, period_seconds, period,
      play_type_handball,play_type_kick,play_type_reception,phase_of_play_handball_received,
      phase_of_play_hard_ball,phase_of_play_loose_ball,phase_of_play_set_shot,
      shot_row, speed5, home
    )

  return(df)
}

select_wp_model_vars <- function(df) {
  df <-
    df %>%
    dplyr::select(
      total_seconds, shot_row, home, points_diff,
      xpoints_diff, pos_lead_prob, time_left_scaler, diff_time_ratio,
      play_type_handball,play_type_kick,play_type_reception,phase_of_play_handball_received,
      phase_of_play_hard_ball,phase_of_play_loose_ball,phase_of_play_set_shot
    )

  return(df)
}
