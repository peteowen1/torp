#' Clean model data
#'
#' @param df
#'
#'
#' @examples
#' \dontrun{
#' clean_model_data_epv(df)
#' }
clean_model_data_epv <- function(df) {
  model_data <- df %>%
    dplyr::filter(
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
    dplyr::filter(dplyr::lag(throw_in) == 0 | lead(throw_in) == 0 | throw_in == 0) %>%
    dplyr::mutate(
      lag_desc = dplyr::lag(description, default = dplyr::first(description)),
      lead_desc = dplyr::lead(description, default = dplyr::last(description)),
      team_id_mdl = dplyr::case_when(
        throw_in == 1 ~ dplyr::lead(team_id),
        TRUE ~ team_id
      ),
      team_id_mdl = zoo::na.locf0(team_id_mdl, fromLast = TRUE),
      team_id_mdl = zoo::na.locf0(team_id_mdl),
      home = dplyr::if_else(team_id_mdl == home_team_id, 1, 0),
      # scoring_team_id = dplyr::if_else(points_row != 0, team_id, NA_character_),
      # home_points = cumsum(home_points_row),
      # away_points = cumsum(away_points_row),
      pos_points = dplyr::if_else(home == 1, home_points, away_points),
      opp_points = dplyr::if_else(home == 1, away_points, home_points),
      points_diff = pos_points - opp_points,
      mirror = dplyr::case_when(
        # not needed for now #(throw_in == 1 & lag(throw_in) != 1 & dplyr::lag(team_id_mdl) == team_id_mdl) ~ x,
        (throw_in == 1 & dplyr::lag(throw_in) != 1 & dplyr::lag(team_id_mdl) != team_id_mdl) ~ -1,
        # not needed for now  #(throw_in == 1 & lag(throw_in) == 1 & dplyr::lag(team_id_mdl,n=2L) == team_id_mdl) ~ x,
        (throw_in == 1 & dplyr::lag(throw_in) == 1 & dplyr::lag(team_id_mdl, n = 2L) != team_id_mdl) & sign(dplyr::lag(x)) == sign(x) ~ -1,
        (throw_in == 1 & dplyr::lag(throw_in) == 1 & dplyr::lag(team_id_mdl, n = 2L) == team_id_mdl & sign(dplyr::lag(x)) != sign(x)) ~ -1,
        (dplyr::lag(throw_in) == 1 & sign(dplyr::lag(x)) == sign(x) & dplyr::lag(team_id_mdl) == team_id_mdl & dplyr::lag(team_id_mdl, n = 2L) != team_id_mdl) ~ -1,
        (dplyr::lag(throw_in, n = 2L) == 1 & sign(dplyr::lag(x, n = 2L)) == sign(x) & dplyr::lag(team_id_mdl, n = 2L) == team_id_mdl & dplyr::lag(team_id_mdl, n = 3L) != team_id_mdl) ~ -1,
        # not needed for now  #(dplyr::lag(throw_in, n = 3L) == 1 & sign(dplyr::lag(x,n=3L)) == sign(x) & dplyr::lag(team_id_mdl,n=3L) == team_id_mdl & dplyr::lag(team_id_mdl,n=4L) != team_id_mdl) ~ -x,
        (dplyr::lag(throw_in) == 1 & sign(dplyr::lead(x, n = 2L)) == sign(x) & dplyr::lead(team_id_mdl, n = 2L) != team_id_mdl) ~ -1,
        (dplyr::lag(throw_in, n = 2L) == 1 & sign(dplyr::lead(x)) == sign(x) & dplyr::lead(team_id_mdl) != team_id_mdl) ~ -1,
        # not needed for now  # (description == "Mark Dropped" & dplyr::lag(team_id_mdl, default = dplyr::first(team_id_mdl)) != team_id_mdl) ~ -x,
        TRUE ~ 1
      ),
      x = mirror * x,
      y = mirror * y,
      goal_x = venue_length / 2 - x,
      lag_x = dplyr::if_else(dplyr::lag(team_id_mdl) == team_id_mdl, dplyr::lag(x, default = dplyr::first(x)), -dplyr::lag(x, default = dplyr::first(x))),
      lag_x = tidyr::replace_na(lag_x, dplyr::first(x)),
      lag_y = dplyr::if_else(dplyr::lag(team_id_mdl) == team_id_mdl, dplyr::lag(y, default = dplyr::first(y)), -dplyr::lag(y, default = dplyr::first(y))),
      lag_y = tidyr::replace_na(lag_y, dplyr::first(y)),
      lag_goal_x = dplyr::if_else(lag(team_id_mdl, 1) == team_id_mdl, dplyr::lag(goal_x, default = dplyr::first(goal_x)), venue_length - dplyr::lag(goal_x, default = dplyr::first(goal_x))),
      lag_goal_x = tidyr::replace_na(lag_goal_x, dplyr::first(goal_x)),
      lag_goal_x5 = dplyr::if_else(lag(team_id_mdl, 5) == team_id_mdl, dplyr::lag(goal_x, 5, default = dplyr::first(goal_x)), venue_length - dplyr::lag(goal_x, 5, default = dplyr::first(goal_x))),
      lag_goal_x5 = tidyr::replace_na(lag_goal_x5, dplyr::first(goal_x)),
      lag_time5 = dplyr::lag(period_seconds, 5, default = dplyr::first(period_seconds)),
      speed1 = (lag_goal_x - goal_x) / pmax((period_seconds - lag(period_seconds)), 1),
      speed1 = tidyr::replace_na(speed1, 0),
      speed5 = (lag_goal_x5 - goal_x) / pmax((period_seconds - lag_time5), 1),
      speed5 = tidyr::replace_na(speed5, 0),
      lag_player = dplyr::lag(player_name, default = dplyr::first(player_name)),
      lead_player = dplyr::lead(player_name, default = dplyr::last(player_name)),
      lead_x = dplyr::lead(x, default = dplyr::last(x)),
      lead_y = dplyr::lead(y, default = dplyr::last(y)),
      lead_goal_x = venue_length / 2 - lead_x
    ) %>%
    dplyr::ungroup() %>%
    fastDummies::dummy_cols(select_columns = c("play_type", "phase_of_play")) %>% # ,remove_selected_columns = TRUE
    janitor::clean_names()

  return(model_data)
}

clean_model_data_wp <- function(df) {
  model_data <- df %>%
    dplyr::filter(!is.na(label_wp)) %>%
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
      play_type_handball, play_type_kick, play_type_reception, phase_of_play_handball_received,
      phase_of_play_hard_ball, phase_of_play_loose_ball, phase_of_play_set_shot,
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
      play_type_handball, play_type_kick, play_type_reception, phase_of_play_handball_received,
      phase_of_play_hard_ball, phase_of_play_loose_ball, phase_of_play_set_shot
    )

  return(df)
}

clean_shots_data <- function(df) {
  ##### Direction Variales
  goal_width <- 6.4

  df <- df %>%
    dplyr::mutate(
      shot_result_multi =
        (dplyr::case_when(
          shot_at_goal == TRUE & disposal == "clanger" ~ 0,
          shot_at_goal == TRUE & disposal == "ineffective" ~ 1,
          shot_at_goal == TRUE & disposal == "effective" ~ 2,
          TRUE ~ NA_real_
        )),
      shot_result =
        as.numeric(dplyr::case_when(
          points_shot == 1 ~ 0,
          points_shot == 6 ~ 1,
          TRUE ~ NA_real_
        )),
      scored_shot = dplyr::if_else(!is.na(shot_at_goal), dplyr::if_else(!is.na(points_shot), 1, 0), NA_real_)
    )


  df$abs_y <- abs(df$y)
  df$side_b <- sqrt((df$goal_x)^2 + (df$y + goal_width / 2)^2)
  df$side_c <- sqrt((df$goal_x)^2 + (df$y - goal_width / 2)^2)
  df$angle <- acos((df$side_b^2 + df$side_c^2 - goal_width^2) / (2 * df$side_b * df$side_c))
  df$distance <- dplyr::if_else(df$y >= -goal_width / 2 & df$y <= goal_width / 2,
    df$goal_x, pmin(df$side_b, df$side_c)
  )
  df$shot_clanger <- dplyr::if_else(df$shot_at_goal == TRUE & df$disposal == "clanger", 1, 0)
  df$shot_effective <- dplyr::if_else(df$shot_at_goal == TRUE & df$disposal == "effective", 1, 0)
  df$shot_ineffective <- dplyr::if_else(df$shot_at_goal == TRUE & df$disposal == "ineffective", 1, 0)

  df <- df %>%
    dplyr::left_join(torp::shot_player_df, by = c("player_id" = "player_id_shot"), keep = TRUE)

  df$player_id_shot <- as.factor(tidyr::replace_na(df$player_id_shot, "Other"))
  df$player_name_shot <- as.factor(tidyr::replace_na(df$player_name_shot, "Other"))

  df <- df %>% dplyr::select(-side_b, -side_c)

  return(df)
}
