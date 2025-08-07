# AFL Field and Game Constants
AFL_GOAL_WIDTH <- 6.4  # Width of AFL goal posts in meters
AFL_QUARTER_DURATION <- 2000  # Duration of AFL quarter in game seconds
AFL_MAX_PERIODS <- 4  # Maximum number of periods/quarters in AFL
AFL_TIME_SCALER_MAX <- 4  # Maximum value for time left scaler calculation

#' Clean Model Data for Expected Points Value (EPV)
#'
#' @param df A dataframe containing raw play-by-play data.
#' @return A cleaned dataframe ready for EPV modeling.
#' @export
clean_model_data_epv <- function(df) {
  df %>%
    filter_relevant_descriptions() %>%
    dplyr::group_by(.data$match_id, .data$period, .data$tot_goals) %>%
    dplyr::filter(dplyr::lag(.data$throw_in) == 0 | dplyr::lead(.data$throw_in) == 0 | .data$throw_in == 0) %>%
    add_epv_variables() %>%
    dplyr::ungroup() %>%
    fastDummies::dummy_cols(select_columns = c("play_type", "phase_of_play")) %>%
    janitor::clean_names()
}

#' Clean Model Data for Win Probability (WP)
#'
#' @param df A dataframe containing raw play-by-play data.
#' @return A cleaned dataframe ready for WP modeling.
#' @export
clean_model_data_wp <- function(df) {
  df %>%
    dplyr::filter(!is.na(.data$label_wp)) %>%
    dplyr::mutate(
      xpoints_diff = .data$points_diff + .data$exp_pts,
      pos_lead_prob = calculate_pos_lead_prob(.data$points_diff, .data$opp_goal, .data$opp_behind, .data$no_score, .data$behind, .data$goal),
      time_left_scaler = exp(pmin(((.data$period - 1) * AFL_QUARTER_DURATION + .data$period_seconds) / AFL_QUARTER_DURATION, AFL_TIME_SCALER_MAX)),
      diff_time_ratio = .data$xpoints_diff * .data$time_left_scaler
    ) %>%
    fastDummies::dummy_cols(select_columns = c("play_type", "phase_of_play")) %>%
    janitor::clean_names()
}

#' Select EPV Model Variables
#'
#' @param df A dataframe containing cleaned play-by-play data.
#' @return A dataframe with selected variables for EPV modeling.
#' @export
select_epv_model_vars <- function(df, label = FALSE) {
  base_vars <- c(
    "goal_x", "y", "lag_goal_x", "lag_goal_x5", "lag_y", 
    "period_seconds", "period", "play_type_handball", "play_type_kick", 
    "play_type_reception", "phase_of_play_handball_received", 
    "phase_of_play_hard_ball", "phase_of_play_loose_ball", 
    "phase_of_play_set_shot", "shot_row", "speed5", "home"
  )
  
  if (label) {
    base_vars <- c(base_vars, "label_ep")
  }
  
  df %>% dplyr::select(dplyr::all_of(base_vars))
}

#' Select WP Model Variables
#'
#' @param df A dataframe containing cleaned play-by-play data.
#' @return A dataframe with selected variables for WP modeling.
#' @export
select_wp_model_vars <- function(df) {
  df %>%
    dplyr::select(
      .data$total_seconds, .data$shot_row, .data$home, .data$points_diff,
      .data$xpoints_diff, .data$pos_lead_prob, .data$time_left_scaler, .data$diff_time_ratio,
      .data$play_type_handball, .data$play_type_kick, .data$play_type_reception, .data$phase_of_play_handball_received,
      .data$phase_of_play_hard_ball, .data$phase_of_play_loose_ball, .data$phase_of_play_set_shot
    )
}

#' Select AFL Match Prediction Model Variables
#'
#' @param df A dataframe containing AFL team model data
#' @return A dataframe with selected variables for AFL modeling
#' @export
select_afl_model_vars <- function(df) {
  df %>%
    dplyr::select(
      # Response variables
      # .data$total_xpoints_adj,
      # .data$xscore_diff,
      # .data$shot_conv_diff,
      # .data$score_diff,
      .data$win,

      # Bring Season through
      .data$season.x,

      # Team and game identifiers
      .data$team_type_fac.x,
      .data$team_name.x,
      .data$team_name.y,
      .data$team_name_season.x,
      .data$team_name_season.y,
      .data$team_type_fac,

      # Time-based variables
      .data$game_year_decimal.x,
      .data$game_prop_through_year.x,
      .data$game_prop_through_month.x,
      .data$game_wday_fac.x,
      .data$game_prop_through_day.x,

      # Team rating variables (TORP - Team Overall Rating Points)
      .data$torp_diff,
      .data$torp_recv_diff,
      .data$torp_disp_diff,
      .data$torp_spoil_diff,
      .data$torp_hitout_diff,
      .data$torp.x,
      .data$torp.y,

      # Venue and travel variables
      .data$venue_fac,
      .data$log_dist.x,
      .data$log_dist.y,
      .data$log_dist_diff,
      .data$familiarity.x,
      .data$familiarity.y,
      .data$familiarity_diff,
      .data$days_rest_diff_fac,

      # Model weights
      .data$weightz,
      .data$shot_weightz,

      # Predicted values (generated during modeling process)
      .data$pred_tot_xscore,
      .data$pred_xscore_diff,
      .data$pred_conv_diff,
      .data$pred_score_diff,
      .data$pred_win
    )
}


#' Clean Shots Data
#'
#' @param df A dataframe containing raw shot data.
#' @return A cleaned dataframe with additional shot-related variables.
#' @export
#' @importFrom utils data
clean_shots_data <- function(df) {
  goal_width <- AFL_GOAL_WIDTH
  
  # Load shot player data safely
  shot_player_df <- NULL
  utils::data("shot_player_df", package = "torp", envir = environment())

  df %>%
    add_shot_result_variables() %>%
    add_shot_geometry_variables(goal_width) %>%
    add_shot_type_variables() %>%
    dplyr::left_join(shot_player_df, by = c("player_id" = "player_id_shot"), keep = TRUE) %>%
    dplyr::mutate(
      player_id_shot = as.factor(tidyr::replace_na(.data$player_id_shot, "Other")),
      player_name_shot = as.factor(tidyr::replace_na(.data$player_name_shot, "Other"))
    ) %>%
    dplyr::select(-.data$side_b, -.data$side_c)
}

#' Select Shot Model Variables
#'
#' @param df A dataframe containing cleaned shot data.
#' @return A dataframe with selected variables for shot modeling.
#' @export
select_shot_model_vars <- function(df) {
  df %>%
    dplyr::select(
      .data$goal_x, .data$abs_y, .data$angle, .data$distance,
      .data$play_type, .data$phase_of_play,
      .data$player_id_shot, .data$player_name_shot
    )
}

#' Prepare Shot Data for Modeling
#'
#' @param df A dataframe containing raw shot data.
#' @return A dataframe ready for shot modeling.
#' @export
prepare_shot_model_data <- function(df) {
  df %>%
    clean_shots_data() %>%
    select_shot_model_vars()
}

#' Create Shot Model Matrix
#'
#' @param df A dataframe containing prepared shot data.
#' @return A model matrix ready for shot modeling.
#' @export
create_shot_model_matrix <- function(df) {
  stats::model.matrix(~ . - 1, data = df)
}

#' Predict Shot Probabilities
#'
#' @param model A fitted shot model.
#' @param new_data A dataframe or model matrix containing new shot data.
#' @return A vector of predicted probabilities.
#' @export
predict_shot_probabilities <- function(model, new_data) {
  stats::predict(model, newdata = new_data, type = "response")
}

#' Filter Relevant Descriptions
#'
#' This function filters the dataframe to include only relevant play descriptions.
#'
#' @param df A dataframe containing play-by-play data.
#' @return A filtered dataframe.
#' @keywords internal
#' @importFrom dplyr filter
filter_relevant_descriptions <- function(df) {
  relevant_descriptions <- c(
    "Ball Up Call", "Bounce", "Centre Bounce", "Contested Knock On", "Contested Mark",
    "Free Advantage", "Free For", "Free For: Before the Bounce", "Free For: In Possession",
    "Free For: Off The Ball", "Gather", "Gather From Hitout", "Gather from Opposition",
    "Ground Kick", "Handball", "Handball Received", "Hard Ball Get", "Hard Ball Get Crumb",
    "Kick", "Knock On", "Loose Ball Get", "Loose Ball Get Crumb", "Mark On Lead",
    "Out of Bounds", "Out On Full After Kick", "Ruck Hard Ball Get", "Uncontested Mark"
  )

  df %>%
    dplyr::filter(.data$description %in% relevant_descriptions) %>%
    dplyr::filter(!(.data$x == -.data$lead_x_tot & .data$y == -.data$lead_y_tot & .data$description != "Centre Bounce"))
}

#' Add Expected Points Value (EPV) Variables
#'
#' This function adds EPV-related variables to the dataframe.
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional EPV variables.
#' @keywords internal
#' @importFrom dplyr mutate lag lead if_else
add_epv_variables <- function(df) {
  df %>%
    dplyr::mutate(
      lag_desc = dplyr::lag(.data$description, default = dplyr::first(.data$description)),
      lead_desc = dplyr::lead(.data$description, default = dplyr::last(.data$description)),
      team_id_mdl = determine_team_id_mdl(.data$throw_in, .data$team_id),
      home = dplyr::if_else(.data$team_id_mdl == .data$home_team_id, 1, 0),
      pos_points = dplyr::if_else(.data$home == 1, .data$home_points, .data$away_points),
      opp_points = dplyr::if_else(.data$home == 1, .data$away_points, .data$home_points),
      points_diff = .data$pos_points - .data$opp_points,
      mirror = calculate_mirror(.data$throw_in, .data$team_id_mdl, .data$x),
      x = .data$mirror * .data$x,
      y = .data$mirror * .data$y,
      goal_x = .data$venue_length / 2 - .data$x
    ) %>%
    add_lagged_variables() %>%
    add_speed_variables()
}

#' Determine Team ID Model
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id A vector of team IDs.
#' @return A vector of determined team IDs.
#' @keywords internal
#' @importFrom dplyr case_when lead
#' @importFrom zoo na.locf0
determine_team_id_mdl <- function(throw_in, team_id) {
  result <- dplyr::case_when(
    throw_in == 1 ~ dplyr::lead(team_id),
    TRUE ~ team_id
  )
  result <- zoo::na.locf0(result, fromLast = TRUE)
  zoo::na.locf0(result)
}

#' Check if current throw-in should be mirrored based on team change
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @return Logical vector indicating mirror condition
#' @keywords internal
is_current_throw_in_team_change <- function(throw_in, team_id_mdl) {
  throw_in == 1 & dplyr::lag(throw_in) != 1 & dplyr::lag(team_id_mdl) != team_id_mdl
}

#' Check if consecutive throw-ins should be mirrored based on team and position
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @param x A vector of x-coordinates.
#' @return Logical vector indicating mirror condition
#' @keywords internal
is_consecutive_throw_in_same_side <- function(throw_in, team_id_mdl, x) {
  (throw_in == 1 & dplyr::lag(throw_in) == 1 & dplyr::lag(team_id_mdl, n = 2L) != team_id_mdl) & 
  sign(dplyr::lag(x)) == sign(x)
}

#' Check if consecutive throw-ins should be mirrored based on position change
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @param x A vector of x-coordinates.
#' @return Logical vector indicating mirror condition
#' @keywords internal
is_consecutive_throw_in_side_change <- function(throw_in, team_id_mdl, x) {
  throw_in == 1 & dplyr::lag(throw_in) == 1 & dplyr::lag(team_id_mdl, n = 2L) == team_id_mdl & 
  sign(dplyr::lag(x)) != sign(x)
}

#' Check if previous throw-in affects current play mirroring
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @param x A vector of x-coordinates.
#' @return Logical vector indicating mirror condition
#' @keywords internal
is_previous_throw_in_affecting <- function(throw_in, team_id_mdl, x) {
  dplyr::lag(throw_in) == 1 & sign(dplyr::lag(x)) == sign(x) & 
  dplyr::lag(team_id_mdl) == team_id_mdl & dplyr::lag(team_id_mdl, n = 2L) != team_id_mdl
}

#' Check if throw-in two plays ago affects current play
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @param x A vector of x-coordinates.
#' @return Logical vector indicating mirror condition
#' @keywords internal
is_throw_in_two_ago_affecting <- function(throw_in, team_id_mdl, x) {
  dplyr::lag(throw_in, n = 2L) == 1 & 
  sign(dplyr::lag(x, n = 2L)) == sign(x) & 
  dplyr::lag(team_id_mdl, n = 2L) == team_id_mdl & 
  dplyr::lag(team_id_mdl, n = 3L) != team_id_mdl
}

#' Check if throw-in affects future position
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @param x A vector of x-coordinates.
#' @return Logical vector indicating mirror condition
#' @keywords internal
is_throw_in_affecting_future_position <- function(throw_in, team_id_mdl, x) {
  dplyr::lag(throw_in) == 1 & 
  sign(dplyr::lead(x, n = 2L)) == sign(x) & 
  dplyr::lead(team_id_mdl, n = 2L) != team_id_mdl
}

#' Check if throw-in two plays ago affects future play
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @param x A vector of x-coordinates.
#' @return Logical vector indicating mirror condition
#' @keywords internal
is_throw_in_two_ago_affecting_future <- function(throw_in, team_id_mdl, x) {
  dplyr::lag(throw_in, n = 2L) == 1 & 
  sign(dplyr::lead(x)) == sign(x) & 
  dplyr::lead(team_id_mdl) != team_id_mdl
}

#' Calculate Mirror Values
#'
#' Determines whether field coordinates should be mirrored based on throw-in patterns
#' and team possession changes. This function handles complex field position adjustments
#' for AFL analytics by breaking down the logic into understandable conditions.
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @param x A vector of x-coordinates.
#' @return A vector of mirror values (-1 or 1).
#' @keywords internal
#' @importFrom dplyr case_when lag lead
calculate_mirror <- function(throw_in, team_id_mdl, x) {
  dplyr::case_when(
    # Current throw-in with team change
    is_current_throw_in_team_change(throw_in, team_id_mdl) ~ -1,
    
    # Consecutive throw-ins on same side with different team
    is_consecutive_throw_in_same_side(throw_in, team_id_mdl, x) ~ -1,
    
    # Consecutive throw-ins with same team but different side
    is_consecutive_throw_in_side_change(throw_in, team_id_mdl, x) ~ -1,
    
    # Previous throw-in affecting current play
    is_previous_throw_in_affecting(throw_in, team_id_mdl, x) ~ -1,
    
    # Throw-in two plays ago affecting current play
    is_throw_in_two_ago_affecting(throw_in, team_id_mdl, x) ~ -1,
    
    # Previous throw-in with future position considerations
    is_throw_in_affecting_future_position(throw_in, team_id_mdl, x) ~ -1,
    
    # Two plays ago throw-in with future considerations
    is_throw_in_two_ago_affecting_future(throw_in, team_id_mdl, x) ~ -1,
    
    # Default case - no mirroring
    TRUE ~ 1
  )
}

#' Add Lagged Variables
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional lagged variables.
#' @keywords internal
#' @importFrom dplyr mutate lag lead
add_lagged_variables <- function(df) {
  df %>%
    dplyr::mutate(
      lag_x = calculate_lagged_coordinate(.data$x, .data$team_id_mdl),
      lag_y = calculate_lagged_coordinate(.data$y, .data$team_id_mdl),
      lag_goal_x = calculate_lagged_goal_x(.data$goal_x, .data$team_id_mdl, .data$venue_length),
      lag_goal_x5 = calculate_lagged_goal_x(.data$goal_x, .data$team_id_mdl, .data$venue_length, lag = 5),
      lag_time5 = dplyr::lag(.data$period_seconds, 5, default = dplyr::first(.data$period_seconds)),
      lag_player = dplyr::lag(.data$player_name, default = dplyr::first(.data$player_name)),
      lead_player = dplyr::lead(.data$player_name, default = dplyr::last(.data$player_name)),
      lead_x = dplyr::lead(.data$x, default = dplyr::last(.data$x)),
      lead_y = dplyr::lead(.data$y, default = dplyr::last(.data$y)),
      lead_goal_x = .data$venue_length / 2 - .data$lead_x
    )
}

#' Calculate Lagged Coordinate
#'
#' @param coord A vector of coordinates.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @return A vector of lagged coordinates.
#' @keywords internal
#' @importFrom dplyr if_else lag
#' @importFrom tidyr replace_na
calculate_lagged_coordinate <- function(coord, team_id_mdl) {
  result <- dplyr::if_else(
    dplyr::lag(team_id_mdl) == team_id_mdl,
    dplyr::lag(coord, default = dplyr::first(coord)),
    -dplyr::lag(coord, default = dplyr::first(coord))
  )
  tidyr::replace_na(result, dplyr::first(coord))
}

#' Calculate Lagged Goal X
#'
#' @param goal_x A vector of goal x-coordinates.
#' @param team_id_mdl A vector of team IDs for modeling.
#' @param venue_length The length of the venue.
#' @param lag The number of positions to lag (default is 1).
#' @return A vector of lagged goal x-coordinates.
#' @keywords internal
#' @importFrom dplyr if_else lag
#' @importFrom tidyr replace_na
calculate_lagged_goal_x <- function(goal_x, team_id_mdl, venue_length, lag = 1) {
  result <- dplyr::if_else(
    dplyr::lag(team_id_mdl, lag) == team_id_mdl,
    dplyr::lag(goal_x, lag, default = dplyr::first(goal_x)),
    venue_length - dplyr::lag(goal_x, lag, default = dplyr::first(goal_x))
  )
  tidyr::replace_na(result, dplyr::first(goal_x))
}

#' Add Speed Variables
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional speed variables.
#' @keywords internal
#' @importFrom dplyr mutate lag
#' @importFrom tidyr replace_na
add_speed_variables <- function(df) {
  df %>%
    dplyr::mutate(
      speed1 = (.data$lag_goal_x - .data$goal_x) / pmax((.data$period_seconds - dplyr::lag(.data$period_seconds)), 1),
      speed1 = tidyr::replace_na(.data$speed1, 0),
      speed5 = (.data$lag_goal_x5 - .data$goal_x) / pmax((.data$period_seconds - .data$lag_time5), 1),
      speed5 = tidyr::replace_na(.data$speed5, 0)
    )
}

#' Calculate Positive Lead Probability
#'
#' @param points_diff The points difference.
#' @param opp_goal Opponent's goal probability.
#' @param opp_behind Opponent's behind probability.
#' @param no_score No score probability.
#' @param behind Behind probability.
#' @param goal Goal probability.
#' @return A numeric value representing the positive lead probability.
#' @keywords internal
#' @importFrom dplyr case_when
calculate_pos_lead_prob <- function(points_diff, opp_goal, opp_behind, no_score, behind, goal) {
  dplyr::case_when(
    points_diff > 6 ~ 1,
    points_diff == 6 ~ (opp_goal * 0.5) + opp_behind + no_score + behind + goal,
    points_diff > 1 ~ (opp_behind * 0.5) + no_score + behind + goal,
    points_diff == 0 ~ (no_score * 0.5) + behind + goal,
    points_diff == -1 ~ (behind * 0.5) + goal,
    points_diff > -6 ~ goal,
    points_diff == -6 ~ (0.5 * goal),
    points_diff < -6 ~ 0
  )
}

#' Add Shot Result Variables
#'
#' @param df A dataframe containing shot data.
#' @return A dataframe with additional shot result variables.
#' @keywords internal
#' @importFrom dplyr mutate case_when if_else
add_shot_result_variables <- function(df) {
  df %>%
    dplyr::mutate(
      shot_result_multi = dplyr::case_when(
        .data$shot_at_goal == TRUE & .data$disposal == "clanger" ~ 0,
        .data$shot_at_goal == TRUE & .data$disposal == "ineffective" ~ 1,
        .data$shot_at_goal == TRUE & .data$disposal == "effective" ~ 2,
        TRUE ~ NA_real_
      ),
      shot_result = as.numeric(dplyr::case_when(
        .data$points_shot == 1 ~ 0,
        .data$points_shot == 6 ~ 1,
        TRUE ~ NA_real_
      )),
      scored_shot = dplyr::if_else(!is.na(.data$shot_at_goal), dplyr::if_else(!is.na(.data$points_shot), 1, 0), NA_real_)
    )
}

#' Add Shot Geometry Variables
#'
#' @param df A dataframe containing shot data.
#' @param goal_width The width of the goal.
#' @return A dataframe with additional shot geometry variables.
#' @keywords internal
#' @importFrom dplyr mutate if_else
add_shot_geometry_variables <- function(df, goal_width) {
  df %>%
    dplyr::mutate(
      abs_y = abs(.data$y),
      side_b = sqrt((.data$goal_x)^2 + (.data$y + goal_width / 2)^2),
      side_c = sqrt((.data$goal_x)^2 + (.data$y - goal_width / 2)^2),
      angle = acos((.data$side_b^2 + .data$side_c^2 - goal_width^2) / (2 * .data$side_b * .data$side_c)),
      distance = dplyr::if_else(.data$y >= -goal_width / 2 & .data$y <= goal_width / 2,
        .data$goal_x, pmin(.data$side_b, .data$side_c)
      )
    )
}

#' Add Shot Type Variables
#'
#' @param df A dataframe containing shot data.
#' @return A dataframe with additional shot type variables.
#' @keywords internal
#' @importFrom dplyr mutate if_else
add_shot_type_variables <- function(df) {
  df %>%
    dplyr::mutate(
      shot_clanger = dplyr::if_else(.data$shot_at_goal == TRUE & .data$disposal == "clanger", 1, 0),
      shot_effective = dplyr::if_else(.data$shot_at_goal == TRUE & .data$disposal == "effective", 1, 0),
      shot_ineffective = dplyr::if_else(.data$shot_at_goal == TRUE & .data$disposal == "ineffective", 1, 0)
    )
}
