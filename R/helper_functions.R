#' Clean Play-by-Play Data
#'
#' This function cleans and processes play-by-play data for AFL matches.
#'
#' @param df A dataframe containing raw play-by-play data.
#'
#' @return A cleaned and processed dataframe with additional variables.
#' @export
#'
#' @examples
#' \dontrun{
#' cleaned_data <- clean_pbp(raw_pbp_data)
#' }
#'
#' @importFrom dplyr mutate group_by ungroup row_number case_when if_else lead lag
#' @importFrom tidyr replace_na
#' @importFrom forcats fct_na_value_to_level
#' @importFrom stringr str_starts str_detect
#' @importFrom zoo na.locf0
#' @importFrom glue glue
#' @importFrom janitor clean_names
clean_pbp <- function(df) {
  df <- janitor::clean_names(df)

  df <- add_torp_ids(df)
  df <- add_basic_variables(df)
  df <- add_chain_variables(df)
  df <- add_quarter_variables(df)
  df <- add_game_variables(df)
  df <- add_score_variables(df)

  return(df)
}

#' Add TORP IDs
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional TORP ID columns.
#' @keywords internal
add_torp_ids <- function(df) {
  df %>%
    dplyr::mutate(
      torp_match_id = glue::glue("{.data$season}_{.data$round_number}_{.data$home_team_team_abbr}_{.data$away_team_team_abbr}"),
      torp_row_id = paste0(.data$torp_match_id, sprintf("%04d", .data$display_order)),
      torp_match_chain_id = paste0(.data$torp_row_id, .data$chain_number)
    )
}

#' Add Basic Variables
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional basic variables.
#' @keywords internal
add_basic_variables <- function(df) {
  df %>%
    dplyr::mutate(
      team = dplyr::if_else(.data$team_id == .data$home_team_id, .data$home_team_team_name, .data$away_team_team_name),
      opp_id = dplyr::if_else(.data$team_id == .data$home_team_id, .data$away_team_id, .data$home_team_id),
      y = -.data$y,
      mirror = dplyr::case_when(
        .data$description == "Spoil" ~ -1,
        !is.na(.data$shot_at_goal) & .data$x < 0 ~ -1,
        TRUE ~ 1
      ),
      home_away = as.factor(dplyr::if_else(.data$team_id == .data$home_team_id, "Home", "Away")),
      goal_x = .data$venue_length / 2 - .data$x,
      throw_in = dplyr::case_when(
        .data$description %in% c("Centre Bounce", "Out of Bounds", "Ball Up Call") ~ 1,
        TRUE ~ 0
      ),
      shot_row = dplyr::if_else(is.na(.data$shot_at_goal), 0, 1),
      player_position_fac = forcats::fct_na_value_to_level(.data$player_position, level = "Other"),
      player_name = forcats::fct_na_value_to_level(paste(.data$player_name_given_name, .data$player_name_surname), level = "Other")
    )
}

#' Add Game Variables
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional game-level variables.
#' @keywords internal
add_game_variables <- function(df) {
  df %>%
    dplyr::group_by(.data$match_id) %>%
    dplyr::mutate(
      home_points = cumsum(.data$home_points_row),
      away_points = cumsum(.data$away_points_row),
      rn = dplyr::row_number(),
      total_seconds = (.data$period - 1) * 1800 + .data$period_seconds,
      model_points = dplyr::if_else(.data$pos_points_team_id == .data$team_id_mdl, .data$pos_points, -.data$pos_points),
      pos_is_goal = dplyr::if_else(.data$pos_points == 6, 1, 0),
      pos_team_shot = dplyr::if_else(.data$model_points > 0, 1, 0),
      is_shot = dplyr::if_else(!is.na(.data$model_points), 1, 0),
      next_score = dplyr::case_when(
        .data$model_points == -6 ~ 0,
        .data$model_points == -1 ~ 1,
        .data$model_points == 1 ~ 2,
        .data$model_points == 6 ~ 3,
        TRUE ~ 4
      ),
      label_ep = as.numeric(.data$next_score)
    ) %>%
    dplyr::ungroup()
}

#' Add Quarter Variables
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional quarter-level variables.
#' @keywords internal
add_quarter_variables <- function(df) {
  df %>%
    dplyr::group_by(.data$match_id, .data$period) %>%
    dplyr::mutate(
      end_of_qtr = ifelse(max(.data$display_order) == .data$display_order, 1, 0),
      points_row = calculate_points_row(.data$final_state, .data$description, .data$end_of_chain, .data$end_of_qtr),
      team_id_mdl = dplyr::case_when(
        .data$throw_in == 1 ~ dplyr::lead(.data$team_id),
        TRUE ~ .data$team_id
      ),
      team_id_mdl = zoo::na.locf0(.data$team_id_mdl, fromLast = TRUE),
      team_id_mdl = zoo::na.locf0(.data$team_id_mdl),
      home = dplyr::if_else(.data$team_id_mdl == .data$home_team_id, 1, 0),
      is_goal_row = dplyr::if_else(.data$description == "Goal", 1, 0),
      tot_goals = cumsum(.data$is_goal_row),
      scoring_team_id =
        ifelse(.data$end_of_chain == 1,
          dplyr::case_when(
            .data$final_state == "behind" ~ .data$team_id,
            .data$final_state == "goal" ~ .data$team_id,
            .data$final_state == "rushed" & .data$description == "" ~ .data$team_id,
            .data$final_state == "rushed" & .data$description == "Spoil" ~ .data$team_id,
            .data$final_state == "rushedOpp" ~ .data$opp_id,
            TRUE ~ NA_character_
          ),
          NA_character_
        ),
      points_team_id = calculate_points_id(.data$final_state, .data$description, .data$end_of_chain, .data$end_of_qtr, .data$team_id, .data$opp_id),
      home_points_row = tidyr::replace_na(ifelse(points_team_id == home_team_id, points_row, NA_integer_), 0),
      away_points_row = tidyr::replace_na(.data$points_row - .data$home_points_row, 0),
      # is_behind_row = dplyr::if_else(.data$home_points_row == 1 | .data$away_points_row == 1, 1, 0),
      # pos_points = dplyr::if_else(.data$home == 1, .data$home_points, .data$away_points),
      # opp_points = dplyr::if_else(.data$home == 1, .data$away_points, .data$home_points),
      # points_diff = .data$pos_points - .data$opp_points,
      lag_desc_tot = dplyr::lag(.data$description, default = "Start of Quarter"),
      phase_of_play = determine_phase_of_play(.data$lag_desc_tot, .data$description, .data$throw_in),
      play_type = determine_play_type(.data$description),
      lag_x_tot = dplyr::lag(.data$x),
      lag_goal_x_tot = dplyr::lag(.data$goal_x),
      lag_y_tot = dplyr::lag(.data$y),
      lead_desc_tot = dplyr::lead(.data$description),
      lead_goal_x_tot = dplyr::lead(.data$goal_x),
      rn_qtr = dplyr::row_number(),
      pos_points = zoo::na.locf0(.data$points_row, fromLast = TRUE),
      pos_points_team_id = zoo::na.locf0(.data$scoring_team_id, fromLast = TRUE)
    ) %>%
    dplyr::ungroup()
}

#' Add Score Variables
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional score-related variables.
#' @keywords internal
add_score_variables <- function(df) {
  df %>%
    dplyr::group_by(.data$match_id, .data$period, .data$tot_goals) %>%
    dplyr::mutate(
      label_wp = calculate_label_wp(.data$home_team_score_total_score, .data$away_team_score_total_score, .data$home),
      lead_x_tot = dplyr::lead(.data$x, default = dplyr::last(.data$x)),
      lead_y_tot = dplyr::lead(.data$y, default = dplyr::last(.data$y))
    ) %>%
    dplyr::ungroup()
}

#' Add Chain Variables
#'
#' @param df A dataframe containing play-by-play data.
#' @return A dataframe with additional chain-related variables.
#' @keywords internal
add_chain_variables <- function(df) {
  df %>%
    dplyr::group_by(.data$match_id, .data$chain_number) %>%
    # tidyr::fill(team_id,.direction = "up") %>%
    dplyr::mutate(
      # chain_team_id = dplyr::first(team_id),
      ### don't love this
      # chain_team_id = tidyr::replace_na(.data$chain_team_id, max(.data$home_team_id)),
      end_of_chain = ifelse(max(.data$display_order) == .data$display_order, 1, 0),
      shot_display = dplyr::if_else(is.na(.data$shot_at_goal),
        dplyr::if_else(.data$description %in% c("Kick", "Ground Kick"), as.numeric(.data$display_order), .data$display_order / 2),
        as.numeric(.data$display_order)
      ),
      max_shot_display = max(.data$shot_display),
      points_shot = dplyr::if_else(.data$shot_display == .data$max_shot_display,
        dplyr::case_when(
          .data$final_state %in% c("rushed", "rushedOpp", "behind") ~ 1,
          .data$final_state == "goal" ~ 6,
          TRUE ~ NA_real_
        ),
        NA_real_
      )
    ) %>%
    dplyr::ungroup()
}

#' Calculate Points Row
#'
#' @param final_state The final state of the play.
#' @param description The description of the play.
#' @return A numeric value representing the points for the row.
#' @keywords internal
calculate_points_row <- function(final_state, description, end_of_chain, end_of_qtr) {
  dplyr::case_when(
    final_state %in% c("rushed", "rushedOpp", "behind") & end_of_chain == 1 ~ 1,
    final_state %in% c("goal") & end_of_chain == 1 ~ 6,
    TRUE ~ 0
  )
}

#' Calculate Home Points Row
#'
#' @param home Whether it's the home team (1) or away team (0).
#' @param final_state The final state of the play.
#' @param description The description of the play.
#' @param points_row The points for the row.
#' @return A numeric value representing the home team's points for the row.
#' @keywords internal
calculate_points_id <- function(final_state, description, end_of_chain, end_of_qtr, team_id, opp_id) {
  dplyr::case_when(
    # home == 0 & final_state %in% c("rushed", "rushedOpp") & description == "Spoil" ~ points_row,
    # dplyr::lead(home) == 0 & final_state %in% c("rushed", "rushedOpp") & dplyr::lead(description) == "Kickin play on" ~ points_row,
    # home == 1 & !final_state %in% c("rushed", "rushedOpp") ~ points_row,
    description == "Goal" ~ team_id,
    description == "Behind" ~ team_id,
    final_state %in% c("rushed", "rushedOpp") & end_of_qtr == 0 ~ lead(opp_id, default = NA_character_),
    final_state %in% c("rushed", "rushedOpp") & end_of_qtr == 1 & !is.na(opp_id) ~ opp_id,
    final_state %in% c("rushed", "rushedOpp") & end_of_qtr == 1 & is.na(opp_id) ~ lag(opp_id, default = NA_character_),
    TRUE ~ NA_character_
  )
}

#' Determine Phase of Play
#'
#' @param lag_desc_tot The lagged description of the play.
#' @param description The current description of the play.
#' @param throw_in Whether it's a throw-in.
#' @return A factor representing the phase of play.
#' @keywords internal
determine_phase_of_play <- function(lag_desc_tot, description, throw_in) {
  as.factor(dplyr::case_when(
    throw_in == 1 ~ "Hard Ball",
    stringr::str_starts(lag_desc_tot, "Free") | stringr::str_starts(lag_desc_tot, "OOF") |
      stringr::str_starts(lag_desc_tot, "Out on") | stringr::str_detect(lag_desc_tot, "ted Mark") |
      stringr::str_detect(lag_desc_tot, "Mark On") ~ "Set Shot",
    stringr::str_starts(description, "Free") | stringr::str_detect(description, "ted Mark") |
      stringr::str_detect(description, "Mark On") ~ "Set Shot",
    stringr::str_starts(lag_desc_tot, "Loose Ball") | stringr::str_starts(description, "Loose Ball") ~ "Loose Ball",
    stringr::str_starts(description, "Contested Knock On") ~ "Hard Ball",
    stringr::str_detect(lag_desc_tot, "Hard Ball") | stringr::str_detect(description, "Hard Ball") ~ "Hard Ball",
    stringr::str_detect(lag_desc_tot, "Handball") ~ "Handball Received",
    TRUE ~ "Hard Ball"
  ))
}

#' Determine Play Type
#'
#' @param description The description of the play.
#' @return A factor representing the play type.
#' @keywords internal
determine_play_type <- function(description) {
  as.factor(dplyr::case_when(
    description == "Handball" ~ "Handball",
    description == "Kick" ~ "Kick",
    description == "Ground Kick" ~ "Ground Kick",
    TRUE ~ "Reception"
  ))
}

#' Calculate Label Win Probability
#'
#' @param home_score The home team's score.
#' @param away_score The away team's score.
#' @param home Whether it's the home team (1) or away team (0).
#' @return A numeric value representing the win probability label.
#' @keywords internal
calculate_label_wp <- function(home_score, away_score, home) {
  dplyr::case_when(
    home_score > away_score & home == 1 ~ 1,
    home_score == away_score & home == 1 ~ 0.5,
    home_score < away_score & home == 1 ~ 0,
    home_score > away_score & home == 0 ~ 0,
    home_score == away_score & home == 0 ~ 0.5,
    home_score < away_score & home == 0 ~ 1
  )
}
