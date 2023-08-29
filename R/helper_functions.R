#' Clean PBP
#'
#' @param df
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clean_pbp(df)
#' }
clean_pbp <- function(df) {
  ### JANITOR CLEAN

  # if ("team.teamName" %in% colnames(df)) {
  #   df <-
  #     df %>%
  #     dplyr::rename(
  #       team = team.teamName,
  #       home_team = homeTeam.teamName,
  #       away_team = awayTeam.teamName,
  #     )
  # }

  df <- janitor::clean_names(df)


  ### TOTAL VARIABLE CHANGE
  df <-
    df %>%
    dplyr::mutate(
      torp_match_id = glue::glue("{season}_{round_number}_{home_team_team_abbr}_{away_team_team_abbr}"),
      torp_row_id = paste0(torp_match_id, sprintf("%04d", display_order)),
      torp_match_chain_id = paste0(torp_row_id, chain_number),
      team = dplyr::if_else(team_id == home_team_id, home_team_team_name, away_team_team_name),
      y = -y,
      mirror = dplyr::case_when(
        description == "Spoil" ~ -1,
        !is.na(shot_at_goal) & x < 0 ~ -1,
        TRUE ~ 1
      ),
      home_away = as.factor(dplyr::if_else(team_id == home_team_id, "Home", "Away")),
      goal_x = venue_length / 2 - x,
      throw_in = dplyr::case_when(
        description == "Centre Bounce" ~ 1,
        description == "Out of Bounds" ~ 1,
        description == "Ball Up Call" ~ 1,
        TRUE ~ 0,
      ),
      shot_row = dplyr::if_else(is.na(shot_at_goal), 0, 1),
      player_position_fac = forcats::fct_na_value_to_level(player_position, level = "Other"),
      player_name = forcats::fct_na_value_to_level(paste(player_name_given_name, player_name_surname), level = "Other")
    )

  ### GAME VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id) %>%
    dplyr::mutate(
      rn = dplyr::row_number(),
      team_id_mdl = dplyr::case_when(
        throw_in == 1 ~ dplyr::lead(team_id),
        TRUE ~ team_id
      ),
      team_id_mdl = zoo::na.locf0(team_id_mdl, fromLast = TRUE),
      team_id_mdl = zoo::na.locf0(team_id_mdl),
      home = dplyr::if_else(team_id_mdl == home_team_id, 1, 0),
      total_seconds = ifelse(period == 1, period_seconds,
        ifelse(period == 2, 1800 + period_seconds,
          ifelse(period == 3, 3600 + period_seconds,
            ifelse(period == 4, 5400 + period_seconds, NA_integer_)
          )
        )
      ),
      ### not sure about top 4 decide later
      points_row = dplyr::case_when(
        chain_number != dplyr::lead(chain_number, default = (dplyr::last(chain_number) + 1)) &
          (final_state == "rushed" | final_state == "rushedOpp") ~ 1,
        description == "Behind" ~ 1,
        description == "Goal" ~ 6,
        TRUE ~ 0
      ),
      points_row = tidyr::replace_na(points_row, 0),
      points_row_na = dplyr::if_else(points_row == 0, NA_real_, points_row),
      # points_row_lead = lead(points_row, default = ?points_shot? ),
      home_points_row = dplyr::case_when(
        home == 0 & (final_state == "rushed" | final_state == "rushedOpp") & description == "Spoil" ~ points_row,
        dplyr::lead(home) == 0 & (final_state == "rushed" | final_state == "rushedOpp") & dplyr::lead(description) == "Kickin play on" ~ points_row,
        home == 1 & (final_state != "rushed" & final_state != "rushedOpp") ~ points_row,
        TRUE ~ 0
      ),
      home_points_row = tidyr::replace_na(home_points_row, 0),
      away_points_row = points_row - home_points_row,
      away_points_row = tidyr::replace_na(away_points_row, 0),
      is_goal_row = dplyr::if_else(description == "Goal", 1, 0),
      is_behind_row = dplyr::if_else(home_points_row == 1 | away_points_row == 1, 1, 0),
      tot_goals = cumsum(is_goal_row),
      scoring_team_id = dplyr::if_else(points_row != 0, team_id, NA_character_),
      home_points = cumsum(home_points_row),
      away_points = cumsum(away_points_row),
      pos_points = dplyr::if_else(home == 1, home_points, away_points),
      opp_points = dplyr::if_else(home == 1, away_points, home_points),
      points_diff = pos_points - opp_points,
      # points_shot = dplyr::case_when(
      #   shot_at_goal == TRUE & lead_desc_tot == "Goal" ~ 6,
      #   shot_at_goal == TRUE & lead_desc_tot == "Behind" ~ 1,
      #   shot_at_goal == TRUE & dplyr::lead(lead_desc_tot) == "Spoil" ~ 1,
      #   shot_at_goal == TRUE & dplyr::lead(description, 3) == "Goal" & chain_number == dplyr::lead(chain_number, 3) ~ 6,
      #   shot_at_goal == TRUE & dplyr::lead(description, 3) == "Behind" & chain_number == dplyr::lead(chain_number, 3) ~ 1,
      #   TRUE ~ NA_real_
      # )
    ) %>%
    dplyr::ungroup()

  ### QUARTER VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id, period) %>%
    dplyr::mutate(
      lag_desc_tot = dplyr::lag(description, default = "Start of Quarter"),
      ################## MAYBE CHANGE PHASE OF PLAY TO BE A LOOKUP
      phase_of_play =
        as.factor(dplyr::case_when(
          throw_in == 1 ~ "Hard Ball",
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
        )),
      ### DO I NEED THIS TO BE A FACTOR MAYBE NOT
      play_type = as.factor(
        dplyr::case_when(
          description == "Handball" ~ "Handball",
          description == "Kick" ~ "Kick",
          description == "Ground Kick" ~ "Ground Kick",
          TRUE ~ "Reception"
        )
      ),
      ### CHECK WHERE I NEED ALL THE LAGS AND LEADS
      lag_x_tot = (dplyr::lag(x)),
      lag_goal_x_tot = (dplyr::lag(goal_x)),
      lag_y_tot = (dplyr::lag(y)),
      lag_desc_tot = dplyr::lead(description),
      ######### DO LEAD LATER
      # lead_x_tot = (dplyr::lead(x)),
      lead_goal_x_tot = (dplyr::lead(goal_x)),
      # lead_y_tot = (dplyr::lead(y)),
      lead_desc_tot = dplyr::lead(description),
      rn_qtr = dplyr::row_number(),
      pos_points = zoo::na.locf0(points_row_na, fromLast = TRUE),
      pos_points_team_id = zoo::na.locf0(scoring_team_id, fromLast = TRUE),
      model_points = dplyr::if_else(pos_points_team_id == team_id_mdl, pos_points, -pos_points),
      pos_is_goal = dplyr::if_else(pos_points == 6, 1, 0),
      pos_team_shot = dplyr::if_else(model_points > 0, 1, 0),
      is_shot = dplyr::if_else(!is.na(model_points), 1, 0),
      next_score = dplyr::case_when(
        model_points == -6 ~ 0,
        model_points == -1 ~ 1,
        model_points == 1 ~ 2,
        model_points == 6 ~ 3,
        TRUE ~ 4
      ),
      label_ep = as.numeric(next_score)
    ) %>%
    dplyr::ungroup()



  ### GOAL/SCORE VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id, period, tot_goals) %>%
    dplyr::mutate(
      label_wp = dplyr::case_when(
        home_team_score_total_score > away_team_score_total_score & home == 1 ~ 1,
        home_team_score_total_score == away_team_score_total_score & home == 1 ~ 0.5,
        home_team_score_total_score < away_team_score_total_score & home == 1 ~ 0,
        home_team_score_total_score > away_team_score_total_score & home == 0 ~ 0,
        home_team_score_total_score == away_team_score_total_score & home == 0 ~ 0.5,
        home_team_score_total_score < away_team_score_total_score & home == 0 ~ 1
      ),
      lead_x_tot = dplyr::lead(x, default = dplyr::last(x)), ##### THINK DEFAULT LAST/FIRST IS SLOW, CHECK THIS
      lead_y_tot = dplyr::lead(y, default = dplyr::last(y)) ##### THINK DEFAULT LAST/FIRST IS SLOW, CHECK THIS
      # lead_goal_x = venue_length / 2 - lead_x,
      # lag_model_desc = dplyr::lag(model_desc, default = "Centre Bounce")
    ) %>%
    dplyr::ungroup()


  ### CHAIN NUMBER VARIABLE CHANGE
  df <-
    df %>%
    dplyr::group_by(match_id, chain_number) %>%
    dplyr::mutate(
      shot_display = dplyr::if_else(is.na(shot_at_goal),
        dplyr::if_else(description == "Kick" | description == "Ground Kick", as.numeric(display_order), display_order / 2),
        as.numeric(display_order)
      ),
      max_shot_display = max(shot_display),
      points_shot = dplyr::if_else(shot_display == max_shot_display,
        dplyr::case_when(
          final_state == "rushed" ~ 1,
          final_state == "rushedOpp" ~ 1,
          final_state == "behind" ~ 1,
          final_state == "goal" ~ 6,
          TRUE ~ NA_real_
        ),
        NA_real_
      )
    ) %>%
    dplyr::ungroup()

  return(df)
}
