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
#' chains <- load_chains(2024, rounds = 1)
#' cleaned_data <- clean_pbp(chains)
#' }
#'
#' @importFrom dplyr mutate group_by ungroup row_number case_when if_else lead lag
#' @importFrom tidyr replace_na
#' @importFrom forcats fct_na_value_to_level
#' @importFrom stringr str_starts str_detect
#' @importFrom data.table nafill setnafill as.data.table setDT setorder fifelse fcase shift copy
#' @importFrom glue glue
#' @importFrom janitor clean_names
clean_pbp <- function(df) {
  # Use optimized data.table version
  clean_pbp_dt(df)
}

#' Clean Play-by-Play Data (data.table optimized)
#'
#' Optimized version of clean_pbp using data.table for better performance.
#' Consolidates all variable additions into fewer passes over the data.
#'
#' @param df A dataframe containing raw play-by-play data.
#' @return A cleaned and processed data.table with additional variables.
#' @keywords internal
#' @importFrom data.table as.data.table setDT setorder setkey fifelse fcase shift nafill copy
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom forcats fct_na_value_to_level
#' @importFrom stringr str_starts str_detect
clean_pbp_dt <- function(df) {
  # Clean names and convert to data.table in one step (avoid extra copy)
  dt <- data.table::as.data.table(janitor::clean_names(df))

  # Set key for efficient grouping operations
  data.table::setkey(dt, match_id)

  # Apply transformations in sequence (each modifies dt by reference)
  add_torp_ids_dt(dt)
  add_chain_vars_dt(dt)
  add_quarter_vars_dt(dt)
  add_game_vars_dt(dt)
  add_score_vars_dt(dt)

  return(dt)
}

#' Add TORP IDs and basic variables (Pass 1)
#'
#' Adds TORP identifiers and basic computed variables to the data.table.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_torp_ids_dt <- function(dt) {
  dt[, `:=`(
    # TORP IDs (use paste0 instead of glue for data.table compatibility)
    torp_match_id = paste0(season, "_", round_number, "_", home_team_team_abbr, "_", away_team_team_abbr),
    torp_row_id = paste0(
      season, "_", round_number, "_", home_team_team_abbr, "_", away_team_team_abbr,
      sprintf("%04d", display_order)
    ),
    # Basic variables
    team = data.table::fifelse(team_id == home_team_id, home_team_team_name, away_team_team_name),
    opp_id = data.table::fifelse(team_id == home_team_id, away_team_id, home_team_id),
    y = -y,
    mirror = data.table::fcase(
      description == "Spoil", -1L,
      !is.na(shot_at_goal) & x < 0, -1L,
      default = 1L
    ),
    home_away = factor(data.table::fifelse(team_id == home_team_id, "Home", "Away")),
    goal_x = venue_length / 2 - x,
    throw_in = data.table::fcase(
      description %in% c("Centre Bounce", "Out of Bounds", "Ball Up Call"), 1L,
      default = 0L
    ),
    shot_row = data.table::fifelse(is.na(shot_at_goal), 0L, 1L),
    player_position_fac = forcats::fct_na_value_to_level(player_position, level = "Other"),
    player_name = forcats::fct_na_value_to_level(
      paste(player_name_given_name, player_name_surname), level = "Other"
    )
  )]
  invisible(NULL)
}

#' Add chain variables (Pass 2)
#'
#' Adds chain-level variables grouped by match_id and chain_number.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_chain_vars_dt <- function(dt) {
  dt[, `:=`(
    end_of_chain = data.table::fifelse(max(display_order) == display_order, 1L, 0L),
    shot_display = data.table::fifelse(
      is.na(shot_at_goal),
      data.table::fifelse(description %in% c("Kick", "Ground Kick"), as.numeric(display_order), display_order / 2),
      as.numeric(display_order)
    )
  ), by = .(match_id, chain_number)]

  dt[, max_shot_display := max(shot_display), by = .(match_id, chain_number)]

  dt[, points_shot := data.table::fifelse(
    shot_display == max_shot_display,
    data.table::fcase(
      final_state %in% c("rushed", "rushedOpp", "behind"), 1,
      final_state == "goal", 6,
      default = NA_real_
    ),
    NA_real_
  )]
  invisible(NULL)
}

#' Add quarter variables (Pass 3)
#'
#' Adds quarter-level variables grouped by match_id and period.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_quarter_vars_dt <- function(dt) {
  dt[, end_of_qtr := data.table::fifelse(max(display_order) == display_order, 1L, 0L),
     by = .(match_id, period)]

  dt[, points_row := data.table::fcase(
    final_state %in% c("rushed", "rushedOpp", "behind") & end_of_chain == 1L, 1L,
    final_state == "goal" & end_of_chain == 1L, 6L,
    default = NA_integer_
  )]

  dt[, points_row_na := data.table::fifelse(is.na(points_row), 0L, points_row)]

  # team_id_mdl: use lead for throw_in, then fill NAs
  dt[, team_id_mdl := data.table::fcase(
    throw_in == 1L, data.table::shift(team_id, n = 1L, type = "lead"),
    default = team_id
  ), by = .(match_id, period)]

  # Fill NAs in team_id_mdl (character column - use nafill_char)
  dt[, team_id_mdl := nafill_char(team_id_mdl, type = "nocb"), by = .(match_id, period)]
  dt[, team_id_mdl := nafill_char(team_id_mdl, type = "locf"), by = .(match_id, period)]

  dt[, `:=`(
    home = data.table::fifelse(team_id_mdl == home_team_id, 1L, 0L),
    is_goal_row = data.table::fifelse(description == "Goal", 1L, 0L)
  )]

  dt[, tot_goals := cumsum(is_goal_row), by = .(match_id, period)]

  # scoring_team_id
  dt[, scoring_team_id := data.table::fifelse(
    end_of_chain == 1L,
    data.table::fcase(
      final_state == "behind", team_id,
      final_state == "goal", team_id,
      final_state == "rushed" & description == "", team_id,
      final_state == "rushed" & description == "Spoil", team_id,
      final_state == "rushedOpp", opp_id,
      default = NA_character_
    ),
    NA_character_
  )]

  # points_team_id calculation
  dt[, lead_opp_id := data.table::shift(opp_id, n = 1L, type = "lead"), by = .(match_id, period)]
  dt[, lag_opp_id := data.table::shift(opp_id, n = 1L, type = "lag"), by = .(match_id, period)]

  dt[, points_team_id := data.table::fcase(
    description == "Goal", team_id,
    description == "Behind", team_id,
    final_state %in% c("rushed", "rushedOpp") & end_of_qtr == 0L, lead_opp_id,
    final_state %in% c("rushed", "rushedOpp") & end_of_qtr == 1L & !is.na(opp_id), opp_id,
    final_state %in% c("rushed", "rushedOpp") & end_of_qtr == 1L & is.na(opp_id), lag_opp_id,
    default = NA_character_
  )]

  # Clean up temp columns
  dt[, c("lead_opp_id", "lag_opp_id") := NULL]

  dt[, `:=`(
    home_points_row = data.table::fifelse(
      is.na(data.table::fifelse(points_team_id == home_team_id, points_row, NA_integer_)),
      0L,
      data.table::fifelse(points_team_id == home_team_id, points_row, NA_integer_)
    ),
    away_points_row = data.table::fifelse(
      is.na(points_row - data.table::fifelse(points_team_id == home_team_id, points_row, 0L)),
      0L,
      points_row - data.table::fifelse(points_team_id == home_team_id, points_row, 0L)
    )
  )]

  # Lag/lead descriptions and coordinates
  dt[, `:=`(
    lag_desc_tot = data.table::shift(description, n = 1L, type = "lag", fill = "Start of Quarter"),
    lag_x_tot = data.table::shift(x, n = 1L, type = "lag"),
    lag_goal_x_tot = data.table::shift(goal_x, n = 1L, type = "lag"),
    lag_y_tot = data.table::shift(y, n = 1L, type = "lag"),
    lead_desc_tot = data.table::shift(description, n = 1L, type = "lead"),
    lead_goal_x_tot = data.table::shift(goal_x, n = 1L, type = "lead"),
    rn_qtr = seq_len(.N)
  ), by = .(match_id, period)]

  # Phase of play and play type
  dt[, phase_of_play := factor(data.table::fcase(
    throw_in == 1L, "Hard Ball",
    stringr::str_starts(lag_desc_tot, "Free") | stringr::str_starts(lag_desc_tot, "OOF") |
      stringr::str_starts(lag_desc_tot, "Out on") | stringr::str_detect(lag_desc_tot, "ted Mark") |
      stringr::str_detect(lag_desc_tot, "Mark On"), "Set Shot",
    stringr::str_starts(description, "Free") | stringr::str_detect(description, "ted Mark") |
      stringr::str_detect(description, "Mark On"), "Set Shot",
    stringr::str_starts(lag_desc_tot, "Loose Ball") | stringr::str_starts(description, "Loose Ball"), "Loose Ball",
    stringr::str_starts(description, "Contested Knock On"), "Hard Ball",
    stringr::str_detect(lag_desc_tot, "Hard Ball") | stringr::str_detect(description, "Hard Ball"), "Hard Ball",
    stringr::str_detect(lag_desc_tot, "Handball"), "Handball Received",
    default = "Hard Ball"
  ))]

  dt[, play_type := factor(data.table::fcase(
    description == "Handball", "Handball",
    description == "Kick", "Kick",
    description == "Ground Kick", "Ground Kick",
    default = "Reception"
  ))]

  # pos_points and pos_points_team_id (NOCB fill)
  dt[, pos_points := data.table::nafill(points_row, type = "nocb"), by = .(match_id, period)]
  dt[, pos_points_team_id := nafill_char(scoring_team_id, type = "nocb"), by = .(match_id, period)]

  invisible(NULL)
}

#' Add game variables (Pass 4)
#'
#' Adds game-level variables grouped by match_id.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_game_vars_dt <- function(dt) {
  dt[, `:=`(
    home_points = cumsum(home_points_row),
    away_points = cumsum(away_points_row),
    rn = seq_len(.N)
  ), by = match_id]

  dt[, `:=`(
    pos_team_points = data.table::fifelse(home == 1L, home_points, away_points),
    opp_team_points = data.table::fifelse(home == 1L, away_points, home_points),
    total_seconds = (period - 1L) * 1800L + period_seconds
  )]

  dt[, points_diff := pos_team_points - opp_team_points]

  dt[, model_points := data.table::fifelse(pos_points_team_id == team_id_mdl, pos_points, -pos_points)]

  dt[, `:=`(
    pos_is_goal = data.table::fifelse(pos_points == 6L, 1L, 0L),
    pos_team_shot = data.table::fifelse(model_points > 0, 1L, 0L),
    is_shot = data.table::fifelse(!is.na(model_points), 1L, 0L),
    next_score = data.table::fcase(
      model_points == -6L, 0L,
      model_points == -1L, 1L,
      model_points == 1L, 2L,
      model_points == 6L, 3L,
      default = 4L
    )
  )]

  dt[, label_ep := as.numeric(next_score)]

  invisible(NULL)
}

#' Add score variables (Pass 5)
#'
#' Adds final score-related variables grouped by match_id, period, and tot_goals.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_score_vars_dt <- function(dt) {
  dt[, label_wp := data.table::fcase(
    home_team_score_total_score > away_team_score_total_score & home == 1L, 1,
    home_team_score_total_score == away_team_score_total_score & home == 1L, 0.5,
    home_team_score_total_score < away_team_score_total_score & home == 1L, 0,
    home_team_score_total_score > away_team_score_total_score & home == 0L, 0,
    home_team_score_total_score == away_team_score_total_score & home == 0L, 0.5,
    home_team_score_total_score < away_team_score_total_score & home == 0L, 1,
    default = NA_real_
  )]

  # lead_x_tot and lead_y_tot with last value as default
  dt[, `:=`(
    lead_x_tot = data.table::fifelse(
      is.na(data.table::shift(x, n = 1L, type = "lead")),
      x[.N],
      data.table::shift(x, n = 1L, type = "lead")
    ),
    lead_y_tot = data.table::fifelse(
      is.na(data.table::shift(y, n = 1L, type = "lead")),
      y[.N],
      data.table::shift(y, n = 1L, type = "lead")
    )
  ), by = .(match_id, period, tot_goals)]

  invisible(NULL)
}

#' Fill NA values in character vectors
#'
#' A helper function to fill NA values in character vectors using LOCF or NOCB.
#' This provides similar functionality to zoo::na.locf0 but is optimized for use
#' within dplyr pipelines.
#'
#' @param x A character vector with potential NA values.
#' @param type Fill type: "locf" (last observation carried forward) or
#'   "nocb" (next observation carried backward).
#' @return A character vector with NA values filled.
#' @keywords internal
nafill_char <- function(x, type = "locf") {
  if (type == "locf") {
    # Last observation carried forward
    idx <- cummax(seq_along(x) * (!is.na(x)))
    idx[idx == 0] <- NA_integer_
    x[idx]
  } else if (type == "nocb") {
    # Next observation carried backward (reverse LOCF)
    n <- length(x)
    rev_idx <- cummax(seq_along(x) * (!is.na(rev(x))))
    rev_idx[rev_idx == 0] <- NA_integer_
    rev(rev(x)[rev_idx])
  } else {
    cli::cli_abort("type must be 'locf' or 'nocb'")
  }
}
