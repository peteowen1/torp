# Note: AFL constants are defined in R/constants.R

#' Clean Model Data for Expected Points Value (EPV)
#'
#' @param df A dataframe containing raw play-by-play data.
#' @return A cleaned dataframe ready for EPV modeling.
#' @keywords internal
clean_model_data_epv <- function(df) {
  clean_model_data_epv_dt(df)
}

#' Clean Model Data for EPV (data.table optimized)
#'
#' Optimized version using data.table shift() with by-reference helpers.
#' Follows the same pattern as clean_pbp() -> clean_pbp_dt().
#'
#' @param df A dataframe containing cleaned play-by-play data from clean_pbp().
#' @return A data.table ready for EPV modeling.
#' @keywords internal
#' @importFrom data.table as.data.table shift fifelse fcase
clean_model_data_epv_dt <- function(df) {
  dt <- data.table::as.data.table(df)
  grp <- c("match_id", "period", "tot_goals")

  # Filter relevant descriptions (vectorized, no grouping needed)
  dt <- dt[description %in% EPV_RELEVANT_DESCRIPTIONS]
  tol <- .Machine$double.eps^0.5
  dt <- dt[!(abs(x - (-lead_x_tot)) < tol & abs(y - (-lead_y_tot)) < tol & description != "Centre Bounce")]

  # Grouped throw_in filter via shift
  dt[, c("lag_ti_flt", "lead_ti_flt") := .(
    data.table::shift(throw_in, 1L, type = "lag"),
    data.table::shift(throw_in, 1L, type = "lead")
  ), by = grp]
  dt <- dt[lag_ti_flt == 0L | lead_ti_flt == 0L | throw_in == 0L]
  dt[, c("lag_ti_flt", "lead_ti_flt") := NULL]

  # Team vars, mirror, coordinate transform
  add_epv_team_vars_dt(dt, grp)

  # Lagged variables + speed
  add_epv_lag_vars_dt(dt, grp)

  # Dummy columns + clean names
  dt <- torp_dummy_cols(dt, select_columns = c("play_type", "phase_of_play"))
  torp_clean_names(dt)
}

#' Add EPV team variables (data.table, by reference)
#'
#' Computes team_id_mdl, home, points, mirror, and coordinate transform.
#' All shift operations for the mirror calculation are batched into a single
#' := call for performance.
#'
#' @param dt A data.table to modify by reference.
#' @param grp Character vector of grouping columns.
#' @return Invisible NULL (modifies dt by reference).
#' @keywords internal
add_epv_team_vars_dt <- function(dt, grp) {
  # Recompute team_id_mdl (rows removed by filtering change lag relationships)
  dt[, team_id_mdl := data.table::fcase(
    throw_in == 1L, data.table::shift(team_id, 1L, type = "lead"),
    default = team_id
  ), by = grp]
  dt[, team_id_mdl := nafill_char(team_id_mdl, "nocb"), by = grp]
  dt[, team_id_mdl := nafill_char(team_id_mdl, "locf"), by = grp]

  # home, pos_points, opp_points, points_diff
  dt[, `:=`(
    home = data.table::fifelse(team_id_mdl == home_team_id, 1L, 0L),
    pos_points = data.table::fifelse(team_id_mdl == home_team_id, home_points, away_points),
    opp_points = data.table::fifelse(team_id_mdl == home_team_id, away_points, home_points)
  )]
  dt[, points_diff := pos_points - opp_points]

  # Batch all 11 shifts needed for mirror (single pass over group index)
  dt[, c("tmp_lag1_ti", "tmp_lag2_ti",
         "tmp_lag1_tm", "tmp_lag2_tm", "tmp_lag3_tm",
         "tmp_lag1_x", "tmp_lag2_x",
         "tmp_lead1_x", "tmp_lead2_x",
         "tmp_lead1_tm", "tmp_lead2_tm") := .(
    data.table::shift(throw_in, 1L, type = "lag"),
    data.table::shift(throw_in, 2L, type = "lag"),
    data.table::shift(team_id_mdl, 1L, type = "lag"),
    data.table::shift(team_id_mdl, 2L, type = "lag"),
    data.table::shift(team_id_mdl, 3L, type = "lag"),
    data.table::shift(x, 1L, type = "lag"),
    data.table::shift(x, 2L, type = "lag"),
    data.table::shift(x, 1L, type = "lead"),
    data.table::shift(x, 2L, type = "lead"),
    data.table::shift(team_id_mdl, 1L, type = "lead"),
    data.table::shift(team_id_mdl, 2L, type = "lead")
  ), by = grp]

  # Pre-compute sign() values to avoid repeated evaluation over 1.6M rows.
  # sign(x) is used in 6 of the 7 conditions; sign(tmp_lag1_x) in 3.
  dt[, c("s_x", "s_lag1_x", "s_lag2_x", "s_lead1_x", "s_lead2_x") := .(
    sign(x), sign(tmp_lag1_x), sign(tmp_lag2_x), sign(tmp_lead1_x), sign(tmp_lead2_x)
  )]

  # Mirror via fcase (same 7 conditions as the original dplyr calculate_mirror,
  # NAs treated as FALSE)
  dt[, mirror := data.table::fcase(
    # 1. Current throw-in with team change
    throw_in == 1L & tmp_lag1_ti != 1L & tmp_lag1_tm != team_id_mdl, -1,
    # 2. Consecutive throw-ins on same side with different team
    throw_in == 1L & tmp_lag1_ti == 1L & tmp_lag2_tm != team_id_mdl &
      s_lag1_x == s_x, -1,
    # 3. Consecutive throw-ins with same team but different side
    throw_in == 1L & tmp_lag1_ti == 1L & tmp_lag2_tm == team_id_mdl &
      s_lag1_x != s_x, -1,
    # 4. Previous throw-in affecting current play
    tmp_lag1_ti == 1L & s_lag1_x == s_x &
      tmp_lag1_tm == team_id_mdl & tmp_lag2_tm != team_id_mdl, -1,
    # 5. Throw-in two plays ago affecting current play
    tmp_lag2_ti == 1L & s_lag2_x == s_x &
      tmp_lag2_tm == team_id_mdl & tmp_lag3_tm != team_id_mdl, -1,
    # 6. Previous throw-in with future position considerations
    tmp_lag1_ti == 1L & s_lead2_x == s_x &
      tmp_lead2_tm != team_id_mdl, -1,
    # 7. Two plays ago throw-in with future considerations
    tmp_lag2_ti == 1L & s_lead1_x == s_x &
      tmp_lead1_tm != team_id_mdl, -1,
    default = 1
  )]

  # Clean up mirror temp columns
  dt[, c("tmp_lag1_ti", "tmp_lag2_ti",
         "tmp_lag1_tm", "tmp_lag2_tm", "tmp_lag3_tm",
         "tmp_lag1_x", "tmp_lag2_x",
         "tmp_lead1_x", "tmp_lead2_x",
         "tmp_lead1_tm", "tmp_lead2_tm",
         "s_x", "s_lag1_x", "s_lag2_x", "s_lead1_x", "s_lead2_x") := NULL]

  # Apply coordinate transform (x, y must be updated before goal_x)
  dt[, `:=`(x = mirror * x, y = mirror * y)]
  dt[, goal_x := venue_length / 2 - x]

  invisible(NULL)
}

#' Add EPV lagged variables and speed (data.table, by reference)
#'
#' Computes all lag/lead variables and derived speed metrics.
#' Batches shift operations into a single := call per group traversal.
#'
#' @param dt A data.table to modify by reference.
#' @param grp Character vector of grouping columns.
#' @return Invisible NULL (modifies dt by reference).
#' @keywords internal
add_epv_lag_vars_dt <- function(dt, grp) {
  # Batch all shifts (numeric + character) in one pass
  dt[, c("lv_lag1_x", "lv_lag1_y", "lv_lag1_gx", "lv_lag5_gx",
         "lv_lag1_ps", "lv_lag5_ps",
         "lv_lag1_tm", "lv_lag5_tm",
         "lv_lead1_x", "lv_lead1_y",
         "lv_lag1_desc", "lv_lead1_desc",
         "lv_lag1_pn", "lv_lead1_pn") := .(
    data.table::shift(x, 1L, type = "lag"),
    data.table::shift(y, 1L, type = "lag"),
    data.table::shift(goal_x, 1L, type = "lag"),
    data.table::shift(goal_x, 5L, type = "lag"),
    data.table::shift(period_seconds, 1L, type = "lag"),
    data.table::shift(period_seconds, 5L, type = "lag"),
    data.table::shift(team_id_mdl, 1L, type = "lag"),
    data.table::shift(team_id_mdl, 5L, type = "lag"),
    data.table::shift(x, 1L, type = "lead"),
    data.table::shift(y, 1L, type = "lead"),
    data.table::shift(description, 1L, type = "lag"),
    data.table::shift(description, 1L, type = "lead"),
    data.table::shift(player_name, 1L, type = "lag"),
    data.table::shift(player_name, 1L, type = "lead")
  ), by = grp]

  # Fill boundary NAs: lag columns with first(), lead columns with last()
  dt[, `:=`(
    lv_lag1_x = data.table::fifelse(is.na(lv_lag1_x), x[1L], lv_lag1_x),
    lv_lag1_y = data.table::fifelse(is.na(lv_lag1_y), y[1L], lv_lag1_y),
    lv_lag1_gx = data.table::fifelse(is.na(lv_lag1_gx), goal_x[1L], lv_lag1_gx),
    lv_lag5_gx = data.table::fifelse(is.na(lv_lag5_gx), goal_x[1L], lv_lag5_gx),
    lv_lag5_ps = data.table::fifelse(is.na(lv_lag5_ps), period_seconds[1L], lv_lag5_ps),
    lv_lead1_x = data.table::fifelse(is.na(lv_lead1_x), x[.N], lv_lead1_x),
    lv_lead1_y = data.table::fifelse(is.na(lv_lead1_y), y[.N], lv_lead1_y),
    lv_lag1_desc = data.table::fifelse(is.na(lv_lag1_desc), description[1L], lv_lag1_desc),
    lv_lead1_desc = data.table::fifelse(is.na(lv_lead1_desc), description[.N], lv_lead1_desc),
    lv_lag1_pn = data.table::fifelse(is.na(lv_lag1_pn), player_name[1L], lv_lag1_pn),
    lv_lead1_pn = data.table::fifelse(is.na(lv_lead1_pn), player_name[.N], lv_lead1_pn)
  ), by = grp]
  # Note: lv_lag1_ps intentionally left with NAs (speed1 NA -> replaced with 0)
  # Note: lv_lag1_tm, lv_lag5_tm left with NAs (used in fcase where NA -> default)

  # lag_x, lag_y: mirror if team changed (calculate_lagged_coordinate equivalent)
  dt[, `:=`(
    lag_x = data.table::fcase(
      is.na(lv_lag1_tm), lv_lag1_x,
      lv_lag1_tm == team_id_mdl, lv_lag1_x,
      default = -lv_lag1_x),
    lag_y = data.table::fcase(
      is.na(lv_lag1_tm), lv_lag1_y,
      lv_lag1_tm == team_id_mdl, lv_lag1_y,
      default = -lv_lag1_y)
  )]

  # lag_goal_x, lag_goal_x5: flip via venue_length if team changed
  dt[, `:=`(
    lag_goal_x = data.table::fcase(
      is.na(lv_lag1_tm), lv_lag1_gx,
      lv_lag1_tm == team_id_mdl, lv_lag1_gx,
      default = venue_length - lv_lag1_gx),
    lag_goal_x5 = data.table::fcase(
      is.na(lv_lag5_tm), lv_lag5_gx,
      lv_lag5_tm == team_id_mdl, lv_lag5_gx,
      default = venue_length - lv_lag5_gx)
  )]

  # Remaining output columns
  dt[, `:=`(
    lag_desc = lv_lag1_desc,
    lead_desc = lv_lead1_desc,
    lag_time5 = lv_lag5_ps,
    lag_player = lv_lag1_pn,
    lead_player = lv_lead1_pn,
    lead_x = lv_lead1_x,
    lead_y = lv_lead1_y,
    lead_goal_x = venue_length / 2 - lv_lead1_x
  )]

  # Speed variables
  dt[, `:=`(
    speed1 = (lag_goal_x - goal_x) / pmax(period_seconds - lv_lag1_ps, 1),
    speed5 = (lag_goal_x5 - goal_x) / pmax(period_seconds - lag_time5, 1)
  )]
  dt[is.na(speed1), speed1 := 0]
  dt[is.na(speed5), speed5 := 0]

  # Clean up temp columns
  dt[, c("lv_lag1_x", "lv_lag1_y", "lv_lag1_gx", "lv_lag5_gx",
         "lv_lag1_ps", "lv_lag5_ps",
         "lv_lag1_tm", "lv_lag5_tm",
         "lv_lead1_x", "lv_lead1_y",
         "lv_lag1_desc", "lv_lead1_desc",
         "lv_lag1_pn", "lv_lead1_pn") := NULL]

  invisible(NULL)
}

#' Clean Model Data for Win Probability (WP)
#'
#' @param df A dataframe containing raw play-by-play data.
#' @return A cleaned dataframe ready for WP modeling.
#' @keywords internal
clean_model_data_wp <- function(df) {
  df |>
    dplyr::filter(!is.na(.data$label_wp)) |>
    dplyr::mutate(
      xpoints_diff = .data$points_diff + .data$exp_pts,
      pos_lead_prob = calculate_pos_lead_prob(.data$points_diff, .data$opp_goal, .data$opp_behind, .data$no_score, .data$behind, .data$goal),
      time_left_scaler = exp(pmin(.data$est_match_elapsed / AFL_PLAY_QUARTER_SECONDS, AFL_TIME_SCALER_MAX)),
      diff_time_ratio = .data$xpoints_diff * .data$time_left_scaler,
      score_urgency = .data$points_diff / pmax(.data$est_match_remaining / 60, 1)
    )
}

#' Select EPV Model Variables
#'
#' @param df A dataframe containing cleaned play-by-play data.
#' @param label Logical, whether to include the label variable for training.
#' @return A dataframe with selected variables for EPV modeling.
#' @keywords internal
select_epv_model_vars <- function(df, label = FALSE) {
  base_vars <- c(
    "goal_x", "y", "lag_goal_x", "lag_goal_x5", "lag_y",
    "period_seconds", "period", "play_type_handball", "play_type_kick",
    "play_type_reception", "phase_of_play_handball_received",
    "phase_of_play_hard_ball", "phase_of_play_loose_ball",
    "phase_of_play_set_shot", "shot_row", "speed5", "home",
    "est_qtr_remaining", "est_match_remaining"
  )
  
  if (label) {
    base_vars <- c(base_vars, "label_ep")
  }
  
  df |> dplyr::select(dplyr::all_of(base_vars))
}

#' Select WP Model Variables
#'
#' @param df A dataframe containing cleaned play-by-play data.
#' @return A dataframe with selected variables for WP modeling.
#' @keywords internal
select_wp_model_vars <- function(df) {
  df |>
    dplyr::select(
      "est_match_elapsed", "est_match_remaining", "shot_row", "home", "points_diff",
      "xpoints_diff", "pos_lead_prob", "time_left_scaler", "diff_time_ratio", "score_urgency",
      "goal_x",
      "play_type_handball", "play_type_kick", "play_type_reception", "phase_of_play_handball_received",
      "phase_of_play_hard_ball", "phase_of_play_loose_ball", "phase_of_play_set_shot"
    )
}

#' Select AFL Match Prediction Model Variables
#'
#' @param df A dataframe containing AFL team model data
#' @return A dataframe with selected variables for AFL modeling
#' @keywords internal
select_afl_model_vars <- function(df) {
  df |>
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

      # Team rating variables (EPR - Expected Possession Rating)
      .data$epr_diff,
      .data$epr_recv_diff,
      .data$epr_disp_diff,
      .data$epr_spoil_diff,
      .data$epr_hitout_diff,
      .data$epr.x,
      .data$epr.y,

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

#' Select AFL XGBoost Model Variables (Lean Feature Set)
#'
#' Selects a reduced set of variables optimised for XGBoost match prediction.
#' Removes high-cardinality factor columns (team_name, team_name_season, venue)
#' that create hundreds of sparse dummies via model.matrix(), and drops sample
#' weights that aren't predictive features.
#'
#' @param df A dataframe containing AFL team model data with GAM predictions
#' @return A dataframe with ~25 numeric/low-cardinality features + response
#' @keywords internal
select_afl_xgb_vars <- function(df) {
  df |>
    dplyr::select(
      # Response
      .data$win,

      # Home/away (1 dummy)
      .data$team_type_fac,

      # Time features (numeric)
      .data$game_year_decimal.x,
      .data$game_prop_through_year.x,
      .data$game_prop_through_month.x,
      .data$game_prop_through_day.x,

      # EPR ratings (numeric)
      .data$epr_diff,
      .data$epr_recv_diff,
      .data$epr_disp_diff,
      .data$epr_spoil_diff,
      .data$epr_hitout_diff,
      .data$epr.x,
      .data$epr.y,

      # Venue/travel (numeric)
      .data$log_dist.x,
      .data$log_dist.y,
      .data$log_dist_diff,
      .data$familiarity.x,
      .data$familiarity.y,
      .data$familiarity_diff,

      # Days rest (4 dummies - low cardinality)
      .data$days_rest_diff_fac,

      # GAM predictions (the main signal)
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
#' @keywords internal
clean_shots_data <- function(df) {
  goal_width <- AFL_GOAL_WIDTH
  
  # Load shot player lookup (maps player_id to lumped factor for shot model)
  shot_player_df <- tryCatch(
    load_model_with_fallback("shot_player_df"),
    error = function(e) NULL
  )

  df <- df |>
    add_shot_result_variables() |>
    add_shot_geometry_variables(goal_width) |>
    add_shot_type_variables()

  if (!is.null(shot_player_df)) {
    df <- df |>
      dplyr::left_join(shot_player_df, by = c("player_id" = "player_id_shot"), keep = TRUE)
  } else {
    df$player_id_shot <- NA_character_
    df$player_name_shot <- NA_character_
  }

  df |>
    dplyr::mutate(
      player_id_shot = as.factor(tidyr::replace_na(.data$player_id_shot, "Other")),
      player_name_shot = as.factor(tidyr::replace_na(.data$player_name_shot, "Other"))
    ) |>
    dplyr::select(-"side_b", -"side_c")
}

#' Select Shot Model Variables
#'
#' @param df A dataframe containing cleaned shot data.
#' @return A dataframe with selected variables for shot modeling.
#' @keywords internal
select_shot_model_vars <- function(df) {
  df |>
    dplyr::select(
      "goal_x", "abs_y", "angle", "distance",
      "play_type", "phase_of_play",
      "player_id_shot", "player_name_shot"
    )
}

#' Prepare Shot Data for Modeling
#'
#' @param df A dataframe containing raw shot data.
#' @return A dataframe ready for shot modeling.
#' @keywords internal
prepare_shot_model_data <- function(df) {
  df |>
    clean_shots_data() |>
    select_shot_model_vars()
}

#' Create Shot Model Matrix
#'
#' @param df A dataframe containing prepared shot data.
#' @return A model matrix ready for shot modeling.
#' @keywords internal
create_shot_model_matrix <- function(df) {
  stats::model.matrix(~ . - 1, data = df)
}

#' Predict Shot Probabilities
#'
#' @param model A fitted shot model.
#' @param new_data A dataframe or model matrix containing new shot data.
#' @return A vector of predicted probabilities.
#' @keywords internal
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
  df |>
    dplyr::filter(.data$description %in% EPV_RELEVANT_DESCRIPTIONS) |>
    dplyr::filter(!(dplyr::near(.data$x, -.data$lead_x_tot) & dplyr::near(.data$y, -.data$lead_y_tot) & .data$description != "Centre Bounce"))
}

# NOTE: add_epv_variables() (legacy dplyr path) was removed alongside
# calculate_mirror(). The data.table implementation in
# clean_model_data_epv_dt() → add_epv_team_vars_dt() is the canonical path.

#' Determine Team ID Model
#'
#' @param throw_in A vector indicating if the play is a throw-in.
#' @param team_id A vector of team IDs.
#' @return A vector of determined team IDs.
#' @keywords internal
#' @importFrom dplyr case_when lead
determine_team_id_mdl <- function(throw_in, team_id) {
  rlang::check_installed("zoo", version = "1.8.0", reason = "for NA fill in EPV model data preparation")
  result <- dplyr::case_when(
    throw_in == 1 ~ dplyr::lead(team_id),
    TRUE ~ team_id
  )
  result <- zoo::na.locf0(result, fromLast = TRUE)
  zoo::na.locf0(result)
}

# NOTE: Legacy dplyr-based mirror helpers (is_current_throw_in_team_change,
# is_consecutive_throw_in_same_side, etc.) and calculate_mirror() were removed
# in favour of the data.table implementation in add_epv_team_vars_dt() above,
# which handles all 7 throw-in mirror conditions via fcase().

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
    points_diff > 1 ~ opp_behind + no_score + behind + goal,
    points_diff == 1 ~ (opp_behind * 0.5) + no_score + behind + goal,
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
  df |>
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
#' Computes side_b, side_c, angle, and distance to the attacking goal.
#' Internally folds `goal_x` to the near-goal distance: a player at x = -30
#' on a 165m field has goal_x = halfLen - x = 112.5 (signed), but the actual
#' distance to the goal they're attacking is 52.5m. Pre-fold, shots from
#' negative-x reported distance to the FAR goal — visible on the blog as e.g.
#' a 126m behind that should have been 40m. The fold uses venue_length when
#' available; tests / legacy callers without venue_length fall through to
#' the raw goal_x (which assumes coords are already pre-mirrored to be
#' positive).
#'
#' @param df A dataframe containing shot data.
#' @param goal_width The width of the goal.
#' @return A dataframe with additional shot geometry variables.
#' @keywords internal
#' @importFrom dplyr mutate if_else
add_shot_geometry_variables <- function(df, goal_width) {
  if ("venue_length" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        goal_x_near = pmin(.data$goal_x, .data$venue_length - .data$goal_x)
      )
  } else {
    df <- df |>
      dplyr::mutate(goal_x_near = .data$goal_x)
  }
  df |>
    dplyr::mutate(
      abs_y = abs(.data$y),
      side_b = sqrt((.data$goal_x_near)^2 + (.data$y + goal_width / 2)^2),
      side_c = sqrt((.data$goal_x_near)^2 + (.data$y - goal_width / 2)^2),
      angle = acos((.data$side_b^2 + .data$side_c^2 - goal_width^2) / (2 * .data$side_b * .data$side_c)),
      distance = dplyr::if_else(.data$y >= -goal_width / 2 & .data$y <= goal_width / 2,
        .data$goal_x_near, pmin(.data$side_b, .data$side_c)
      )
    ) |>
    dplyr::select(-"goal_x_near")
}

#' Add Shot Type Variables
#'
#' @param df A dataframe containing shot data.
#' @return A dataframe with additional shot type variables.
#' @keywords internal
#' @importFrom dplyr mutate if_else
add_shot_type_variables <- function(df) {
  df |>
    dplyr::mutate(
      shot_clanger = dplyr::if_else(.data$shot_at_goal == TRUE & .data$disposal == "clanger", 1, 0),
      shot_effective = dplyr::if_else(.data$shot_at_goal == TRUE & .data$disposal == "effective", 1, 0),
      shot_ineffective = dplyr::if_else(.data$shot_at_goal == TRUE & .data$disposal == "ineffective", 1, 0)
    )
}
