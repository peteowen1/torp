#' Add Expected Points Value (EPV) Variables
#'
#' This function adds EPV-related variables to the input dataframe.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with additional EPV-related variables.
#' @export
#' @importFrom dplyr mutate case_when if_else lead lag group_by ungroup
#' @importFrom lubridate as_date
add_epv_vars <- function(df) {
  base_ep_preds <- get_epv_preds(df)
  pbp_final <- cbind(df, base_ep_preds)

  pbp_final <- pbp_final %>%
    dplyr::group_by(.data$match_id, .data$period) %>%
    dplyr::mutate(
      exp_pts = round(dplyr::case_when(
        .data$description == "Centre Bounce" ~ 0,
        TRUE ~ -6 * .data$opp_goal - .data$opp_behind + .data$behind + 6 * .data$goal
      ), 5),
      exp_pts = dplyr::if_else(.data$description == "Out On Full After Kick", -dplyr::lead(.data$exp_pts, default = 0), .data$exp_pts),
      kick_points = dplyr::case_when(
        (.data$shot_at_goal == TRUE & .data$disposal == "clanger") ~ 0,
        TRUE ~ .data$points_shot
      ),
      player_name = paste(.data$player_name_given_name, .data$player_name_surname),
      pos_team = dplyr::case_when(
        !is.na(.data$points_shot) ~ 1,
        dplyr::lead(.data$team_id_mdl) == .data$team_id_mdl ~ 1,
        is.na(dplyr::lead(.data$team_id_mdl)) ~ 1,
        TRUE ~ -1
      ),
      team_change = dplyr::case_when(
        dplyr::lead(.data$team_id_mdl) == .data$team_id_mdl ~ 1,
        is.na(dplyr::lead(.data$team_id_mdl)) ~ 1,
        TRUE ~ -1
      ),
      lead_points = dplyr::if_else(is.na(.data$points_shot), dplyr::lead(.data$exp_pts, default = 0), (.data$points_shot - dplyr::lead(.data$exp_pts, default = 0))),
      lead_player = dplyr::if_else(!is.na(.data$points_shot) | .data$lead_desc == "Out of Bounds" | .data$description == "Out On Full After Kick",
        .data$player_name, dplyr::lead(.data$player_name)
      ),
      lead_player_id = dplyr::if_else(!is.na(.data$points_shot) | .data$lead_desc == "Out of Bounds" | .data$description == "Out On Full After Kick",
        .data$player_id, dplyr::lead(.data$player_id)
      ),
      lead_team = dplyr::if_else(is.na(.data$points_shot), dplyr::lead(.data$team), .data$team),
      xpoints_diff = .data$points_diff + .data$exp_pts,
      delta_epv = dplyr::lead(.data$xpoints_diff, default = dplyr::last(.data$points_diff)) * .data$team_change - .data$xpoints_diff,
      weight_gm = exp(as.numeric(-(lubridate::as_date(Sys.Date()) - lubridate::as_date(.data$utc_start_time))) / 365),
      round_week = sprintf("%02d", .data$round_number)
    ) %>%
    dplyr::ungroup()

  return(pbp_final)
}

#' Add Win Probability Variables
#'
#' This function adds win probability-related variables to the input dataframe.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with additional win probability-related variables.
#' @export
#' @importFrom dplyr mutate case_when lead group_by ungroup
add_wp_vars <- function(df) {
  base_wp_preds <- get_wp_preds(df)
  colnames(base_wp_preds) <- "wp"
  pbp_final <- cbind(df, base_wp_preds)

  pbp_final <- pbp_final %>%
    dplyr::group_by(.data$match_id) %>%
    dplyr::mutate(
      wp = round(.data$wp, 5),
      wpa = round(dplyr::case_when(
        dplyr::lead(.data$team_id_mdl, default = dplyr::last(.data$team_id_mdl)) == .data$team_id_mdl ~ dplyr::lead(.data$wp, default = dplyr::last(.data$wp)) - .data$wp,
        dplyr::lead(.data$team_id_mdl, default = dplyr::last(.data$team_id_mdl)) != .data$team_id_mdl ~ (1 - dplyr::lead(.data$wp, default = dplyr::last(.data$wp))) - .data$wp
      ), 5)
    ) %>%
    dplyr::ungroup()

  return(pbp_final)
}

#' Add Shot Variables
#'
#' This function adds shot-related variables to the input dataframe.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with additional shot-related variables.
#' @export
#' @importFrom dplyr mutate
add_shot_vars <- function(df) {
  ocat_shot_result_preds <- as.data.frame(get_shot_result_preds(df))
  colnames(ocat_shot_result_preds) <- c("clanger_prob", "behind_prob", "goal_prob")

  pbp_final <- cbind(df, ocat_shot_result_preds)

  pbp_final <- pbp_final %>%
    dplyr::mutate(
      on_target_prob = .data$goal_prob + .data$behind_prob,
      xscore = .data$goal_prob * 6 + .data$behind_prob
    ) %>%
    dplyr::ungroup()

  return(pbp_final)
}

#' Get Expected Points Value Predictions
#'
#' This function generates EPV predictions for the input data.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with EPV predictions.
#' @keywords internal
#' @importFrom stats predict model.matrix
get_epv_preds <- function(df) {
  preds <- as.data.frame(
    matrix(stats::predict(torp::ep_model, stats::model.matrix(~ . + 0, data = df %>% select_epv_model_vars())),
      ncol = 5, byrow = TRUE
    )
  )
  colnames(preds) <- c("opp_goal", "opp_behind", "behind", "goal", "no_score")

  return(preds)
}

#' Get Win Probability Predictions
#'
#' This function generates win probability predictions for the input data.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with win probability predictions.
#' @keywords internal
#' @importFrom stats predict model.matrix
get_wp_preds <- function(df) {
  preds <- as.data.frame(
    matrix(stats::predict(torp::wp_model, stats::model.matrix(~ . + 0, data = df %>% select_wp_model_vars())),
      ncol = 1, byrow = TRUE
    )
  )

  return(preds)
}

#' Get Shot Result Predictions
#'
#' This function generates shot result predictions for the input data.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with shot result predictions.
#' @keywords internal
#' @importFrom stats predict
get_shot_result_preds <- function(df) {
  preds <- stats::predict(torp::shot_ocat_mdl, df, type = "response")
  return(preds)
}
