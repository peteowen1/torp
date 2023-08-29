#' add ep vars
#'
#' @param df
#'
#'
#' @examples
#' \dontrun{
#' add_epv_vars(df)
#' }
add_epv_vars <- function(df) {
  base_ep_preds <- get_epv_preds(df)
  pbp_final <- cbind(df, base_ep_preds)

  pbp_final <- pbp_final %>%
    group_by(match_id, period) %>%
    dplyr::mutate(
      exp_pts = round(dplyr::case_when(
        description == "Centre Bounce" ~ 0,
        TRUE ~ -6 * opp_goal - opp_behind + behind + 6 * goal
      ), 5),
      exp_pts = dplyr::if_else(description == "Out On Full After Kick", -dplyr::lead(exp_pts, default = 0), exp_pts),
      kick_points = dplyr::case_when(
        (shot_at_goal == T & disposal == "clanger") ~ 0,
        TRUE ~ points_shot
      ),
      player_name = (paste(player_name_given_name, player_name_surname)),
      pos_team = dplyr::case_when(
        !is.na(points_shot) ~ 1,
        # dplyr::lead(throw_in) == 1 ~ 1,
        # throw_in == 1 & dplyr::lag(team_id_mdl) != team_id_mdl ~ -1,
        dplyr::lead(team_id_mdl) == team_id_mdl ~ 1,
        is.na(dplyr::lead(team_id_mdl)) ~ 1,
        TRUE ~ -1
      ),
      team_change = dplyr::case_when(
        dplyr::lead(team_id_mdl) == team_id_mdl ~ 1,
        is.na(dplyr::lead(team_id_mdl)) ~ 1,
        TRUE ~ -1
      ),
      lead_points = dplyr::if_else(is.na(points_shot), dplyr::lead(exp_pts, default = 0), (points_shot - lead(exp_pts, default = 0))), ### maybe? (kick_points - lead_exp_pts) e.g. for behinds
      lead_player = dplyr::if_else(!is.na(points_shot) | lead_desc == "Out of Bounds" | description == "Out On Full After Kick",
        player_name, dplyr::lead(player_name)
      ),
      lead_player_id = dplyr::if_else(!is.na(points_shot) | lead_desc == "Out of Bounds" | description == "Out On Full After Kick",
        player_id, dplyr::lead(player_id)
      ),
      lead_team = dplyr::if_else(is.na(points_shot), dplyr::lead(team), team),
      xpoints_diff = points_diff + exp_pts,
      delta_epv = lead(xpoints_diff, default = dplyr::last(points_diff)) * team_change - xpoints_diff, # round(lead_points * pos_team - exp_pts, 5),
      weight_gm = exp(as.numeric(-(Sys.Date() - as.Date(utc_start_time))) / 365),
      round_week = sprintf("%02d", round_number)
    ) %>%
    ungroup()

  return(pbp_final)
}

######## multinomial
# add_shot_preds <- function(df) {
#   base_shot_preds <- get_shot_preds(df)
#   colnames(base_shot_preds) <- c('clanger_prob','behind_prob','goal_prob')
#
#   pbp_final <- cbind(df, base_shot_preds)
#   pbp_final <- pbp_final %>%
#     mutate(clanger_prob = dplyr::if_else(!is.na(shot_at_goal),clanger_prob,NA),
#            behind_prob = dplyr::if_else(!is.na(shot_at_goal),behind_prob,NA),
#            goal_prob = dplyr::if_else(!is.na(shot_at_goal),goal_prob,NA),
#            xg = 6*goal_prob + behind_prob)
#
#   return(pbp_final)
# }


add_wp_vars <- function(df) {
  base_wp_preds <- get_wp_preds(df)
  colnames(base_wp_preds) <- "wp"
  pbp_final <- cbind(df, base_wp_preds)

  pbp_final <- pbp_final %>%
    dplyr::group_by(match_id) %>%
    dplyr::mutate(
      wp = round(wp, 5),
      wpa = round(dplyr::case_when(
        dplyr::lead(team_id_mdl, default = dplyr::last(team_id_mdl)) == team_id_mdl ~ dplyr::lead(wp, default = dplyr::last(wp)) - wp,
        dplyr::lead(team_id_mdl, default = dplyr::last(team_id_mdl)) != team_id_mdl ~ (1 - dplyr::lead(wp, default = dplyr::last(wp))) - wp
      ), 5)
    ) %>%
    dplyr::ungroup()

  return(pbp_final)
}


add_shot_vars <- function(df) {
  base_shot_on_target_preds <- tibble::tibble(on_target_prob = get_shot_on_target_preds(df))
  base_shot_result_preds <- tibble::tibble(goal_prob = get_shot_result_preds(df))
  pbp_final <- cbind(df, base_shot_result_preds, base_shot_on_target_preds)

  pbp_final <- pbp_final %>%
    dplyr::mutate(
      on_target_prob = round(dplyr::if_else(!is.na(shot_at_goal) & x > 0, on_target_prob, NA_real_), 5),
      goal_prob = round(dplyr::if_else(!is.na(shot_at_goal) & x > 0, goal_prob, NA_real_), 5),
      xscore = on_target_prob * (goal_prob * 6 + (1 - goal_prob))
    ) %>%
    dplyr::ungroup()

  return(pbp_final)
}
# get predictions for a set of data
# for predict stage
get_epv_preds <- function(df) {
  preds <- as.data.frame(
    matrix(stats::predict(ep_model, stats::model.matrix(~ . + 0, data = df %>% select_epv_model_vars())),
      ncol = 5, byrow = TRUE
    )
  )
  colnames(preds) <- c("opp_goal", "opp_behind", "behind", "goal", "no_score")

  return(preds)
}

# for predict stage
get_wp_preds <- function(df) {
  preds <- as.data.frame(
    matrix(stats::predict(wp_model, stats::model.matrix(~ . + 0, data = df %>% select_wp_model_vars())),
      ncol = 1, byrow = TRUE
    )
  )

  return(preds)
}

# for predict stage
get_shot_on_target_preds <- function(df) {
  preds <- stats::predict(shot_on_target_mdl, df, type = "response")
  return(preds)
}

# for predict stage
get_shot_result_preds <- function(df) {
  preds <- stats::predict(shot_result_mdl, df, type = "response")
  return(preds)
}
