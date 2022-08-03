#' add ep vars
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' add_epv_vars(df)
#' }
add_epv_vars <- function(df) {
  base_ep_preds <- get_epv_preds(df)
  pbp_final <- cbind(df, base_ep_preds)

  pbp_final <- pbp_final %>%
    group_by(match_id) %>%
    dplyr::mutate(
      exp_pts = round(case_when(
        description == "Centre Bounce" ~ 0,
        TRUE ~ -6 * opp_goal - opp_behind + behind + 6 * goal
      ), 5),
      kick_points = dplyr::case_when(
        (shot_at_goal == T & disposal == "clanger") ~ 0,
        TRUE ~ points_shot
      ),
      player_name = (paste(player_name_given_name, player_name_surname)),
      pos_team = dplyr::case_when(
        shot_at_goal == TRUE ~ 1,
        dplyr::lead(throw_in) == 1 ~ 1,
        throw_in == 1 & dplyr::lag(team_id_mdl) != team_id_mdl ~ -1,
        dplyr::lead(team_id_mdl) == team_id_mdl ~ 1,
        is.na(dplyr::lead(team_id_mdl)) ~ 1,
        TRUE ~ -1
      ),
      lead_points = ifelse(is.na(shot_at_goal), dplyr::lead(exp_pts, default = 0), (kick_points - lead(exp_pts, default = 0))), ### maybe? (kick_points - lead_exp_pts) e.g. for behinds
      lead_player = ifelse(!is.na(shot_at_goal) | lead_desc == "Out of Bounds" | description == "Out On Full After Kick",
        player_name, dplyr::lead(player_name)
      ),
      lead_player_id = ifelse(!is.na(shot_at_goal) | lead_desc == "Out of Bounds" | description == "Out On Full After Kick",
        player_id, dplyr::lead(player_id)
      ),
      lead_team = ifelse(is.na(shot_at_goal), dplyr::lead(team), team),
      delta_epv = round(lead_points * pos_team - exp_pts, 5),
      weight_gm = exp(as.numeric(-(Sys.Date() - as.Date(utc_start_time))) / 365),
      round_week = sprintf("%02d", round_number)
    ) %>%
    ungroup()

  return(pbp_final)
}


add_wp_vars <- function(df) {
  base_wp_preds <- get_wp_preds(df)
  colnames(base_wp_preds) <- "wp"
  pbp_final <- cbind(df, base_wp_preds)

  pbp_final$wp2 <- round(mgcv::predict.bam(wp_model_gam,pbp_final,type="response"),5)

  pbp_final <- pbp_final %>%
    group_by(match_id) %>%
    dplyr::mutate(
      wp = round(wp, 5),
      wpa = round(case_when(
        lead(team_id_mdl, default = last(team_id_mdl)) == team_id_mdl ~ dplyr::lead(wp, default = last(wp)) - wp,
        lead(team_id_mdl, default = last(team_id_mdl)) != team_id_mdl ~ (1 - dplyr::lead(wp, default = last(wp))) - wp
      ),5),
      wpa2 = round(case_when(
        lead(team_id_mdl, default = last(team_id_mdl)) == team_id_mdl ~ dplyr::lead(wp2, default = last(wp2)) - wp2,
        lead(team_id_mdl, default = last(team_id_mdl)) != team_id_mdl ~ (1 - dplyr::lead(wp2, default = last(wp2))) - wp2
      ),5)
    ) %>%
    ungroup()

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
