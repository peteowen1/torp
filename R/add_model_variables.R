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
  base_ep_preds <- get_preds(df)
  pbp_final <- cbind(df, base_ep_preds)

  pbp_final <- pbp_final %>%
    dplyr::mutate(
      exp_pts = case_when(
        description == "Centre Bounce" ~ 0,
        TRUE ~ -6 * Opp_Goal - Opp_Behind + Behind + 6 * Goal
      ),
      kick_points = dplyr::case_when(
        (shot_at_goal == T & disposal == "clanger") ~ 0,
        TRUE ~ points_shot
      ),
      player_full = (paste(player_name_given_name, player_name_surname)),
      pos_team = dplyr::case_when(
        shot_at_goal == TRUE ~ 1,
        lead(throw_in) == 1 ~ 1,
        throw_in == 1 & lag(team_id_mdl) != team_id_mdl ~ -1,
        lead(team_id_mdl) == team_id_mdl ~ 1,
        is.na(lead(team_id_mdl)) ~ 1,
        TRUE ~ -1
      ),
      lead_points = ifelse(is.na(shot_at_goal), lead(exp_pts), (kick_points)), ### maybe? (kick_points - lead_exp_pts) e.g. for behinds
      lead_player = ifelse(is.na(shot_at_goal), lead(player_full), player_full),
      lead_player_id = ifelse(is.na(shot_at_goal), lead(player_id), player_id),
      lead_team = ifelse(is.na(shot_at_goal), lead(team), team),
      delta = round(lead_points * pos_team - exp_pts, 5),
      weight_gm = exp(as.numeric(-(Sys.Date() - as.Date(utc_start_time))) / 365),
      round_week = sprintf("%02d", round_number)
    )

  return(pbp_final)
}

# get predictions for a set of data
# for predict stage
get_preds <- function(df) {
  preds <- as.data.frame(
    matrix(stats::predict(ep_model, stats::model.matrix(~ . + 0, data = df %>% select_epv_model_vars())),
      ncol = 5, byrow = TRUE
    )
  )
  colnames(preds) <- c("Opp_Goal", "Opp_Behind", "Behind", "Goal", "No_Score")

  return(preds)
}
