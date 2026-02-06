#' Create Player Game Data
#'
#' Transforms raw play-by-play data and player stats into processed per-game
#' player performance data used by the TORP ratings pipeline.
#'
#' Computes disposal points, reception points, spoil/tackle points, and
#' hitout points for each player-game combination. Adjusts each component
#' by subtracting the 40th percentile within each position group.
#'
#' @param pbp_data Play-by-play data from \code{load_pbp()}. If NULL, loads all available.
#' @param player_stats Raw player stats from \code{load_player_stats()}. If NULL, loads all available.
#' @param teams Team lineup data from \code{load_teams()}. If NULL, loads all available.
#' @param decay Decay factor for time-weighting games. Default is 500.
#'
#' @return A data.table with one row per player per match, containing columns:
#'   \code{player_id}, \code{match_id}, \code{plyr_nm}, \code{utc_start_time},
#'   \code{tm}, \code{opp}, \code{pos}, \code{round}, \code{season}, \code{team_id},
#'   \code{weight_gm}, \code{tot_p_adj}, \code{recv_pts_adj}, \code{disp_pts_adj},
#'   \code{spoil_pts_adj}, \code{hitout_pts_adj}.
#'
#' @export
#'
#' @importFrom dplyr arrange select mutate group_by summarise left_join filter ungroup relocate if_else last n_distinct
#' @importFrom tidyr replace_na
#' @importFrom lubridate year
#' @importFrom stats quantile
create_player_game_data <- function(pbp_data = NULL,
                                    player_stats = NULL,
                                    teams = NULL,
                                    decay = 500) {

  if (is.null(pbp_data)) pbp_data <- load_pbp(TRUE)
  if (is.null(player_stats)) player_stats <- load_player_stats(TRUE)

  if (is.null(teams)) teams <- load_teams(TRUE)

  # --- Step 1: Disposal points from PBP (grouped by player_id + match_id) ---
  disp_df <- pbp_data %>%
    dplyr::arrange(match_id, display_order) %>%
    dplyr::select(
      player_name, player_id, match_id, utc_start_time, home_away,
      away_team_team_name, home_team_team_name,
      delta_epv, team, player_position, round_week, pos_team, wpa
    ) %>%
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay),
      opp_tm = ifelse(home_away == "Home", away_team_team_name, home_team_team_name)
    ) %>%
    dplyr::group_by(player_id, match_id) %>%
    dplyr::summarise(
      plyr_nm = max(player_name, na.rm = TRUE),
      gms = dplyr::n_distinct(match_id),
      utc_start_time = max(utc_start_time),
      weight_gm = max(weight_gm),
      disp_pts = sum(dplyr::if_else(pos_team == -1, delta_epv - 0.04, delta_epv + 0.08) / 2),
      disp_pts_wt = sum((dplyr::if_else(pos_team == -1, delta_epv - 0.04, delta_epv + 0.08) * max(weight_gm)) / 2),
      disp = floor(dplyr::n() / 2),
      tm = dplyr::last(team),
      opp = dplyr::last(opp_tm),
      pos = dplyr::last(player_position),
      round = as.numeric(dplyr::last(round_week)),
      season = dplyr::last(lubridate::year(utc_start_time)),
      .groups = "drop"
    )

  # --- Step 2: Reception points (self-join on lead_player_id) ---
  recv_df <- pbp_data %>%
    dplyr::select(
      lead_player, lead_player_id, match_id, utc_start_time, home_away,
      away_team_team_name, home_team_team_name,
      delta_epv, team, player_position, round_week, pos_team, wpa
    ) %>%
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay)
    ) %>%
    dplyr::group_by(lead_player, lead_player_id, match_id) %>%
    dplyr::summarise(
      recv_pts = sum(dplyr::if_else(pos_team == -1, (1.5 * delta_epv * pos_team) + 0.1, (1 * delta_epv * pos_team) + 0.05) / 2),
      recv_pts_wt = sum((dplyr::if_else(pos_team == -1, (1.5 * delta_epv * pos_team) + 0.1, ((1 * delta_epv * pos_team) + 0.05)) * max(weight_gm)) / 2),
      recvs = dplyr::n(),
      .groups = "drop"
    )

  # --- Step 3: Join disposal + reception ---
  plyr_gm_df <- disp_df %>%
    dplyr::left_join(
      recv_df,
      by = c("player_id" = "lead_player_id", "match_id" = "match_id")
    )

  # --- Step 4: Join spoils/tackles/hitouts from raw player_stats ---
  spoil_hitout_df <- player_stats %>%
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(max(as.Date(utc_start_time)) - as.Date(utc_start_time))) / decay),
      spoil_pts = extended_stats_spoils * 0.6 + tackles * 0.1 + extended_stats_pressure_acts * 0.1 - extended_stats_def_half_pressure_acts * 0.2,
      spoil_pts_wt = spoil_pts * max(weight_gm),
      hitout_pts = hitouts * 0.15 + extended_stats_hitouts_to_advantage * 0.25 - extended_stats_ruck_contests * 0.06,
      hitout_pts_wt = hitout_pts * max(weight_gm)
    ) %>%
    dplyr::select(-utc_start_time)

  plyr_gm_df <- plyr_gm_df %>%
    dplyr::left_join(
      spoil_hitout_df,
      by = c("player_id" = "player_player_player_player_id", "match_id" = "provider_id")
    )

  # --- Step 5: Replace NAs and compute totals ---
  plyr_gm_df <- plyr_gm_df %>%
    dplyr::mutate(
      recv_pts = tidyr::replace_na(recv_pts, 0),
      recv_pts_wt = tidyr::replace_na(recv_pts_wt, 0),
      disp_pts = tidyr::replace_na(disp_pts, 0) - (bounces * 0.2),
      disp_pts_wt = tidyr::replace_na(disp_pts_wt, 0),
      spoil_pts = tidyr::replace_na(spoil_pts, 0),
      spoil_pts_wt = tidyr::replace_na(spoil_pts_wt, 0),
      hitout_pts = tidyr::replace_na(hitout_pts, 0),
      hitout_pts_wt = tidyr::replace_na(hitout_pts_wt, 0),
      tot_p = recv_pts + disp_pts + spoil_pts + hitout_pts,
      tot_p_wt = recv_pts_wt + disp_pts_wt + spoil_pts_wt + hitout_pts_wt
    )

  # --- Step 6: Join teams data for position ---
  plyr_gm_df <- plyr_gm_df %>%
    dplyr::left_join(
      teams,
      by = c("match_id" = "providerId", "player_id" = "player.playerId")
    )

  # --- Step 7: Position-group adjustment (subtract 40th percentile) ---
  plyr_gm_df <- plyr_gm_df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(position) %>%
    dplyr::mutate(
      recv_pts_adj = recv_pts - stats::quantile(recv_pts, 0.4, na.rm = TRUE),
      disp_pts_adj = disp_pts - stats::quantile(disp_pts, 0.4, na.rm = TRUE),
      spoil_pts_adj = spoil_pts - stats::quantile(spoil_pts, 0.4, na.rm = TRUE),
      hitout_pts_adj = hitout_pts - stats::quantile(hitout_pts, 0.4, na.rm = TRUE),
      tot_p_adj = recv_pts_adj + disp_pts_adj + spoil_pts_adj + hitout_pts_adj
    ) %>%
    dplyr::ungroup()

  # --- Step 8: Handle duplicate season columns and clean up ---
  if ("season.x" %in% names(plyr_gm_df)) {
    plyr_gm_df <- plyr_gm_df %>% dplyr::mutate(season = season.x)
  }

  plyr_gm_df <- plyr_gm_df %>%
    dplyr::relocate(tot_p_adj, disp, recv_pts_adj, disp_pts_adj, spoil_pts_adj, hitout_pts_adj) %>%
    dplyr::filter(!is.na(tm))

  return(plyr_gm_df)
}
