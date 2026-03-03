#' Default credit assignment parameters
#'
#' Returns a named list of all credit assignment parameters with their default
#' values. Used by \code{create_player_game_data()} when no custom params are provided.
#'
#' @return A named list of credit assignment parameters.
#' @export
default_credit_params <- function() {
  list(
    disp_neg_offset   = CREDIT_DISP_NEG_OFFSET,
    disp_pos_offset   = CREDIT_DISP_POS_OFFSET,
    disp_scale        = CREDIT_DISP_SCALE,
    bounce_penalty    = CREDIT_BOUNCE_PENALTY,
    recv_neg_mult     = CREDIT_RECV_NEG_MULT,
    recv_neg_offset   = CREDIT_RECV_NEG_OFFSET,
    recv_pos_mult     = CREDIT_RECV_POS_MULT,
    recv_pos_offset   = CREDIT_RECV_POS_OFFSET,
    recv_scale        = CREDIT_RECV_SCALE,
    spoil_wt          = CREDIT_SPOIL_WT,
    tackle_wt         = CREDIT_TACKLE_WT,
    pressure_wt       = CREDIT_PRESSURE_WT,
    def_pressure_wt   = CREDIT_DEF_PRESSURE_WT,
    hitout_wt         = CREDIT_HITOUT_WT,
    hitout_adv_wt     = CREDIT_HITOUT_ADV_WT,
    ruck_contest_wt        = CREDIT_RUCK_CONTEST_WT,
    pos_adj_quantile       = CREDIT_POS_ADJ_QUANTILE
  )
}

#' Create Player Game Data
#'
#' Transforms raw play-by-play data and player stats into processed per-game
#' player performance data used by the TORP ratings pipeline.
#'
#' Computes disposal points, reception points, spoil/tackle points, and
#' hitout points for each player-game combination.
#'
#' @param pbp_data Play-by-play data from \code{load_pbp()}. If NULL, loads all available.
#' @param player_stats Raw player stats from \code{load_player_stats()}. If NULL, loads all available.
#' @param teams Team lineup data from \code{load_teams()}. If NULL, loads all available.
#' @param decay Decay factor for time-weighting games. Default is \code{RATING_DECAY_DEFAULT_DAYS} (486).
#' @param credit_params Named list of credit assignment parameters. If NULL,
#'   uses \code{default_credit_params()}.
#'
#' @return A data.table with one row per player per match, containing:
#'   identifiers (\code{player_id}, \code{match_id}, \code{season}, \code{round},
#'   \code{plyr_nm}, \code{tm}, \code{opp}, \code{pos}, \code{position}, \code{team_id},
#'   \code{utc_start_time}), position-adjusted TORP credits (\code{tot_p_adj},
#'   \code{recv_pts_adj}, \code{disp_pts_adj}, \code{spoil_pts_adj}, \code{hitout_pts_adj}),
#'   raw TORP credits (\code{tot_p}, \code{recv_pts}, \code{disp_pts}, \code{spoil_pts},
#'   \code{hitout_pts}), and key box-score stats.
#'
#' @export
#'
#' @importFrom dplyr arrange select mutate group_by summarise left_join filter ungroup if_else last n_distinct
#' @importFrom tidyr replace_na
#' @importFrom lubridate year
#' @importFrom stats quantile
#' @importFrom cli cli_warn
create_player_game_data <- function(pbp_data = NULL,
                                    player_stats = NULL,
                                    teams = NULL,
                                    decay = RATING_DECAY_DEFAULT_DAYS,
                                    credit_params = NULL) {

  p <- if (is.null(credit_params)) default_credit_params() else credit_params

  if (is.null(pbp_data)) pbp_data <- load_pbp(TRUE)
  if (is.null(player_stats)) player_stats <- load_player_stats(TRUE)

  if (is.null(teams)) teams <- load_teams(TRUE)

  # Compute a single reference date for consistent decay weights across all data sources
  ref_date <- max(as.Date(pbp_data$utc_start_time), na.rm = TRUE)

  # --- Step 1: Disposal points from PBP (grouped by player_id + match_id) ---
  disp_df <- pbp_data |>
    dplyr::arrange(match_id, display_order) |>
    dplyr::select(
      player_name, player_id, match_id, utc_start_time, home_away,
      away_team_team_name, home_team_team_name,
      delta_epv, team, player_position, round_week, pos_team, wpa
    ) |>
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(ref_date - as.Date(utc_start_time))) / decay),
      opp_tm = ifelse(home_away == "Home", away_team_team_name, home_team_team_name)
    ) |>
    dplyr::group_by(player_id, match_id) |>
    dplyr::summarise(
      plyr_nm = max(player_name, na.rm = TRUE),
      utc_start_time = max(utc_start_time),
      weight_gm = max(weight_gm),
      disp_pts = sum(dplyr::if_else(pos_team == -1, delta_epv + p$disp_neg_offset, delta_epv + p$disp_pos_offset) * p$disp_scale),
      disp = floor(dplyr::n() / 2),
      tm = dplyr::last(team),
      opp = dplyr::last(opp_tm),
      pos = dplyr::last(player_position),
      round = as.numeric(dplyr::last(round_week)),
      season = dplyr::last(lubridate::year(utc_start_time)),
      .groups = "drop"
    )

  # --- Step 2: Reception points (self-join on lead_player_id) ---
  recv_df <- pbp_data |>
    dplyr::select(
      lead_player, lead_player_id, match_id, utc_start_time, home_away,
      away_team_team_name, home_team_team_name,
      delta_epv, team, player_position, round_week, pos_team, wpa
    ) |>
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(ref_date - as.Date(utc_start_time))) / decay)
    ) |>
    dplyr::group_by(lead_player, lead_player_id, match_id) |>
    dplyr::summarise(
      recv_pts = sum(dplyr::if_else(pos_team == -1, (p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset, (p$recv_pos_mult * delta_epv * pos_team) + p$recv_pos_offset) * p$recv_scale),
      recvs = dplyr::n(),
      .groups = "drop"
    )

  # --- Step 3: Join disposal + reception ---
  plyr_gm_df <- disp_df |>
    dplyr::left_join(
      recv_df,
      by = c("player_id" = "lead_player_id", "match_id" = "match_id")
    )

  # --- Step 4: Join spoils/tackles/hitouts from raw player_stats ---
  spoil_hitout_df <- player_stats |>
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(ref_date - as.Date(utc_start_time))) / decay),
      spoil_pts = extended_stats_spoils * p$spoil_wt + tackles * p$tackle_wt + extended_stats_pressure_acts * p$pressure_wt - extended_stats_def_half_pressure_acts * p$def_pressure_wt,
      hitout_pts = hitouts * p$hitout_wt + extended_stats_hitouts_to_advantage * p$hitout_adv_wt - extended_stats_ruck_contests * p$ruck_contest_wt
    ) |>
    dplyr::select(-utc_start_time)

  plyr_gm_df <- plyr_gm_df |>
    dplyr::left_join(
      spoil_hitout_df,
      by = c("player_id" = "player_player_player_player_id", "match_id" = "provider_id")
    )

  # Assert join produced matches (catches upstream schema changes)
  if (all(is.na(plyr_gm_df$spoil_pts))) {
    cli::cli_abort(c(
      "Player stats join produced no matches - all spoil/hitout points are zero.",
      "i" = "The column {.val player_player_player_player_id} may have changed in upstream data.",
      "i" = "Check that {.fn load_player_stats} returns the expected column names."
    ))
  }

  # --- Step 5: Replace NAs and compute totals ---
  plyr_gm_df <- plyr_gm_df |>
    dplyr::mutate(
      recv_pts = tidyr::replace_na(recv_pts, 0),
      disp_pts = tidyr::replace_na(disp_pts, 0) - (bounces * p$bounce_penalty),
      spoil_pts = tidyr::replace_na(spoil_pts, 0),
      hitout_pts = tidyr::replace_na(hitout_pts, 0),
      tot_p = recv_pts + disp_pts + spoil_pts + hitout_pts
    )

  # --- Step 6: Join teams data for position ---
  plyr_gm_df <- plyr_gm_df |>
    dplyr::left_join(
      teams,
      by = c("match_id" = "providerId", "player_id" = "player.playerId")
    ) |>
    dplyr::mutate(
      position = dplyr::if_else(position == "MIDFIELDER_FORWARD", "MEDIUM_FORWARD", position)
    )

  # --- Step 7: Position-group quantile adjustment ---
  plyr_gm_df <- plyr_gm_df |>
    dplyr::group_by(position) |>
    dplyr::mutate(
      recv_pts_adj = recv_pts - stats::quantile(recv_pts, p$pos_adj_quantile, na.rm = TRUE),
      disp_pts_adj = disp_pts - stats::quantile(disp_pts, p$pos_adj_quantile, na.rm = TRUE),
      spoil_pts_adj = spoil_pts - stats::quantile(spoil_pts, p$pos_adj_quantile, na.rm = TRUE),
      hitout_pts_adj = hitout_pts - stats::quantile(hitout_pts, p$pos_adj_quantile, na.rm = TRUE),
      tot_p_adj = recv_pts_adj + disp_pts_adj + spoil_pts_adj + hitout_pts_adj
    ) |>
    dplyr::ungroup()

  # --- Step 8: Handle duplicate season columns and select final columns ---
  if ("season.x" %in% names(plyr_gm_df)) {
    plyr_gm_df <- plyr_gm_df |> dplyr::mutate(season = season.x)
  }

  plyr_gm_df <- plyr_gm_df |>
    dplyr::filter(!is.na(tm)) |>
    dplyr::select(
      # Identifiers
      player_id, match_id, season, round,
      plyr_nm, tm, opp, pos, position, team_id,
      utc_start_time,
      # TORP credit points (position-adjusted)
      tot_p_adj, recv_pts_adj, disp_pts_adj, spoil_pts_adj, hitout_pts_adj,
      # TORP credit points (raw)
      tot_p, recv_pts, disp_pts, spoil_pts, hitout_pts,
      # PBP-derived action counts
      disp, recvs,
      # Credit model input stats
      extended_stats_spoils, tackles, extended_stats_pressure_acts,
      extended_stats_def_half_pressure_acts,
      hitouts, extended_stats_hitouts_to_advantage, extended_stats_ruck_contests,
      bounces,
      # Core box-score stats
      goals, behinds, kicks, handballs, disposals, marks,
      contested_possessions, uncontested_possessions,
      inside50s, marks_inside50, contested_marks,
      clearances_total_clearances,
      metres_gained, time_on_ground_percentage,
      intercepts, rebound50s, one_percenters,
      frees_for, frees_against, clangers, turnovers,
      score_involvements, shots_at_goal, goal_assists,
      extended_stats_ground_ball_gets
    )

  return(plyr_gm_df)
}
