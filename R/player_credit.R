#' Default EPV assignment parameters
#'
#' Returns a named list of all EPV assignment parameters with their default
#' values. Used by \code{create_player_game_data()} when no custom params are provided.
#'
#' @return A named list of EPV assignment parameters.
#' @export
default_epv_params <- function() {
  list(
    disp_neg_offset   = EPV_DISP_NEG_OFFSET,
    disp_pos_offset   = EPV_DISP_POS_OFFSET,
    disp_scale        = EPV_DISP_SCALE,
    bounce_wt         = EPV_BOUNCE_WT,
    recv_neg_mult     = EPV_RECV_NEG_MULT,
    recv_neg_offset   = EPV_RECV_NEG_OFFSET,
    recv_pos_mult     = EPV_RECV_POS_MULT,
    recv_pos_offset   = EPV_RECV_POS_OFFSET,
    recv_scale        = EPV_RECV_SCALE,
    recv_intercept_mark_scale = EPV_RECV_INTERCEPT_MARK_SCALE,
    spoil_wt          = EPV_SPOIL_WT,
    tackle_wt         = EPV_TACKLE_WT,
    pressure_wt       = EPV_PRESSURE_WT,
    def_pressure_wt   = EPV_DEF_PRESSURE_WT,
    hitout_wt         = EPV_HITOUT_WT,
    hitout_adv_wt     = EPV_HITOUT_ADV_WT,
    ruck_contest_wt        = EPV_RUCK_CONTEST_WT,
    contested_poss_wt      = EPV_CONTESTED_POSS_WT,
    contested_marks_wt     = EPV_CONTESTED_MARKS_WT,
    ground_ball_gets_wt    = EPV_GROUND_BALL_GETS_WT,
    marks_inside50_wt      = EPV_MARKS_INSIDE50_WT,
    inside50s_wt           = EPV_INSIDE50S_WT,
    clangers_wt            = EPV_CLANGERS_WT,
    score_involvements_wt  = EPV_SCORE_INVOLVEMENTS_WT,
    intercepts_wt          = EPV_INTERCEPTS_WT,
    one_percenters_wt      = EPV_ONE_PERCENTERS_WT,
    rebound50s_wt          = EPV_REBOUND50S_WT,
    frees_against_wt       = EPV_FREES_AGAINST_WT,
    clearances_wt          = EPV_CLEARANCES_WT,
    frees_for_wt           = EPV_FREES_FOR_WT,
    goals_wt               = EPV_GOALS_WT,
    behinds_wt             = EPV_BEHINDS_WT,
    marks_wt               = EPV_MARKS_WT,
    uncontested_poss_wt    = EPV_UNCONTESTED_POSS_WT,
    shots_at_goal_wt       = EPV_SHOTS_AT_GOAL_WT,
    kicks_wt               = EPV_KICKS_WT,
    handballs_wt           = EPV_HANDBALLS_WT,
    metres_gained_wt       = EPV_METRES_GAINED_WT,
    turnovers_wt           = EPV_TURNOVERS_WT,
    goal_assists_wt        = EPV_GOAL_ASSISTS_WT,
    pos_adj_quantile_recv   = EPV_POS_ADJ_QUANTILE_RECV,
    pos_adj_quantile_disp   = EPV_POS_ADJ_QUANTILE_DISP,
    pos_adj_quantile_spoil  = EPV_POS_ADJ_QUANTILE_SPOIL,
    pos_adj_quantile_hitout = EPV_POS_ADJ_QUANTILE_HITOUT
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
#' @param decay Decay factor for time-weighting games. Default is \code{EPR_DECAY_DEFAULT_DAYS} (486).
#' @param epv_params Named list of EPV assignment parameters. If NULL,
#'   uses \code{default_epv_params()}.
#'
#' @return A data.table with one row per player per match, containing:
#'   identifiers (\code{player_id}, \code{match_id}, \code{season}, \code{round},
#'   \code{player_name}, \code{team}, \code{opponent}, \code{listed_position}, \code{position}, \code{team_id},
#'   \code{utc_start_time}), position-adjusted EPV (\code{epv_adj},
#'   \code{recv_epv_adj}, \code{disp_epv_adj}, \code{spoil_epv_adj}, \code{hitout_epv_adj}),
#'   raw EPV (\code{epv}, \code{recv_epv}, \code{disp_epv}, \code{spoil_epv},
#'   \code{hitout_epv}), and key box-score stats.
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
                                    decay = EPR_DECAY_DEFAULT_DAYS,
                                    epv_params = NULL) {

  p <- if (is.null(epv_params)) default_epv_params() else epv_params

  if (is.null(pbp_data)) pbp_data <- load_pbp(TRUE)
  if (is.null(player_stats)) player_stats <- load_player_stats(TRUE)

  if (is.null(teams)) teams <- load_teams(TRUE)

  # Compute a single reference date for consistent decay weights across all data sources
  ref_date <- max(as.Date(pbp_data$utc_start_time), na.rm = TRUE)

  # --- Steps 1-3: PBP aggregation using data.table for performance ---
  # Convert once and compute shared columns (avoids two full dplyr scans of PBP)
  dt <- data.table::as.data.table(pbp_data)
  data.table::setorder(dt, match_id, display_order)
  dt[, `:=`(
    weight_gm = exp(as.numeric(-(ref_date - as.Date(utc_start_time))) / decay),
    opp_tm = data.table::fifelse(home_away == "Home", away_team_name, home_team_name)
  )]

  # Step 1: Disposal points (grouped by player_id + match_id)
  disp_dt <- dt[, .(
    player_name = max(player_name, na.rm = TRUE),
    utc_start_time = max(utc_start_time),
    weight_gm = max(weight_gm),
    disp_epv = sum(data.table::fifelse(pos_team == -1, delta_epv + p$disp_neg_offset, delta_epv + p$disp_pos_offset) * p$disp_scale),
    disposals_pbp = floor(.N / 2L),
    team = team[.N],
    opponent = opp_tm[.N],
    listed_position = player_position[.N],
    round = as.numeric(round_week[.N]),
    season = lubridate::year(utc_start_time[.N])
  ), by = .(player_id, match_id)]

  # Step 2: Reception points (grouped by lead_player_id + match_id)
  # Intercept marks (pos_team == -1 AND mark description) get a separate scale
  dt[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
  recv_dt <- dt[, .(
    recv_epv = sum(data.table::fifelse(
      is_intercept_mark,
      ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) * p$recv_intercept_mark_scale,
      data.table::fifelse(
        pos_team == -1L,
        ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) * p$recv_scale,
        ((p$recv_pos_mult * delta_epv * pos_team) + p$recv_pos_offset) * p$recv_scale
      )
    )),
    receptions = .N
  ), by = .(lead_player_id, match_id)]
  dt[, is_intercept_mark := NULL]

  # Step 3: Join disposal + reception
  plyr_gm_df <- merge(disp_dt, recv_dt,
    by.x = c("player_id", "match_id"),
    by.y = c("lead_player_id", "match_id"),
    all.x = TRUE, sort = FALSE)

  # --- Step 4: Join spoils/tackles/hitouts from raw player_stats ---
  spoil_hitout_df <- player_stats |>
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(ref_date - as.Date(utc_start_time))) / decay),
      spoil_epv = spoils * p$spoil_wt + tackles * p$tackle_wt + pressure_acts * p$pressure_wt + def_half_pressure_acts * p$def_pressure_wt +
                  intercepts * p$intercepts_wt + one_percenters * p$one_percenters_wt + rebound50s * p$rebound50s_wt + frees_against * p$frees_against_wt,
      hitout_epv = hitouts * p$hitout_wt + hitouts_to_advantage * p$hitout_adv_wt + ruck_contests * p$ruck_contest_wt +
                   clearances * p$clearances_wt
    ) |>
    dplyr::select(-dplyr::any_of(c("utc_start_time", "player_name", "given_name", "surname",
                                   "player_captain", "player_jumper_number", "player_photo_url",
                                   "home_team_name", "away_team_name", "last_updated",
                                   "team_status")))

  plyr_gm_df <- plyr_gm_df |>
    dplyr::left_join(
      spoil_hitout_df,
      by = c("player_id" = "player_id", "match_id" = "match_id")
    )

  # Assert join produced matches (catches upstream schema changes)
  if (all(is.na(plyr_gm_df$spoil_epv))) {
    cli::cli_abort(c(
      "Player stats join produced no matches - all spoil/hitout points are zero.",
      "i" = "Check that {.fn load_player_stats} returns the expected column names."
    ))
  }

  # --- Step 5: Replace NAs and compute totals ---
  plyr_gm_df <- plyr_gm_df |>
    dplyr::mutate(
      recv_epv = tidyr::replace_na(recv_epv, 0) +
                 contested_possessions * p$contested_poss_wt + contested_marks * p$contested_marks_wt +
                 ground_ball_gets * p$ground_ball_gets_wt + marks_inside50 * p$marks_inside50_wt +
                 marks * p$marks_wt + uncontested_possessions * p$uncontested_poss_wt +
                 frees_for * p$frees_for_wt,
      disp_epv = tidyr::replace_na(disp_epv, 0) + bounces * p$bounce_wt +
                 inside50s * p$inside50s_wt + clangers * p$clangers_wt + score_involvements * p$score_involvements_wt +
                 kicks * p$kicks_wt + handballs * p$handballs_wt + metres_gained * p$metres_gained_wt +
                 turnovers * p$turnovers_wt + goal_assists * p$goal_assists_wt +
                 goals * p$goals_wt + behinds * p$behinds_wt + shots_at_goal * p$shots_at_goal_wt,
      spoil_epv = tidyr::replace_na(spoil_epv, 0),
      hitout_epv = tidyr::replace_na(hitout_epv, 0),
      epv = recv_epv + disp_epv + spoil_epv + hitout_epv
    )

  # --- Step 6: Join teams data for position ---
  teams_pos <- teams |>
    dplyr::select(match_id, player_id, position) |>
    dplyr::distinct()
  plyr_gm_df <- plyr_gm_df |>
    dplyr::select(-dplyr::any_of("position")) |>
    dplyr::left_join(teams_pos, by = c("match_id", "player_id")) |>
    dplyr::mutate(
      position = dplyr::if_else(position == "MIDFIELDER_FORWARD", "MEDIUM_FORWARD", position)
    )

  # --- Step 7: Per-80 normalisation then position-group quantile adjustment ---
  # Normalise to per-full-game rate BEFORE position adjustment so the quantile
  # compares like-for-like rates, not raw totals that mix ability with TOG.
  plyr_gm_df <- plyr_gm_df |>
    dplyr::mutate(
      tog_safe = pmax(.data$time_on_ground_percentage / 100, 0.1),
      recv_epv_p80 = .data$recv_epv / .data$tog_safe,
      disp_epv_p80 = .data$disp_epv / .data$tog_safe,
      spoil_epv_p80 = .data$spoil_epv / .data$tog_safe,
      hitout_epv_p80 = .data$hitout_epv / .data$tog_safe
    ) |>
    dplyr::group_by(position) |>
    dplyr::mutate(
      recv_epv_adj = .data$recv_epv_p80 - mean(.data$recv_epv_p80, na.rm = TRUE),
      disp_epv_adj = .data$disp_epv_p80 - mean(.data$disp_epv_p80, na.rm = TRUE),
      spoil_epv_adj = .data$spoil_epv_p80 - mean(.data$spoil_epv_p80, na.rm = TRUE),
      hitout_epv_adj = .data$hitout_epv_p80 - mean(.data$hitout_epv_p80, na.rm = TRUE),
      epv_adj = .data$recv_epv_adj + .data$disp_epv_adj + .data$spoil_epv_adj + .data$hitout_epv_adj
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"tog_safe", -"recv_epv_p80", -"disp_epv_p80", -"spoil_epv_p80", -"hitout_epv_p80")

  # --- Step 8: Handle duplicate season columns and select final columns ---
  if ("season.x" %in% names(plyr_gm_df)) {
    plyr_gm_df <- plyr_gm_df |> dplyr::mutate(season = season.x)
  }

  plyr_gm_df <- plyr_gm_df |>
    dplyr::filter(!is.na(team)) |>
    dplyr::select(
      # Identifiers
      player_id, match_id, season, round,
      player_name, team, opponent, listed_position, position, team_id,
      utc_start_time,
      # EPV (position-adjusted)
      epv_adj, recv_epv_adj, disp_epv_adj, spoil_epv_adj, hitout_epv_adj,
      # EPV (raw)
      epv, recv_epv, disp_epv, spoil_epv, hitout_epv,
      # PBP-derived action counts
      disposals_pbp, receptions,
      # EPV model input stats
      spoils, tackles, pressure_acts,
      def_half_pressure_acts,
      hitouts, hitouts_to_advantage, ruck_contests,
      bounces,
      # Core box-score stats
      goals, behinds, kicks, handballs, disposals, marks,
      contested_possessions, uncontested_possessions,
      inside50s, marks_inside50, contested_marks,
      clearances,
      metres_gained, time_on_ground_percentage,
      intercepts, rebound50s, one_percenters,
      frees_for, frees_against, clangers, turnovers,
      score_involvements, shots_at_goal, goal_assists,
      ground_ball_gets
    )

  return(plyr_gm_df)
}

#' @rdname default_epv_params
#' @export
default_credit_params <- default_epv_params
