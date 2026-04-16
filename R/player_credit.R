#' Default EPV assignment parameters
#'
#' Returns a named list of all EPV assignment parameters with their default
#' values. Used by \code{create_player_game_data()} when no custom params are provided.
#'
#' @return A named list of EPV assignment parameters.
#' @keywords internal
default_epv_params <- function() {
  list(
    bounce_wt         = EPV_BOUNCE_WT,
    disp_neg_offset   = EPV_DISP_NEG_OFFSET,
    disp_pos_offset   = EPV_DISP_POS_OFFSET,
    disp_scale        = EPV_DISP_SCALE,
    recv_neg_mult     = EPV_RECV_NEG_MULT,
    recv_neg_offset   = EPV_RECV_NEG_OFFSET,
    recv_pos_mult     = EPV_RECV_POS_MULT,
    recv_pos_offset   = EPV_RECV_POS_OFFSET,
    recv_scale        = EPV_RECV_SCALE,
    recv_intercept_mark_scale = EPV_RECV_INTERCEPT_MARK_SCALE,
    recv_failed_contest_wt = EPV_RECV_FAILED_CONTEST_WT,
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
    goal_assists_wt        = EPV_GOAL_ASSISTS_WT
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
#' @param chains Raw chains data from \code{load_chains()}. If NULL, loads all
#'   available. Used to compute failed reception credit from aerial contests.
#' @param decay Decay factor for time-weighting games. Default is \code{EPR_DECAY_DEFAULT_DAYS} (486).
#' @param epv_params Named list of EPV assignment parameters. If NULL,
#'   uses \code{default_epv_params()}.
#'
#' @return A data.table with one row per player per match, containing:
#'   identifiers (\code{player_id}, \code{match_id}, \code{season}, \code{round},
#'   \code{player_name}, \code{team}, \code{opponent}, \code{position_group}, \code{lineup_position}, \code{team_id},
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
                                    chains = NULL,
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
  # For contested kicks (contest_target_id is non-NA), reduce disposal scale
  # from 50% to contest_share (1/3) — the remaining credit goes to target/defender.
  # Backward compat: if contest_target_id column doesn't exist in older PBP, use
  # full disp_scale for all rows.
  contest_share <- p$contest_share %||% (1 / 3)
  has_contest_col <- "contest_target_id" %in% names(dt)
  dt[, .disp_scale := if (has_contest_col) {
    data.table::fifelse(!is.na(contest_target_id), contest_share, p$disp_scale)
  } else {
    p$disp_scale
  }]
  disp_dt <- dt[, .(
    player_name = max(player_name, na.rm = TRUE),
    utc_start_time = max(utc_start_time),
    weight_gm = max(weight_gm),
    disp_epv = sum(data.table::fifelse(pos_team == -1, delta_epv + p$disp_neg_offset, delta_epv + p$disp_pos_offset) * .disp_scale),
    disposals_pbp = floor(.N / 2L),
    team = team[.N],
    opponent = opp_tm[.N],
    position_group = player_position[.N],
    round = as.numeric(round_week[.N]),
    season = lubridate::year(utc_start_time[.N])
  ), by = .(player_id, match_id)]
  dt[, .disp_scale := NULL]

  # Step 2: Reception points (grouped by lead_player_id + match_id)
  # Exclude rows where lead_desc is a contest target — those plays are credited
  # via contest_epv instead (avoids double-counting the 3-way split)
  # Exclude contested kicks from recv_epv: if contest_target_id is set on a row,

  # it's a 3-player contest handled by contest_epv instead
  has_contest_col <- "contest_target_id" %in% names(dt)
  dt[, is_contest_target_recv := has_contest_col & !is.na(contest_target_id)]
  dt[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
  recv_dt <- dt[is_contest_target_recv == FALSE, .(
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
  dt[, c("is_intercept_mark", "is_contest_target_recv") := NULL]

  # Step 3: Join disposal + reception
  plyr_gm_df <- merge(disp_dt, recv_dt,
    by.x = c("player_id", "match_id"),
    by.y = c("lead_player_id", "match_id"),
    all.x = TRUE, sort = FALSE)

  # --- Step 3b: WPA credit ---
  wp_dt <- tryCatch({
    wpc <- create_wp_credit(pbp_data)
    wpc[, .(player_id, match_id, wp_credit, wp_disp_credit, wp_recv_credit)]
  }, error = function(e) {
    cli::cli_warn("WPA credit skipped: {conditionMessage(e)}")
    data.table::data.table(
      player_id = character(), match_id = character(),
      wp_credit = numeric(), wp_disp_credit = numeric(), wp_recv_credit = numeric()
    )
  })
  plyr_gm_df <- merge(plyr_gm_df, wp_dt,
    by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)

  # --- Step 3c: Contest credit from aerial contests (3-way EPV split) ---
  contest_dt <- tryCatch({
    if (is.null(chains)) chains <- load_chains(TRUE)
    compute_contest_credit(chains, pbp_data,
                           contest_share = p$contest_share %||% (1 / 3))
  }, error = function(e) {
    is_data_unavailable <- grepl(
      "load_chains|download|HTTP|connection|404|timeout",
      conditionMessage(e), ignore.case = TRUE
    )
    if (!is_data_unavailable) {
      cli::cli_abort("Contest credit computation failed: {conditionMessage(e)}")
    }
    cli::cli_warn("Contest credit skipped (data unavailable): {conditionMessage(e)}")
    data.table::data.table(
      player_id = character(), match_id = character(),
      contest_epv = numeric(),
      aerial_target_wins = integer(), aerial_target_losses = integer(),
      aerial_def_wins = integer(), aerial_def_losses = integer()
    )
  })
  plyr_gm_df <- merge(plyr_gm_df, contest_dt,
    by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)

  # --- Step 4: Join spoils/tackles/hitouts from raw player_stats ---
  spoil_hitout_df <- player_stats |>
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(ref_date - as.Date(utc_start_time))) / decay),
      spoil_epv = spoils * p$spoil_wt + tackles * p$tackle_wt + pressure_acts * p$pressure_wt + def_half_pressure_acts * p$def_pressure_wt +
                  intercepts * p$intercepts_wt + one_percenters * p$one_percenters_wt + rebound50s * p$rebound50s_wt + frees_against * p$frees_against_wt,
      hitout_epv = hitouts * p$hitout_wt + hitouts_to_advantage * p$hitout_adv_wt + ruck_contests * p$ruck_contest_wt
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
  # Zero-fill all box-score stats before weighted sums to prevent NA propagation
  box_score_cols <- c(
    "contested_possessions", "contested_marks", "ground_ball_gets",
    "marks_inside50", "marks", "uncontested_possessions", "frees_for",
    "inside50s", "clangers", "score_involvements", "kicks", "handballs",
    "metres_gained", "turnovers", "goal_assists", "goals", "behinds",
    "shots_at_goal"
  )
  for (col in intersect(box_score_cols, names(plyr_gm_df))) {
    plyr_gm_df[[col]] <- tidyr::replace_na(plyr_gm_df[[col]], 0)
  }

  plyr_gm_df <- plyr_gm_df |>
    dplyr::mutate(
      contest_epv = tidyr::replace_na(contest_epv, 0),
      aerial_target_wins = as.integer(tidyr::replace_na(aerial_target_wins, 0)),
      aerial_target_losses = as.integer(tidyr::replace_na(aerial_target_losses, 0)),
      aerial_def_wins = as.integer(tidyr::replace_na(aerial_def_wins, 0)),
      aerial_def_losses = as.integer(tidyr::replace_na(aerial_def_losses, 0)),
      recv_epv = tidyr::replace_na(recv_epv, 0) + contest_epv +
                 contested_possessions * p$contested_poss_wt + contested_marks * p$contested_marks_wt +
                 ground_ball_gets * p$ground_ball_gets_wt + marks_inside50 * p$marks_inside50_wt +
                 marks * p$marks_wt + uncontested_possessions * p$uncontested_poss_wt +
                 frees_for * p$frees_for_wt,
      disp_epv = tidyr::replace_na(disp_epv, 0) +
                 inside50s * p$inside50s_wt + clangers * p$clangers_wt + score_involvements * p$score_involvements_wt +
                 kicks * p$kicks_wt + handballs * p$handballs_wt + metres_gained * p$metres_gained_wt +
                 turnovers * p$turnovers_wt + goal_assists * p$goal_assists_wt +
                 goals * p$goals_wt + behinds * p$behinds_wt + shots_at_goal * p$shots_at_goal_wt,
      spoil_epv = tidyr::replace_na(spoil_epv, 0),
      hitout_epv = tidyr::replace_na(hitout_epv, 0),
      epv = recv_epv + disp_epv + spoil_epv + hitout_epv,
      wp_credit = tidyr::replace_na(wp_credit, 0),
      wp_disp_credit = tidyr::replace_na(wp_disp_credit, 0),
      wp_recv_credit = tidyr::replace_na(wp_recv_credit, 0)
    )

  # --- Step 6: Join teams data for lineup_position (20-way AFL lineup role) ---
  # teams_data$position is the ~20-way lineup role (HBFR, FB, INT, SUB, ...),
  # distinct from position_group (6-way KEY_DEFENDER / MIDFIELDER / etc.) set
  # at line 131 from PBP's player_position.
  teams_pos <- teams |>
    dplyr::select(match_id, player_id, lineup_position = "position") |>
    dplyr::distinct()
  plyr_gm_df <- plyr_gm_df |>
    dplyr::select(-dplyr::any_of(c("position", "lineup_position"))) |>
    dplyr::left_join(teams_pos, by = c("match_id", "player_id")) |>
    dplyr::mutate(
      position_group = dplyr::if_else(.data$position_group == "MIDFIELDER_FORWARD",
                                      "MEDIUM_FORWARD", .data$position_group)
    )

  # --- Step 7: Per-80 normalisation then position-group adjustment ---
  # Normalise to per-full-game rate BEFORE position adjustment so the adjustment
  # compares like-for-like rates, not raw totals that mix ability with TOG.
  # Group on position_group (6-way class), NOT lineup_position (20-way role) —
  # the 20-way groups are too granular and produce noisy / singleton means.
  plyr_gm_df <- plyr_gm_df |>
    dplyr::mutate(
      tog_safe = pmax(dplyr::coalesce(.data$time_on_ground_percentage / 100, 0.1), 0.1),
      recv_epv_p80 = .data$recv_epv / .data$tog_safe,
      disp_epv_p80 = .data$disp_epv / .data$tog_safe,
      spoil_epv_p80 = .data$spoil_epv / .data$tog_safe,
      hitout_epv_p80 = .data$hitout_epv / .data$tog_safe,
      wp_credit_p80 = .data$wp_credit / .data$tog_safe,
      wp_disp_credit_p80 = .data$wp_disp_credit / .data$tog_safe,
      wp_recv_credit_p80 = .data$wp_recv_credit / .data$tog_safe
    ) |>
    dplyr::group_by(position_group) |>
    dplyr::mutate(
      recv_epv_adj = (.data$recv_epv_p80 - stats::weighted.mean(.data$recv_epv_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe,
      disp_epv_adj = (.data$disp_epv_p80 - stats::weighted.mean(.data$disp_epv_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe,
      spoil_epv_adj = (.data$spoil_epv_p80 - stats::weighted.mean(.data$spoil_epv_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe,
      hitout_epv_adj = (.data$hitout_epv_p80 - stats::weighted.mean(.data$hitout_epv_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe,
      epv_adj = .data$recv_epv_adj + .data$disp_epv_adj + .data$spoil_epv_adj + .data$hitout_epv_adj,
      wp_credit_adj = (.data$wp_credit_p80 - stats::weighted.mean(.data$wp_credit_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe,
      wp_disp_credit_adj = (.data$wp_disp_credit_p80 - stats::weighted.mean(.data$wp_disp_credit_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe,
      wp_recv_credit_adj = (.data$wp_recv_credit_p80 - stats::weighted.mean(.data$wp_recv_credit_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"tog_safe",
                  -"recv_epv_p80", -"disp_epv_p80", -"spoil_epv_p80", -"hitout_epv_p80",
                  -"wp_credit_p80", -"wp_disp_credit_p80", -"wp_recv_credit_p80")

  # --- Step 8: Handle duplicate season columns and select final columns ---
  if ("season.x" %in% names(plyr_gm_df)) {
    plyr_gm_df <- plyr_gm_df |> dplyr::mutate(season = season.x)
  }

  plyr_gm_df <- plyr_gm_df |>
    dplyr::filter(!is.na(team)) |>
    dplyr::select(
      # Identifiers
      player_id, match_id, season, round,
      player_name, team, opponent, position_group, lineup_position, team_id,
      utc_start_time,
      # EPV (position-adjusted)
      epv_adj, recv_epv_adj, disp_epv_adj, spoil_epv_adj, hitout_epv_adj,
      # EPV (raw)
      epv, recv_epv, disp_epv, spoil_epv, hitout_epv,
      # WPA (position-adjusted)
      wp_credit_adj, wp_disp_credit_adj, wp_recv_credit_adj,
      # WPA (raw)
      wp_credit, wp_disp_credit, wp_recv_credit,
      # Contest credit (3-way aerial split)
      contest_epv, aerial_target_wins, aerial_target_losses,
      aerial_def_wins, aerial_def_losses,
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
      ground_ball_gets,
      # Efficiency stats
      dplyr::any_of(c("effective_disposals", "effective_kicks",
                       "disposal_efficiency", "kick_efficiency"))
    )

  return(plyr_gm_df)
}

#' Compute Contest Credit from Aerial Contests
#'
#' Joins aerial contest data back to PBP to get the kicker's \code{delta_epv},
#' then splits credit three ways: kicker, target, and defender. When an opponent
#' is involved (spoil, intercept mark), each gets 1/3 of the EPV at stake.
#' The target and defender receive credit from their own team's perspective
#' (positive if they won, negative if they lost).
#'
#' Only applies to contests with a 3rd player from the opposing team. When
#' the target takes the mark themselves (no opponent), the standard 50/50
#' kicker/receiver split is unchanged.
#'
#' @param chains Raw chains data (from \code{load_chains()}).
#' @param pbp_data Clean PBP data (from \code{load_pbp()}) containing
#'   \code{delta_epv} values.
#' @param contest_share Fraction of \code{delta_epv} attributed to each
#'   contest participant. Default \code{1/3}.
#'
#' @return A data.table with columns: \code{player_id}, \code{match_id},
#'   \code{contest_epv} (positive for winners, negative for losers),
#'   \code{aerial_target_wins}, \code{aerial_target_losses},
#'   \code{aerial_def_wins}, \code{aerial_def_losses}.
#'
#' @keywords internal
compute_contest_credit <- function(chains, pbp_data, contest_share = 1 / 3) {
  empty_dt <- data.table::data.table(
    player_id = character(), match_id = character(),
    contest_epv = numeric(),
    aerial_target_wins = integer(), aerial_target_losses = integer(),
    aerial_def_wins = integer(), aerial_def_losses = integer()
  )

  chains_dt <- data.table::as.data.table(chains)
  pbp_dt <- data.table::as.data.table(pbp_data)

  detect_chains_columns(chains_dt)

  target_descs <- CHAINS_CONTEST_TARGET_DESCS
  kick_descs <- c("Kick", "Ground Kick")
  data.table::setorder(chains_dt, match_id, display_order)

  # Build shift columns for forward (outcome) and backward (kicker) lookup
  chains_dt[, `:=`(
    .next_desc = data.table::shift(description, 1L, type = "lead"),
    .next_pid  = data.table::shift(player_id, 1L, type = "lead"),
    .next_tid  = data.table::shift(team_id, 1L, type = "lead"),
    .next_x    = data.table::shift(x, 1L, type = "lead"),
    .next_y    = data.table::shift(y, 1L, type = "lead"),
    .lag1_desc = data.table::shift(description, 1L, type = "lag"),
    .lag2_desc = data.table::shift(description, 2L, type = "lag"),
    .lag3_desc = data.table::shift(description, 3L, type = "lag"),
    .lag4_desc = data.table::shift(description, 4L, type = "lag"),
    .lag5_desc = data.table::shift(description, 5L, type = "lag"),
    .lag1_do   = data.table::shift(display_order, 1L, type = "lag"),
    .lag2_do   = data.table::shift(display_order, 2L, type = "lag"),
    .lag3_do   = data.table::shift(display_order, 3L, type = "lag"),
    .lag4_do   = data.table::shift(display_order, 4L, type = "lag"),
    .lag5_do   = data.table::shift(display_order, 5L, type = "lag")
  ), by = match_id]

  # Filter to contest target rows with opposing-team outcome at same x,y
  contests <- chains_dt[
    description %in% target_descs &
    !is.na(player_id) &
    !is.na(.next_tid) &
    x == .next_x & y == .next_y &
    team_id != .next_tid &
    !is.na(.next_pid)
  ]

  if (nrow(contests) == 0) {
    chains_dt[, grep("^\\.", names(chains_dt), value = TRUE) := NULL]
    return(empty_dt)
  }

  # Find the kicker's display_order (first Kick/Ground Kick within 5 rows back)
  contests[, kick_display_order := data.table::fcase(
    .lag1_desc %chin% kick_descs, .lag1_do,
    .lag2_desc %chin% kick_descs, .lag2_do,
    .lag3_desc %chin% kick_descs, .lag3_do,
    .lag4_desc %chin% kick_descs, .lag4_do,
    .lag5_desc %chin% kick_descs, .lag5_do,
    default = NA_integer_
  )]

  # Build triples table (drop rows without a matched kick)
  triples_dt <- contests[!is.na(kick_display_order), .(
    match_id,
    kick_display_order,
    target_player_id = player_id,
    target_team_id = team_id,
    defender_player_id = .next_pid,
    defender_team_id = .next_tid,
    outcome_desc = .next_desc
  )]

  # Clean up temp columns
  chains_dt[, grep("^\\.", names(chains_dt), value = TRUE) := NULL]

  if (nrow(triples_dt) == 0) return(empty_dt)

  # Join to PBP to get delta_epv from the kicker's row
  triples_dt <- merge(
    triples_dt,
    pbp_dt[, .(match_id, display_order, delta_epv)],
    by.x = c("match_id", "kick_display_order"),
    by.y = c("match_id", "display_order"),
    all.x = TRUE, sort = FALSE
  )
  # Drop rows without delta_epv (shouldn't happen but safety)
  triples_dt <- triples_dt[!is.na(delta_epv)]
  if (nrow(triples_dt) == 0) return(empty_dt)

  # Compute credit: delta_epv is from kicker's team perspective
  # Target (same team as kicker): credit = delta_epv * share
  # Defender (opp team): credit = -delta_epv * share (flip perspective)
  share <- contest_share

  # Build per-player rows: one for target, one for defender
  target_credit <- triples_dt[, .(
    player_id = target_player_id,
    match_id = match_id,
    contest_epv = delta_epv * share,
    is_target = TRUE
  )]
  defender_credit <- triples_dt[, .(
    player_id = defender_player_id,
    match_id = match_id,
    contest_epv = -delta_epv * share,
    is_target = FALSE
  )]
  all_credit <- data.table::rbindlist(list(target_credit, defender_credit))

  # Aggregate per player per match
  all_credit[, .(
    contest_epv = sum(contest_epv),
    aerial_target_wins = sum(is_target & contest_epv > 0),
    aerial_target_losses = sum(is_target & contest_epv <= 0),
    aerial_def_wins = sum(!is_target & contest_epv > 0),
    aerial_def_losses = sum(!is_target & contest_epv <= 0)
  ), by = .(player_id, match_id)]
}


#' Backward-compatible wrapper (deprecated)
#' @keywords internal
compute_failed_recv_credit <- function(chains,
                                       weight_per_loss = EPV_RECV_FAILED_CONTEST_WT) {
  cli::cli_warn("Use {.fn compute_contest_credit} instead of {.fn compute_failed_recv_credit}")
  contests <- extract_contests(chains = chains, type = "aerial")
  if (nrow(contests) == 0) {
    return(data.table::data.table(
      player_id = character(), match_id = character(),
      failed_recv_epv = numeric(), failed_receptions = integer()
    ))
  }
  failed <- contests[outcome %in% c("spoil", "intercept_mark") & winner == "player2"]
  if (nrow(failed) == 0) {
    return(data.table::data.table(
      player_id = character(), match_id = character(),
      failed_recv_epv = numeric(), failed_receptions = integer()
    ))
  }
  failed[, .(failed_recv_epv = .N * weight_per_loss, failed_receptions = .N),
         by = .(player_id = player1_id, match_id)]
}


#' @rdname default_epv_params
#' @keywords internal
default_credit_params <- default_epv_params
