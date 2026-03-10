#' Get TORP ratings
#'
#' This function calculates TORP (Total Overall Rating Points) for players based on their performance.
#'
#' @param season_val The season to calculate ratings for. Default is the next season.
#' @param round_val The round to calculate ratings for. Default is the next round.
#' @param decay_recv Decay factor (days) for receiving component. Default is \code{RATING_DECAY_RECV}.
#' @param decay_disp Decay factor (days) for disposal component. Default is \code{RATING_DECAY_DISP}.
#' @param decay_spoil Decay factor (days) for spoil component. Default is \code{RATING_DECAY_SPOIL}.
#' @param decay_hitout Decay factor (days) for hitout component. Default is \code{RATING_DECAY_HITOUT}.
#' @param loading The loading factor for TORP calculations. Default is \code{RATING_LOADING_DEFAULT}.
#' @param prior_games_recv The number of prior games to consider for receiving. Default is \code{RATING_PRIOR_GAMES_RECV}.
#' @param prior_games_disp The number of prior games to consider for disposal. Default is \code{RATING_PRIOR_GAMES_DISP}.
#' @param plyr_tm_df Optional pre-loaded player team data. If NULL, will load automatically.
#' @param player_game_data Optional pre-loaded player game data. If NULL, will load automatically.
#' @param prior_games_spoil Prior games for spoil shrinkage. Default is \code{RATING_PRIOR_GAMES_SPOIL}.
#' @param prior_games_hitout Prior games for hitout shrinkage. Default is \code{RATING_PRIOR_GAMES_HITOUT}.
#' @param fixtures Optional pre-loaded fixtures data. If NULL, will load automatically.
#' @param prior_rate_recv Prior rate for receiving shrinkage target. Default is \code{RATING_PRIOR_RATE_RECV}.
#' @param prior_rate_disp Prior rate for disposal shrinkage target. Default is \code{RATING_PRIOR_RATE_DISP}.
#' @param prior_rate_spoil Prior rate for spoil shrinkage target. Default is \code{RATING_PRIOR_RATE_SPOIL}.
#' @param prior_rate_hitout Prior rate for hitout shrinkage target. Default is \code{RATING_PRIOR_RATE_HITOUT}.
#' @param skills Controls TOG-weighted average adjustment. When active, TORP
#'   components are re-centered to "above TOG-weighted average" by subtracting
#'   the weighted mean of each component (weighted by pred_tog =
#'   \code{squad_selection_skill * cond_tog_skill}). Accepts:
#'   \itemize{
#'     \item \code{TRUE} (default): auto-loads skills via
#'       \code{get_player_skills(current = FALSE)}.
#'     \item A data.frame with \code{player_id},
#'       \code{cond_tog_skill}, and \code{squad_selection_skill} columns.
#'     \item \code{FALSE} or \code{NULL}: skip adjustment.
#'   }
#'   Players not in \code{skills} default to weight 0 (excluded from the
#'   average, but still have the average subtracted).
#'
#' @return A data frame containing player TORP ratings.
#' @export
#'
#' @importFrom dplyr filter summarise pull ungroup mutate group_by n_distinct arrange left_join select
#' @importFrom lubridate as_date decimal_date
#' @importFrom cli cli_abort
#' @importFrom utils data
calculate_torp_ratings <- function(season_val = get_afl_season(type = "current"),
                         round_val = get_afl_week(type = "next"),
                         decay_recv = RATING_DECAY_RECV,
                         decay_disp = RATING_DECAY_DISP,
                         decay_spoil = RATING_DECAY_SPOIL,
                         decay_hitout = RATING_DECAY_HITOUT,
                         loading = RATING_LOADING_DEFAULT,
                         prior_games_recv = RATING_PRIOR_GAMES_RECV,
                         prior_games_disp = RATING_PRIOR_GAMES_DISP,
                         plyr_tm_df = NULL,
                         player_game_data = NULL,
                         prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
                         prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
                         fixtures = NULL,
                         skills = TRUE,
                         prior_rate_recv = RATING_PRIOR_RATE_RECV,
                         prior_rate_disp = RATING_PRIOR_RATE_DISP,
                         prior_rate_spoil = RATING_PRIOR_RATE_SPOIL,
                         prior_rate_hitout = RATING_PRIOR_RATE_HITOUT) {


  # Resolve skills: TRUE → auto-load, FALSE/NULL → skip, data.frame → validate
  if (isTRUE(skills)) {
    skills <- tryCatch(
      get_player_skills(current = FALSE),
      error = function(e) {
        cli::cli_warn("Could not load skills data, skipping TOG adjustment: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(skills) && !"cond_tog_skill" %in% names(skills)) {
      # Backwards compat: accept time_on_ground_skill as cond_tog_skill
      if ("time_on_ground_skill" %in% names(skills)) {
        skills$cond_tog_skill <- skills$time_on_ground_skill
      } else {
        cli::cli_warn("Skills data missing {.field cond_tog_skill} column, skipping TOG adjustment.")
        skills <- NULL
      }
    }
  } else if (isFALSE(skills)) {
    skills <- NULL
  } else if (!is.null(skills)) {
    if (!"cond_tog_skill" %in% names(skills)) {
      if ("time_on_ground_skill" %in% names(skills)) {
        skills$cond_tog_skill <- skills$time_on_ground_skill
      } else {
        cli::cli_abort("{.arg skills} must contain a {.field cond_tog_skill} column.")
      }
    }
  }

  # Load player team details if not provided
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val)
    # Fall back to previous season if current season data unavailable
    if (nrow(plyr_tm_df) == 0 || !"season" %in% names(plyr_tm_df)) {
      plyr_tm_df <- load_player_details(season_val - 1)
    }
  }

  # Load player game data if not provided
  if (is.null(player_game_data)) {
    player_game_data <- load_player_game_data(TRUE)
  }

  # Load fixtures if not provided
  if (is.null(fixtures)) {
    fixtures <- load_fixtures(TRUE)
  }

  gwk <- sprintf("%02d", round_val)
  match_ref <- paste0("CD_M", season_val, "014", gwk)

  date_val <- fixtures |>
    dplyr::filter(.data$season == season_val, .data$round_number == round_val) |>
    dplyr::summarise(lubridate::as_date(min(.data$utc_start_time))) |>
    dplyr::pull()

  if (is.na(date_val)) {
    cli::cli_warn("Fixtures for this date not available yet")
    return(data.frame())
  } else {
    plyr_gm_df_rnd <- calculate_player_stats(player_game_data, match_ref, date_val, decay_recv = decay_recv, decay_disp = decay_disp, decay_spoil = decay_spoil, decay_hitout = decay_hitout, loading = loading, prior_games_recv = prior_games_recv, prior_games_disp = prior_games_disp, prior_games_spoil = prior_games_spoil, prior_games_hitout = prior_games_hitout, prior_rate_recv = prior_rate_recv, prior_rate_disp = prior_rate_disp, prior_rate_spoil = prior_rate_spoil, prior_rate_hitout = prior_rate_hitout)

    # Ensure decomposed TOG columns exist (needed by prepare_final_dataframe)
    plyr_gm_df_rnd[, pred_tog := NA_real_]
    plyr_gm_df_rnd[, pred_selection := NA_real_]
    plyr_gm_df_rnd[, pred_cond_tog := NA_real_]

    # Attach decomposed TOG from skills (centering happens after roster filter)
    if (!is.null(skills)) {
      skills_dt <- data.table::as.data.table(skills)
      plyr_gm_df_rnd[skills_dt, `:=`(
        pred_selection = i.squad_selection_skill,
        pred_cond_tog = i.cond_tog_skill
      ), on = "player_id"]
      plyr_gm_df_rnd[is.na(pred_selection), pred_selection := 0]
      plyr_gm_df_rnd[is.na(pred_cond_tog), pred_cond_tog := 0]
      plyr_gm_df_rnd[, pred_tog := pred_selection * pred_cond_tog]
    }

    final_df <- prepare_final_dataframe(plyr_tm_df, plyr_gm_df_rnd, season_val, round_val, fixtures)

    # TOG-weighted centering: scale pred_tog to n_teams * 18 (full-game equivalents)
    # across rostered players, then re-center each TORP component to "above/below average"
    if (!is.null(skills) && nrow(final_df) > 0) {
      final_df$pred_tog[is.na(final_df$pred_tog)] <- 0
      tot_tog <- sum(final_df$pred_tog)
      if (tot_tog > 0) {
        n_teams <- length(unique(final_df$team))
        target_tog <- n_teams * 18L
        final_df$pred_tog <- final_df$pred_tog * (target_tog / tot_tog)

        comps <- c("torp_recv", "torp_disp", "torp_spoil", "torp_hitout")
        for (comp in comps) {
          avg_val <- sum(final_df[[comp]] * final_df$pred_tog, na.rm = TRUE) / sum(final_df$pred_tog)
          final_df[[comp]] <- final_df[[comp]] - avg_val
        }
        final_df$torp <- round(final_df$torp_recv + final_df$torp_disp + final_df$torp_spoil + final_df$torp_hitout, 2)
        for (comp in comps) {
          final_df[[comp]] <- round(final_df[[comp]], 2)
        }
      }
    }

    cli::cli_inform("TORP ratings as at {season_val} round {round_val}")
    return(final_df)
  }
}

#' Calculate player statistics
#'
#' @param player_game_data Player game data frame
#' @param match_ref Match reference
#' @param date_val Date value
#' @param decay_recv Decay factor (days) for receiving component
#' @param decay_disp Decay factor (days) for disposal component
#' @param decay_spoil Decay factor (days) for spoil component
#' @param decay_hitout Decay factor (days) for hitout component
#' @param loading Loading factor
#' @param prior_games_recv Prior games for receiving
#' @param prior_games_disp Prior games for disposal
#' @param prior_games_spoil Prior games for spoil shrinkage. Default is \code{RATING_PRIOR_GAMES_SPOIL}.
#' @param prior_games_hitout Prior games for hitout shrinkage. Default is \code{RATING_PRIOR_GAMES_HITOUT}.
#' @param prior_rate_recv Prior rate for receiving shrinkage target. Default is \code{RATING_PRIOR_RATE_RECV}.
#' @param prior_rate_disp Prior rate for disposal shrinkage target. Default is \code{RATING_PRIOR_RATE_DISP}.
#' @param prior_rate_spoil Prior rate for spoil shrinkage target. Default is \code{RATING_PRIOR_RATE_SPOIL}.
#' @param prior_rate_hitout Prior rate for hitout shrinkage target. Default is \code{RATING_PRIOR_RATE_HITOUT}.
#'
#' @return A data frame with calculated player statistics
#'
#' @importFrom data.table as.data.table uniqueN setDT
calculate_player_stats <- function(player_game_data = NULL, match_ref, date_val, decay_recv = RATING_DECAY_RECV, decay_disp = RATING_DECAY_DISP, decay_spoil = RATING_DECAY_SPOIL, decay_hitout = RATING_DECAY_HITOUT, loading = RATING_LOADING_DEFAULT, prior_games_recv = RATING_PRIOR_GAMES_RECV, prior_games_disp = RATING_PRIOR_GAMES_DISP, prior_games_spoil = RATING_PRIOR_GAMES_SPOIL, prior_games_hitout = RATING_PRIOR_GAMES_HITOUT, prior_rate_recv = RATING_PRIOR_RATE_RECV, prior_rate_disp = RATING_PRIOR_RATE_DISP, prior_rate_spoil = RATING_PRIOR_RATE_SPOIL, prior_rate_hitout = RATING_PRIOR_RATE_HITOUT) {
  if (is.null(player_game_data)) {
    player_game_data <- load_player_game_data(TRUE)
  }

  # Subset to relevant matches — avoid full copy if already a keyed data.table
  if (data.table::is.data.table(player_game_data) &&
      identical(data.table::key(player_game_data), "match_id")) {
    # Binary search on existing key, no copy of full dataset
    dt <- player_game_data[match_id <= match_ref]
  } else {
    dt <- data.table::as.data.table(player_game_data)
    data.table::setkey(dt, match_id)
    dt <- dt[match_id <= match_ref]
  }
  dt[, days_diff := as.numeric(as.Date(date_val) - as.Date(utc_start_time))]
  n_future <- sum(dt$days_diff < 0, na.rm = TRUE)
  if (n_future > 0) {
    cli::cli_warn("Filtering {n_future} row{?s} with future dates (days_diff < 0)")
    dt <- dt[days_diff >= 0]
  }
  dt[, `:=`(
    wt_recv   = exp(-days_diff / decay_recv),
    wt_disp   = exp(-days_diff / decay_disp),
    wt_spoil  = exp(-days_diff / decay_spoil),
    wt_hitout = exp(-days_diff / decay_hitout)
  )]

  # Handle player_name: use existing if present, otherwise construct from given + surname
  if (!"player_name" %in% names(dt)) {
    dt[, player_name := paste(player_given_name, player_surname)]
  }

  # Aggregate by player_id using data.table syntax
  # Per-80 normalisation is already applied in create_player_game_data() (before
  # position adjustment), so _adj columns are already per-full-game rates.
  # Each component uses its own decay weight for both sums and wt_gms.
  # Note: player_game_data has one row per player-match, so wt_gms = sum(wt).
  result <- dt[, .(
    player_name = max(player_name),
    gms = .N,
    wt_gms_recv   = sum(wt_recv, na.rm = TRUE),
    wt_gms_disp   = sum(wt_disp, na.rm = TRUE),
    wt_gms_spoil  = sum(wt_spoil, na.rm = TRUE),
    wt_gms_hitout = sum(wt_hitout, na.rm = TRUE),
    tog_sum    = sum(time_on_ground_percentage * wt_recv, na.rm = TRUE),
    recv_sum   = sum(recv_credits_adj * wt_recv, na.rm = TRUE),
    disp_sum   = sum(disp_credits_adj * wt_disp, na.rm = TRUE),
    spoil_sum  = sum(spoil_credits_adj * wt_spoil, na.rm = TRUE),
    hitout_sum = sum(hitout_credits_adj * wt_hitout, na.rm = TRUE),
    posn = data.table::last(listed_position)
  ), by = player_id]

  # Derive per-component TORP with per-component wt_gms
  result[, `:=`(
    wt_gms = wt_gms_recv,  # backwards-compat alias; per-component cols are canonical
    wt_tog = round(tog_sum / pmax(wt_gms_recv, 1e-10), 1),
    torp_recv   = (loading * recv_sum   + prior_games_recv   * prior_rate_recv)   / (wt_gms_recv   + prior_games_recv),
    torp_disp   = (loading * disp_sum   + prior_games_disp   * prior_rate_disp)   / (wt_gms_disp   + prior_games_disp),
    torp_spoil  = (loading * spoil_sum  + prior_games_spoil  * prior_rate_spoil)  / (wt_gms_spoil  + prior_games_spoil),
    torp_hitout = (loading * hitout_sum + prior_games_hitout * prior_rate_hitout) / (wt_gms_hitout + prior_games_hitout)
  )]

  # Compute final torp
  result[, torp := round(torp_recv + torp_disp + torp_spoil + torp_hitout, 2)]

  # Remove intermediate columns
  result[, c("tog_sum", "recv_sum", "disp_sum", "spoil_sum", "hitout_sum") := NULL]

  return(result)
}

#' Batch calculate player statistics for multiple rounds
#'
#' Vectorized version of \code{\link{calculate_player_stats}} that processes
#' all rounds in a single data.table non-equi join operation. This avoids
#' repeated subset + aggregate calls per round.
#'
#' @param player_game_data Keyed data.table of player game data (keyed on match_id).
#' @param round_info data.table with columns: \code{round_val} (integer round
#'   number), \code{match_ref} (character match ID cutoff), \code{date_val}
#'   (Date for decay weight reference).
#' @inheritParams calculate_player_stats
#'
#' @return A data.table with a \code{round_val} column and the same per-player
#'   columns as \code{calculate_player_stats} output.
#' @keywords internal
calculate_player_stats_batch <- function(player_game_data = NULL,
                                         round_info,
                                         decay_recv = RATING_DECAY_RECV,
                                         decay_disp = RATING_DECAY_DISP,
                                         decay_spoil = RATING_DECAY_SPOIL,
                                         decay_hitout = RATING_DECAY_HITOUT,
                                         loading = RATING_LOADING_DEFAULT,
                                         prior_games_recv = RATING_PRIOR_GAMES_RECV,
                                         prior_games_disp = RATING_PRIOR_GAMES_DISP,
                                         prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
                                         prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
                                         prior_rate_recv = RATING_PRIOR_RATE_RECV,
                                         prior_rate_disp = RATING_PRIOR_RATE_DISP,
                                         prior_rate_spoil = RATING_PRIOR_RATE_SPOIL,
                                         prior_rate_hitout = RATING_PRIOR_RATE_HITOUT) {
  if (is.null(player_game_data)) {
    player_game_data <- load_player_game_data(TRUE)
  }

  dt <- if (data.table::is.data.table(player_game_data)) {
    player_game_data
  } else {
    data.table::as.data.table(player_game_data)
  }

  if (!identical(data.table::key(dt), "match_id")) {
    data.table::setkey(dt, match_id)
  }

  ri <- data.table::as.data.table(round_info)

  # data.table non-equi joins require numeric columns for <= operator,
  # so map character match_ids to integer indices for the join
  all_mids <- unique(dt$match_id)  # already sorted (dt is keyed)
  if (!"match_idx" %in% names(dt)) {
    dt[, match_idx := match(match_id, all_mids)]
  }
  ri[, match_idx_max := vapply(match_ref, function(ref) sum(all_mids <= ref), integer(1))]

  # Non-equi join on numeric index: for each round, get all games
  # where match_idx <= match_idx_max (equivalent to match_id <= match_ref).
  cross <- dt[ri, on = .(match_idx <= match_idx_max), allow.cartesian = TRUE, nomatch = NULL]

  cross[, days_diff := as.numeric(as.Date(date_val) - as.Date(utc_start_time))]
  cross <- cross[days_diff >= 0]

  cross[, `:=`(
    wt_recv   = exp(-days_diff / decay_recv),
    wt_disp   = exp(-days_diff / decay_disp),
    wt_spoil  = exp(-days_diff / decay_spoil),
    wt_hitout = exp(-days_diff / decay_hitout)
  )]

  if (!"player_name" %in% names(cross)) {
    cross[, player_name := paste(player_given_name, player_surname)]
  }

  # Aggregate by (round_val, player_id) — all rounds in one pass
  result <- cross[, .(
    player_name = max(player_name),
    gms = .N,
    wt_gms_recv   = sum(wt_recv, na.rm = TRUE),
    wt_gms_disp   = sum(wt_disp, na.rm = TRUE),
    wt_gms_spoil  = sum(wt_spoil, na.rm = TRUE),
    wt_gms_hitout = sum(wt_hitout, na.rm = TRUE),
    tog_sum    = sum(time_on_ground_percentage * wt_recv, na.rm = TRUE),
    recv_sum   = sum(recv_credits_adj * wt_recv, na.rm = TRUE),
    disp_sum   = sum(disp_credits_adj * wt_disp, na.rm = TRUE),
    spoil_sum  = sum(spoil_credits_adj * wt_spoil, na.rm = TRUE),
    hitout_sum = sum(hitout_credits_adj * wt_hitout, na.rm = TRUE),
    posn = data.table::last(listed_position)
  ), by = .(round_val, player_id)]

  result[, `:=`(
    wt_gms = wt_gms_recv,
    wt_tog = round(tog_sum / pmax(wt_gms_recv, 1e-10), 1),
    torp_recv   = (loading * recv_sum   + prior_games_recv   * prior_rate_recv)   / (wt_gms_recv   + prior_games_recv),
    torp_disp   = (loading * disp_sum   + prior_games_disp   * prior_rate_disp)   / (wt_gms_disp   + prior_games_disp),
    torp_spoil  = (loading * spoil_sum  + prior_games_spoil  * prior_rate_spoil)  / (wt_gms_spoil  + prior_games_spoil),
    torp_hitout = (loading * hitout_sum + prior_games_hitout * prior_rate_hitout) / (wt_gms_hitout + prior_games_hitout)
  )]

  result[, torp := round(torp_recv + torp_disp + torp_spoil + torp_hitout, 2)]
  result[, c("tog_sum", "recv_sum", "disp_sum", "spoil_sum", "hitout_sum") := NULL]

  return(result)
}

#' Prepare final dataframe
#'
#' @param plyr_tm_df Player team database
#' @param player_game_data Player statistics dataframe (aggregated from calculate_player_stats)
#' @param season_val Season value
#' @param round_val Round value
#' @param fixtures Optional pre-loaded fixtures data. If NULL, will load automatically.
#' @param fix_summary Optional pre-computed fixtures summary (season, round, ref_date).
#'   If NULL, computed from \code{fixtures} each call. Pass this when calling in a loop
#'   to avoid redundant summarisation.
#'
#' @return A final dataframe with player ratings
#'
#' @importFrom dplyr filter left_join ungroup mutate select arrange
#' @importFrom utils data
prepare_final_dataframe <- function(plyr_tm_df = NULL, player_game_data = NULL, season_val, round_val, fixtures = NULL, fix_summary = NULL) {
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val)
    # Fall back to previous season if current season data unavailable
    if (nrow(plyr_tm_df) == 0 || !"season" %in% names(plyr_tm_df)) {
      plyr_tm_df <- load_player_details(season_val - 1)
      season_val_details <- season_val - 1
    } else {
      season_val_details <- season_val
    }
  } else {
    if ("season" %in% names(plyr_tm_df) && !season_val %in% plyr_tm_df$season) {
      season_val_details <- max(plyr_tm_df$season)
    } else {
      season_val_details <- season_val
    }
  }

  if (is.null(player_game_data)) {
    player_game_data <- load_player_game_data(season_val)
  }

  # Load fixtures if not provided
  if (is.null(fixtures)) {
    fixtures <- load_fixtures(TRUE)
  }

  # Pre-compute fixtures summary if not provided (avoids redundant summarise in loops)
  if (is.null(fix_summary)) {
    fix_summary <- fixtures |>
      dplyr::group_by(season = .data$season, round = .data$round_number) |>
      dplyr::summarise(ref_date = lubridate::as_date(min(.data$utc_start_time)), .groups = "drop")
  }

  plyr_tm_df |>
    dplyr::filter(.data$season == season_val_details) |>
    dplyr::left_join(player_game_data, by = c("player_id")) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      round = .env$round_val,
      season = .env$season_val
    ) |>
    dplyr::left_join(fix_summary) |>
    dplyr::mutate(
      age = lubridate::decimal_date(lubridate::as_date(.data$ref_date)) -
        lubridate::decimal_date(lubridate::as_date(.data$dateOfBirth))
    ) |>
    dplyr::select(
      player_id = "player_id", player_name = "player_name.x", age = "age", team = "team",
      torp = "torp", torp_recv = "torp_recv", torp_disp = "torp_disp",
      torp_spoil = "torp_spoil", torp_hitout = "torp_hitout",
      position = "position", season = "season", round = "round", gms = "gms", wt_gms = "wt_gms", wt_tog = "wt_tog",
      wt_gms_recv = "wt_gms_recv", wt_gms_disp = "wt_gms_disp",
      wt_gms_spoil = "wt_gms_spoil", wt_gms_hitout = "wt_gms_hitout",
      pred_tog = "pred_tog",
      pred_selection = "pred_selection",
      pred_cond_tog = "pred_cond_tog"
    ) |>
    dplyr::arrange(-.data$torp)
}

#' @rdname calculate_torp_ratings
#' @export
torp_ratings <- calculate_torp_ratings
