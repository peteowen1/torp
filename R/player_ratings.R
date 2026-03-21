#' Load player details with previous-season fallback
#'
#' @param season_val Season year to load.
#' @return A data frame of player details.
#' @keywords internal
.load_player_details_with_fallback <- function(season_val) {
  plyr_tm_df <- load_player_details(season_val)
  if (nrow(plyr_tm_df) == 0 || !"season" %in% names(plyr_tm_df)) {
    plyr_tm_df <- load_player_details(season_val - 1)
  }
  plyr_tm_df
}

#' Calculate EPR (Expected Possession Rating)
#'
#' Calculates EPR ratings for players based on their EPV credit contributions,
#' with exponential decay weighting and Bayesian shrinkage.
#'
#' @param season_val The season to calculate ratings for. Default is the current season.
#' @param round_val The round to calculate ratings for. Default is the next round.
#' @param decay_recv Decay factor (days) for receiving component. Default is \code{EPR_DECAY_RECV}.
#' @param decay_disp Decay factor (days) for disposal component. Default is \code{EPR_DECAY_DISP}.
#' @param decay_spoil Decay factor (days) for spoil component. Default is \code{EPR_DECAY_SPOIL}.
#' @param decay_hitout Decay factor (days) for hitout component. Default is \code{EPR_DECAY_HITOUT}.
#' @param loading The loading factor for EPR calculations. Default is \code{EPR_LOADING_DEFAULT}.
#' @param prior_games_recv The number of prior games to consider for receiving. Default is \code{EPR_PRIOR_GAMES_RECV}.
#' @param prior_games_disp The number of prior games to consider for disposal. Default is \code{EPR_PRIOR_GAMES_DISP}.
#' @param plyr_tm_df Optional pre-loaded player team data. If NULL, will load automatically.
#' @param player_game_data Optional pre-loaded player game data. If NULL, will load automatically.
#' @param prior_games_spoil Prior games for spoil shrinkage. Default is \code{EPR_PRIOR_GAMES_SPOIL}.
#' @param prior_games_hitout Prior games for hitout shrinkage. Default is \code{EPR_PRIOR_GAMES_HITOUT}.
#' @param fixtures Optional pre-loaded fixtures data. If NULL, will load automatically.
#' @param prior_rate_recv Prior rate for receiving shrinkage target. Default is \code{EPR_PRIOR_RATE_RECV}.
#' @param prior_rate_disp Prior rate for disposal shrinkage target. Default is \code{EPR_PRIOR_RATE_DISP}.
#' @param prior_rate_spoil Prior rate for spoil shrinkage target. Default is \code{EPR_PRIOR_RATE_SPOIL}.
#' @param prior_rate_hitout Prior rate for hitout shrinkage target. Default is \code{EPR_PRIOR_RATE_HITOUT}.
#' @param skills Controls TOG-weighted average adjustment. When active, EPR
#'   components are re-centered to "above TOG-weighted average" by subtracting
#'   the weighted mean of each component (weighted by pred_tog =
#'   \code{squad_selection_rating * cond_tog_rating}). Accepts:
#'   \itemize{
#'     \item \code{TRUE} (default): auto-loads stat ratings via
#'       \code{get_player_stat_ratings(current = FALSE)}.
#'     \item A data.frame with \code{player_id},
#'       \code{cond_tog_rating}, and \code{squad_selection_rating} columns.
#'     \item \code{FALSE} or \code{NULL}: skip adjustment.
#'   }
#'   Players not in \code{skills} default to weight 0 (excluded from the
#'   average, but still have the average subtracted).
#'
#' @return A data frame containing player EPR ratings.
#' @export
#'
#' @importFrom dplyr filter summarise pull ungroup mutate group_by n_distinct arrange left_join select
#' @importFrom lubridate as_date decimal_date
#' @importFrom cli cli_abort
#' @importFrom utils data
calculate_epr <- function(season_val = get_afl_season(type = "current"),
                         round_val = get_afl_week(type = "next"),
                         decay_recv = EPR_DECAY_RECV,
                         decay_disp = EPR_DECAY_DISP,
                         decay_spoil = EPR_DECAY_SPOIL,
                         decay_hitout = EPR_DECAY_HITOUT,
                         loading = EPR_LOADING_DEFAULT,
                         prior_games_recv = EPR_PRIOR_GAMES_RECV,
                         prior_games_disp = EPR_PRIOR_GAMES_DISP,
                         plyr_tm_df = NULL,
                         player_game_data = NULL,
                         prior_games_spoil = EPR_PRIOR_GAMES_SPOIL,
                         prior_games_hitout = EPR_PRIOR_GAMES_HITOUT,
                         fixtures = NULL,
                         skills = TRUE,
                         prior_rate_recv = EPR_PRIOR_RATE_RECV,
                         prior_rate_disp = EPR_PRIOR_RATE_DISP,
                         prior_rate_spoil = EPR_PRIOR_RATE_SPOIL,
                         prior_rate_hitout = EPR_PRIOR_RATE_HITOUT) {


  # Resolve skills: TRUE → auto-load, FALSE/NULL → skip, data.frame → validate
  if (isTRUE(skills)) {
    skills <- tryCatch(
      get_player_stat_ratings(current = FALSE),
      error = function(e) {
        cli::cli_warn("Could not load skills data, skipping TOG adjustment: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(skills) && !"cond_tog_rating" %in% names(skills)) {
      # Backwards compat: accept _skill suffix as _rating
      if ("cond_tog_skill" %in% names(skills)) {
        skills$cond_tog_rating <- skills$cond_tog_skill
      } else if ("time_on_ground_skill" %in% names(skills)) {
        skills$cond_tog_rating <- skills$time_on_ground_skill
      } else {
        cli::cli_warn("Skills data missing {.field cond_tog_rating} column, skipping TOG adjustment.")
        skills <- NULL
      }
    }
    if (!is.null(skills) && !"squad_selection_rating" %in% names(skills)) {
      if ("squad_selection_skill" %in% names(skills)) {
        skills$squad_selection_rating <- skills$squad_selection_skill
      }
    }
  } else if (isFALSE(skills)) {
    skills <- NULL
  } else if (!is.null(skills)) {
    if (!"cond_tog_rating" %in% names(skills)) {
      if ("cond_tog_skill" %in% names(skills)) {
        skills$cond_tog_rating <- skills$cond_tog_skill
      } else if ("time_on_ground_skill" %in% names(skills)) {
        skills$cond_tog_rating <- skills$time_on_ground_skill
      } else {
        cli::cli_abort("{.arg skills} must contain a {.field cond_tog_rating} column.")
      }
    }
    if (!"squad_selection_rating" %in% names(skills)) {
      if ("squad_selection_skill" %in% names(skills)) {
        skills$squad_selection_rating <- skills$squad_selection_skill
      }
    }
  }

  # Load player team details if not provided
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- .load_player_details_with_fallback(season_val)
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
    plyr_gm_df_rnd <- calculate_epr_stats(player_game_data, match_ref, date_val, decay_recv = decay_recv, decay_disp = decay_disp, decay_spoil = decay_spoil, decay_hitout = decay_hitout, loading = loading, prior_games_recv = prior_games_recv, prior_games_disp = prior_games_disp, prior_games_spoil = prior_games_spoil, prior_games_hitout = prior_games_hitout, prior_rate_recv = prior_rate_recv, prior_rate_disp = prior_rate_disp, prior_rate_spoil = prior_rate_spoil, prior_rate_hitout = prior_rate_hitout)

    # Ensure decomposed TOG columns exist (needed by .prepare_final_dataframe)
    plyr_gm_df_rnd[, pred_tog := NA_real_]
    plyr_gm_df_rnd[, pred_selection := NA_real_]
    plyr_gm_df_rnd[, pred_cond_tog := NA_real_]

    # Attach decomposed TOG from skills (centering happens after roster filter)
    if (!is.null(skills)) {
      skills_dt <- data.table::as.data.table(skills)
      plyr_gm_df_rnd[skills_dt, `:=`(
        pred_selection = i.squad_selection_rating,
        pred_cond_tog = i.cond_tog_rating
      ), on = "player_id"]
      plyr_gm_df_rnd[is.na(pred_selection), pred_selection := 0]
      plyr_gm_df_rnd[is.na(pred_cond_tog), pred_cond_tog := 0]
      plyr_gm_df_rnd[, pred_tog := pred_selection * pred_cond_tog]
    }

    final_df <- .prepare_final_dataframe(plyr_tm_df, plyr_gm_df_rnd, season_val, round_val, fixtures)

    # TOG-weighted centering: scale pred_tog to n_teams * 18 (full-game equivalents)
    # across rostered players, then re-center each TORP component to "above/below average"
    if (!is.null(skills) && nrow(final_df) > 0) {
      final_df$pred_tog[is.na(final_df$pred_tog)] <- 0
      tot_tog <- sum(final_df$pred_tog)
      if (tot_tog > 0) {
        n_teams <- length(unique(final_df$team))
        target_tog <- n_teams * 18L
        final_df$pred_tog <- final_df$pred_tog * (target_tog / tot_tog)

        comps <- c("recv_epr", "disp_epr", "spoil_epr", "hitout_epr")
        for (comp in comps) {
          avg_val <- sum(final_df[[comp]] * final_df$pred_tog, na.rm = TRUE) / sum(final_df$pred_tog)
          final_df[[comp]] <- final_df[[comp]] - avg_val
        }
        final_df$epr <- round(final_df$recv_epr + final_df$disp_epr + final_df$spoil_epr + final_df$hitout_epr, 2)
        for (comp in comps) {
          final_df[[comp]] <- round(final_df[[comp]], 2)
        }
      }
    }

    cli::cli_inform("EPR ratings as at {season_val} round {round_val}")
    return(final_df)
  }
}

#' Bayesian shrinkage formula
#'
#' Computes the posterior mean for a single EPR component:
#' \code{(loading * sum_val + prior_games * prior_rate) / (wt_gms + prior_games)}.
#'
#' @param sum_val Numeric vector of weighted sums.
#' @param wt_gms Numeric vector of weighted games.
#' @param loading Loading factor.
#' @param prior_games Prior games shrinkage strength.
#' @param prior_rate Prior rate (shrinkage target).
#' @return Numeric vector of posterior means.
#' @keywords internal
.bayesian_shrink <- function(sum_val, wt_gms, loading, prior_games, prior_rate) {
  (loading * sum_val + prior_games * prior_rate) / (wt_gms + prior_games)
}

#' Calculate EPR statistics per player
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
#' @param prior_games_spoil Prior games for spoil shrinkage. Default is \code{EPR_PRIOR_GAMES_SPOIL}.
#' @param prior_games_hitout Prior games for hitout shrinkage. Default is \code{EPR_PRIOR_GAMES_HITOUT}.
#' @param prior_rate_recv Prior rate for receiving shrinkage target. Default is \code{EPR_PRIOR_RATE_RECV}.
#' @param prior_rate_disp Prior rate for disposal shrinkage target. Default is \code{EPR_PRIOR_RATE_DISP}.
#' @param prior_rate_spoil Prior rate for spoil shrinkage target. Default is \code{EPR_PRIOR_RATE_SPOIL}.
#' @param prior_rate_hitout Prior rate for hitout shrinkage target. Default is \code{EPR_PRIOR_RATE_HITOUT}.
#'
#' @return A data frame with calculated EPR statistics per player
#'
#' @importFrom data.table as.data.table uniqueN setDT
calculate_epr_stats <- function(player_game_data = NULL, match_ref, date_val, decay_recv = EPR_DECAY_RECV, decay_disp = EPR_DECAY_DISP, decay_spoil = EPR_DECAY_SPOIL, decay_hitout = EPR_DECAY_HITOUT, loading = EPR_LOADING_DEFAULT, prior_games_recv = EPR_PRIOR_GAMES_RECV, prior_games_disp = EPR_PRIOR_GAMES_DISP, prior_games_spoil = EPR_PRIOR_GAMES_SPOIL, prior_games_hitout = EPR_PRIOR_GAMES_HITOUT, prior_rate_recv = EPR_PRIOR_RATE_RECV, prior_rate_disp = EPR_PRIOR_RATE_DISP, prior_rate_spoil = EPR_PRIOR_RATE_SPOIL, prior_rate_hitout = EPR_PRIOR_RATE_HITOUT) {
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
    recv_sum   = sum(recv_epv_adj * wt_recv, na.rm = TRUE),
    disp_sum   = sum(disp_epv_adj * wt_disp, na.rm = TRUE),
    spoil_sum  = sum(spoil_epv_adj * wt_spoil, na.rm = TRUE),
    hitout_sum = sum(hitout_epv_adj * wt_hitout, na.rm = TRUE),
    posn = data.table::last(listed_position)
  ), by = player_id]

  # Derive per-component EPR with per-component wt_gms
  result[, `:=`(
    wt_gms = wt_gms_recv,  # backwards-compat alias; per-component cols are canonical
    wt_tog = round(tog_sum / pmax(wt_gms_recv, 1e-10), 1),
    recv_epr   = .bayesian_shrink(recv_sum,   wt_gms_recv,   loading, prior_games_recv,   prior_rate_recv),
    disp_epr   = .bayesian_shrink(disp_sum,   wt_gms_disp,   loading, prior_games_disp,   prior_rate_disp),
    spoil_epr  = .bayesian_shrink(spoil_sum,  wt_gms_spoil,  loading, prior_games_spoil,  prior_rate_spoil),
    hitout_epr = .bayesian_shrink(hitout_sum, wt_gms_hitout, loading, prior_games_hitout, prior_rate_hitout)
  )]

  # Compute final epr
  result[, epr := round(recv_epr + disp_epr + spoil_epr + hitout_epr, 2)]

  # Remove intermediate columns
  result[, c("tog_sum", "recv_sum", "disp_sum", "spoil_sum", "hitout_sum") := NULL]

  return(result)
}

#' Batch calculate EPR statistics for multiple rounds
#'
#' Vectorized version of \code{\link{calculate_epr_stats}} that processes
#' all rounds in a single data.table non-equi join operation. This avoids
#' repeated subset + aggregate calls per round.
#'
#' @param player_game_data Keyed data.table of player game data (keyed on match_id).
#' @param round_info data.table with columns: \code{round_val} (integer round
#'   number), \code{match_ref} (character match ID cutoff), \code{date_val}
#'   (Date for decay weight reference).
#' @inheritParams calculate_epr_stats
#'
#' @return A data.table with a \code{round_val} column and the same per-player
#'   columns as \code{calculate_epr_stats} output.
#' @keywords internal
calculate_epr_stats_batch <- function(player_game_data = NULL,
                                         round_info,
                                         decay_recv = EPR_DECAY_RECV,
                                         decay_disp = EPR_DECAY_DISP,
                                         decay_spoil = EPR_DECAY_SPOIL,
                                         decay_hitout = EPR_DECAY_HITOUT,
                                         loading = EPR_LOADING_DEFAULT,
                                         prior_games_recv = EPR_PRIOR_GAMES_RECV,
                                         prior_games_disp = EPR_PRIOR_GAMES_DISP,
                                         prior_games_spoil = EPR_PRIOR_GAMES_SPOIL,
                                         prior_games_hitout = EPR_PRIOR_GAMES_HITOUT,
                                         prior_rate_recv = EPR_PRIOR_RATE_RECV,
                                         prior_rate_disp = EPR_PRIOR_RATE_DISP,
                                         prior_rate_spoil = EPR_PRIOR_RATE_SPOIL,
                                         prior_rate_hitout = EPR_PRIOR_RATE_HITOUT) {
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
    recv_sum   = sum(recv_epv_adj * wt_recv, na.rm = TRUE),
    disp_sum   = sum(disp_epv_adj * wt_disp, na.rm = TRUE),
    spoil_sum  = sum(spoil_epv_adj * wt_spoil, na.rm = TRUE),
    hitout_sum = sum(hitout_epv_adj * wt_hitout, na.rm = TRUE),
    posn = data.table::last(listed_position)
  ), by = .(round_val, player_id)]

  result[, `:=`(
    wt_gms = wt_gms_recv,
    wt_tog = round(tog_sum / pmax(wt_gms_recv, 1e-10), 1),
    recv_epr   = .bayesian_shrink(recv_sum,   wt_gms_recv,   loading, prior_games_recv,   prior_rate_recv),
    disp_epr   = .bayesian_shrink(disp_sum,   wt_gms_disp,   loading, prior_games_disp,   prior_rate_disp),
    spoil_epr  = .bayesian_shrink(spoil_sum,  wt_gms_spoil,  loading, prior_games_spoil,  prior_rate_spoil),
    hitout_epr = .bayesian_shrink(hitout_sum, wt_gms_hitout, loading, prior_games_hitout, prior_rate_hitout)
  )]

  result[, epr := round(recv_epr + disp_epr + spoil_epr + hitout_epr, 2)]
  result[, c("tog_sum", "recv_sum", "disp_sum", "spoil_sum", "hitout_sum") := NULL]

  return(result)
}

#' Prepare final dataframe
#'
#' @param plyr_tm_df Player team database
#' @param player_game_data Player statistics dataframe (aggregated from calculate_epr_stats)
#' @param season_val Season value
#' @param round_val Round value
#' @param fixtures Optional pre-loaded fixtures data. If NULL, will load automatically.
#' @param fix_summary Optional pre-computed fixtures summary (season, round, ref_date).
#'   If NULL, computed from \code{fixtures} each call. Pass this when calling in a loop
#'   to avoid redundant summarisation.
#'
#' @return A final dataframe with player ratings
#' @keywords internal
#'
#' @importFrom dplyr filter left_join ungroup mutate select arrange
#' @importFrom utils data
.prepare_final_dataframe <- function(plyr_tm_df = NULL, player_game_data = NULL, season_val, round_val, fixtures = NULL, fix_summary = NULL) {
  if (is.null(plyr_tm_df)) {
    plyr_tm_df <- .load_player_details_with_fallback(season_val)
    season_val_details <- if ("season" %in% names(plyr_tm_df)) {
      plyr_tm_df$season[1]
    } else {
      season_val
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
    dplyr::left_join(fix_summary, by = c("season", "round")) |>
    dplyr::mutate(
      age = lubridate::decimal_date(lubridate::as_date(.data$ref_date)) -
        lubridate::decimal_date(lubridate::as_date(.data$date_of_birth))
    ) |>
    dplyr::select(
      player_id = "player_id", player_name = "player_name.x", age = "age", team = "team",
      epr = "epr", recv_epr = "recv_epr", disp_epr = "disp_epr",
      spoil_epr = "spoil_epr", hitout_epr = "hitout_epr",
      position = "position", season = "season", round = "round", gms = "gms", wt_gms = "wt_gms", wt_tog = "wt_tog",
      wt_gms_recv = "wt_gms_recv", wt_gms_disp = "wt_gms_disp",
      wt_gms_spoil = "wt_gms_spoil", wt_gms_hitout = "wt_gms_hitout",
      pred_tog = "pred_tog",
      pred_selection = "pred_selection",
      pred_cond_tog = "pred_cond_tog"
    ) |>
    dplyr::arrange(-.data$epr)
}

#' Calculate TORP (Total Over Replacement Predictive-value)
#'
#' Blends EPR (Expected Possession Rating) with PSR (Player Skill Rating)
#' to produce the final TORP rating. TORP = w * epr + (1 - w) * psr.
#'
#' @param epr_df Data frame with EPR ratings (must contain \code{player_id}, \code{epr}).
#' @param psr_df Data frame with PSR ratings (must contain \code{player_id}, \code{psr}).
#' @param epr_weight Weight for EPR in the blend. Default is \code{TORP_EPR_WEIGHT} (0.5).
#'
#' @return A data frame with all EPR columns plus \code{psr} and \code{torp} (blended).
#' @export
calculate_torp <- function(epr_df, psr_df, epr_weight = TORP_EPR_WEIGHT) {
  psr_cols <- intersect(c("player_id", "season", "round", "psr", "osr", "dsr"), names(psr_df))

  # Take latest PSR per player to avoid duplicate joins
  latest_psr <- psr_df |>
    dplyr::select(dplyr::all_of(psr_cols)) |>
    dplyr::arrange(player_id, season, round) |>
    dplyr::group_by(player_id) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c("season", "round")))

  # Remove pre-existing psr/osr/dsr/torp columns (and any .x/.y suffixed
  # duplicates from prior joins) to prevent column name collisions when
  # epr_df was loaded from a previous pipeline run that already blended PSR
  stale_cols <- grep("^(psr|osr|dsr|torp)(\\.|$)", names(epr_df), value = TRUE)
  if (length(stale_cols) > 0) {
    epr_df <- epr_df[, setdiff(names(epr_df), stale_cols), drop = FALSE]
  }

  result <- dplyr::left_join(epr_df, latest_psr, by = "player_id")

  # Ensure psr column exists even if join didn't add it
  if (!"psr" %in% names(result)) {
    cli::cli_warn("PSR column missing after join \u2014 defaulting all players to prior rate")
    result$psr <- PSR_PRIOR_RATE
  }
  result$psr[is.na(result$psr)] <- PSR_PRIOR_RATE
  if ("osr" %in% names(result)) result$osr[is.na(result$osr)] <- PSR_PRIOR_RATE / 2
  if ("dsr" %in% names(result)) result$dsr[is.na(result$dsr)] <- PSR_PRIOR_RATE / 2
  result$torp <- round(epr_weight * result$epr + (1 - epr_weight) * result$psr, 2)
  dplyr::arrange(result, -.data$torp)
}



#' EPR Ratings (Expected Possession Rating)
#'
#' Convenience alias for \code{\link{calculate_epr}}. Returns possession-value
#' based ratings only (no PSR or TORP blend).
#'
#' @inheritParams calculate_epr
#'
#' @return A data frame with columns: \code{player_id}, \code{player_name},
#'   \code{epr}, \code{recv_epr}, \code{disp_epr}, \code{spoil_epr},
#'   \code{hitout_epr}, plus metadata columns.
#'
#' @seealso \code{\link{torp_ratings}} for the full blended rating,
#'   \code{\link{psr_ratings}} for skill ratings only.
#'
#' @examples
#' \dontrun{
#'   epr_df <- epr_ratings(2026, 1)
#' }
#'
#' @export
epr_ratings <- calculate_epr


#' Deprecated: use \code{\link{calculate_epr}} or \code{\link{torp_ratings}}
#'
#' @param ... Arguments passed to \code{\link{calculate_epr}}.
#' @return See \code{\link{calculate_epr}}.
#' @keywords internal
#' @export
calculate_torp_ratings <- function(...) {
  cli::cli_warn("{.fn calculate_torp_ratings} is deprecated. Use {.fn calculate_epr} for EPR-only or {.fn torp_ratings} for the full TORP blend.")
  calculate_epr(...)
}


#' PSR Ratings (Player Skill Rating)
#'
#' Computes skill-based ratings for each player-round: \code{psr} (margin-based
#' total), plus \code{osr} (offensive) and \code{dsr} (defensive) components
#' that sum to \code{psr}.
#'
#' @param season_val Season to compute PSR for. Default is current season.
#' @param round_val Round to filter to. If NULL (default), returns all rounds.
#' @param psr_coef_path Optional path to the margin PSR coefficient CSV.
#'   If NULL, uses the bundled \code{inst/extdata/psr_v2_coefficients.csv}.
#'
#' @return A data.table with columns: \code{player_id}, \code{player_name},
#'   \code{season}, \code{round}, \code{pos_group}, \code{psr_raw},
#'   \code{psr}, \code{osr}, \code{dsr}.
#'
#' @seealso \code{\link{torp_ratings}} for the full blended rating,
#'   \code{\link{epr_ratings}} for possession-value ratings only.
#'
#' @examples
#' \dontrun{
#'   psr_df <- psr_ratings(2026, 1)
#'   psr_df <- psr_ratings(2026)      # all rounds
#' }
#'
#' @export
psr_ratings <- function(season_val = get_afl_season(type = "current"),
                        round_val = get_afl_week(type = "next"),
                        psr_coef_path = NULL) {
  skills <- load_player_stat_ratings(season_val)
  result <- .compute_psr_from_stat_ratings(skills, psr_coef_path)
  if (is.null(result)) {
    return(data.table::data.table(
      player_id = character(), player_name = character(),
      season = integer(), round = integer(),
      pos_group = character(), psr = numeric(), osr = numeric(), dsr = numeric()
    ))
  }
  if (!is.null(round_val)) {
    result <- result[result$round == round_val, ]
  }
  result[, psr_raw := NULL]
  result[order(-psr)]
}


#' TORP Ratings (Total Over Replacement Predictive-value)
#'
#' High-level convenience function that computes the full TORP rating for
#' each player: EPR (possession-value rating) + PSR (skill rating) blended
#' together. Returns \code{epr}, \code{psr}, \code{osr}, \code{dsr}, and
#' \code{torp} columns.
#'
#' @param season_val Season to calculate ratings for.
#' @param round_val Round to calculate ratings for.
#' @param ... Additional arguments passed to \code{\link{calculate_epr}}.
#'
#' @return A data frame with columns: \code{player_id}, \code{player_name},
#'   \code{epr}, \code{recv_epr}, \code{disp_epr}, \code{spoil_epr},
#'   \code{hitout_epr}, \code{psr}, \code{osr}, \code{dsr}, \code{torp},
#'   plus metadata columns.
#'
#' @seealso \code{\link{epr_ratings}} for possession-value ratings only,
#'   \code{\link{psr_ratings}} for skill ratings only.
#'
#' @examples
#' \dontrun{
#'   tr <- torp_ratings(2026, 1)
#' }
#'
#' @export
torp_ratings <- function(season_val = get_afl_season(type = "current"),
                         round_val = get_afl_week(type = "next"), ...) {
  # Step 1: EPR
  epr_df <- suppressMessages(calculate_epr(season_val, round_val, ...))

  # Step 2: PSR with osr/dsr decomposition (only load current season skills)
  psr_df <- tryCatch({
    skills <- load_player_stat_ratings(season_val)
    .compute_psr_from_stat_ratings(skills)
  }, error = function(e) {
    cli::cli_warn("Could not compute PSR: {e$message} -- returning EPR-only ratings (no TORP blend)")
    NULL
  })

  if (is.null(psr_df)) {
    epr_df$torp <- epr_df$epr
    epr_df$psr <- PSR_PRIOR_RATE
    epr_df$has_psr <- FALSE
    return(epr_df)
  }

  # Step 3: Blend and reorder columns so torp is visible
  cli::cli_inform("TORP ratings as at {season_val} round {round_val}")
  result <- calculate_torp(epr_df, psr_df)

  # Step 4: Join current injuries
  injuries <- tryCatch(get_all_injuries(season_val, scrape = TRUE), error = function(e) NULL)
  result <- match_injuries(result, injuries)

  front_cols <- c("player_id", "player_name", "age", "team", "torp", "epr", "psr",
                  "osr", "dsr", "injury", "estimated_return")
  front_cols <- intersect(front_cols, names(result))
  other_cols <- setdiff(names(result), front_cols)
  result <- result[order(-result$torp), c(front_cols, other_cols)]
  result
}
