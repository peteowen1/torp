# Match Model Orchestration
# ========================
# Convenience wrappers and the full predictions pipeline.
# Data prep helpers are in match_data_prep.R; training in match_train.R.

# build_team_mdl_df (convenience wrapper) ----

#' Build complete match model dataset end-to-end
#'
#' Convenience wrapper chaining all internal .build_* functions.
#' Loads data, builds fixtures, ratings, features, weather, and model dataset.
#'
#' @param season Season to build for (default: current via get_afl_season())
#' @param target_weeks Numeric vector of target round numbers (used for weather forecasting)
#' @return Complete team_mdl_df ready for GAM training
#' @keywords internal
build_team_mdl_df <- function(season = NULL, target_weeks = NULL,
                              psr_coef_path = NULL) {
  if (is.null(season)) season <- get_afl_season()

  cli::cli_h2("Loading data")
  all_grounds <- file_reader("stadium_data", "reference-data")
  xg_df <- load_xg(TRUE)
  fixtures <- load_fixtures(TRUE)
  results <- load_results(TRUE)
  teams <- load_teams(TRUE)
  torp_df <- load_torp_ratings()

  # Load PSR (Player Skill Ratings) with osr/dsr decomposition
  psr_df <- NULL
  tryCatch({
    skills <- load_player_stat_ratings(TRUE)
    psr_df <- .compute_psr_from_stat_ratings(skills, psr_coef_path)
    if (!is.null(psr_df)) {
      cli::cli_inform("PSR computed for {nrow(psr_df)} player-rounds")
    }
  }, error = function(e) {
    cli::cli_warn("Failed to compute PSR: {e$message}")
  })

  cli::cli_inform(paste0(
    "Loaded: fixtures=", nrow(fixtures), ", results=", nrow(results),
    ", teams=", nrow(teams), ", ratings=", nrow(torp_df)
  ))

  # Input validation
  if (nrow(fixtures) < 100) cli::cli_abort("Fixtures too small ({nrow(fixtures)} rows)")
  if (nrow(torp_df) < 100) cli::cli_abort("Ratings too small ({nrow(torp_df)} rows)")
  if (nrow(teams) < 100) cli::cli_abort("Teams too small ({nrow(teams)} rows)")

  cli::cli_h2("Building fixture features")
  fix_df <- .build_fixtures_df(fixtures)

  cli::cli_h2("Processing lineups")
  team_rt_df <- .build_team_ratings_df(teams, torp_df, psr_df)

  cli::cli_h2("Computing features")
  team_rt_fix_df <- .build_match_features(fix_df, team_rt_df, all_grounds)

  cli::cli_h2("Loading weather")
  weather_df <- .load_match_weather(fixtures, all_grounds, target_weeks, season)

  # Weight anchor: most recent fixture date
  weight_anchor_date <- if (!is.null(target_weeks) && !is.null(season)) {
    target_fix <- fixtures |>
      dplyr::filter(season == .env$season, round_number %in% target_weeks)
    if (nrow(target_fix) > 0) as.Date(min(target_fix$utc_start_time)) else Sys.Date()
  } else {
    max(as.Date(fix_df$utc_start_time), na.rm = TRUE)
  }
  cli::cli_inform("Weight anchor date: {weight_anchor_date}")

  cli::cli_h2("Building model dataset")
  team_mdl_df <- .build_team_mdl_df(
    team_rt_fix_df, results, xg_df, weather_df, weight_anchor_date
  )

  team_mdl_df
}


# run_predictions_pipeline ----

#' Run weekly match predictions pipeline
#'
#' Average home/away rows into one match-level prediction
#'
#' Flips away-team predictions to the home-team perspective, then averages
#' home and (flipped) away predictions per match. Expects columns from
#' \code{.train_match_gams()} output (team_mdl_df).
#'
#' @param df Long-form team_mdl_df with both home and away rows per match
#' @return One-row-per-match tibble with averaged predictions from the home
#'   team perspective
#' @keywords internal
.format_match_preds <- function(df) {
  home <- df |>
    dplyr::filter(team_type_fac.x == "home") |>
    dplyr::select(
      season = season.x, round = round_number.x, players = count.x, match_id,
      home_team = team_name.x, home_epr = epr.x, home_psr = psr.x,
      away_team = team_name.y, away_epr = epr.y, away_psr = psr.y,
      pred_xtotal = pred_tot_xscore, pred_xmargin = pred_xscore_diff,
      pred_margin = pred_score_diff, pred_win, bits,
      margin = score_diff, start_time = local_start_time_str, venue = venue.x
    )
  away <- df |>
    dplyr::mutate(
      pred_xscore_diff = -pred_xscore_diff,
      pred_score_diff = -pred_score_diff,
      pred_win = 1 - pred_win,
      score_diff = -score_diff
    ) |>
    dplyr::filter(team_type_fac.x == "away") |>
    dplyr::select(
      season = season.x, round = round_number.x, players = count.x, match_id,
      home_team = team_name.y, home_epr = epr.y, home_psr = psr.y,
      away_team = team_name.x, away_epr = epr.x, away_psr = psr.x,
      pred_xtotal = pred_tot_xscore, pred_xmargin = pred_xscore_diff,
      pred_margin = pred_score_diff, pred_win, bits,
      margin = score_diff, start_time = local_start_time_str, venue = venue.x
    )
  dplyr::bind_rows(home, away) |>
    dplyr::group_by(season, round, match_id, home_team, home_epr, home_psr,
                    away_team, away_epr, away_psr, start_time, venue) |>
    dplyr::summarise(
      players = mean(players), pred_xtotal = mean(pred_xtotal),
      pred_margin = mean(pred_margin), pred_win = mean(pred_win),
      margin = mean(margin), .groups = "drop"
    ) |>
    dplyr::mutate(
      epr_diff = home_epr - away_epr,
      psr_diff = home_psr - away_psr
    ) |>
    dplyr::select(season, round, match_id:away_psr, start_time, venue,
                  epr_diff, psr_diff, players:margin)
}


#' Builds team_mdl_df with injury-adjusted ratings, trains the 5-model sequential
#' GAM pipeline, generates predictions for target weeks, and uploads to torpdata
#' releases.
#'
#' @param week Single target week (auto-detected if NULL)
#' @param weeks Vector of weeks, or "all" for all fixture weeks
#' @param season Season year (default: current via get_afl_season())
#' @return A list (invisibly) with:
#'   \item{predictions}{All match predictions across all seasons (season, round,
#'     providerId, home_team, away_team, pred_margin, pred_win, margin, etc.)}
#'   \item{models}{Named list of 5 GAM models: total_xpoints, xscore_diff,
#'     conv_diff, score_diff, win}
#' @keywords internal
run_predictions_pipeline <- function(week = NULL, weeks = NULL, season = NULL) {

  if (is.null(season)) season <- get_afl_season()

  if (!is.null(week) && !is.null(weeks)) {
    cli::cli_abort("Specify either {.arg week} or {.arg weeks}, not both")
  }
  if (is.null(week) && is.null(weeks)) week <- get_afl_week(type = "next")

  cli::cli_h1("Match Predictions Pipeline")
  .pipeline_start <- proc.time()

  # Load Data ----
  cli::cli_h2("Loading data")

  all_grounds <- file_reader("stadium_data", "reference-data")
  xg_df <- load_xg(TRUE)
  fixtures <- load_fixtures(TRUE)
  results <- load_results(TRUE)

  # Refresh current season results from AFL API
  tryCatch({
    cli::cli_progress_step("Refreshing {season} results from AFL API")
    fresh_results <- get_afl_results(season)
    if (!is.null(fresh_results) && nrow(fresh_results) > 0) {
      save_to_release(fresh_results, paste0("results_", season), "results-data")
      results <- load_results(TRUE)
      cli::cli_inform("Refreshed results: {nrow(fresh_results)} rows for {season}")
    }
  }, error = function(e) {
    cli::cli_warn("Could not refresh {season} results: {e$message}")
  })

  teams <- load_teams(TRUE)

  # Load TORP ratings with compute-from-scratch fallback
  torp_df_total <- tryCatch(load_torp_ratings(), error = function(e) {
    cli::cli_warn("Could not load TORP ratings from release: {e$message}")
    NULL
  })
  if (is.null(torp_df_total) || nrow(torp_df_total) < 100) {
    cli::cli_warn("TORP ratings unavailable or too small from release - computing from scratch (this may be slow)")
    torp_df_total <- tryCatch(
      calculate_epr(season_val = season, round_val = get_afl_week(type = "next")),
      error = function(e) {
        cli::cli_abort("Failed to compute TORP ratings from scratch: {e$message}")
      }
    )
  }

  cli::cli_inform("Loaded: fixtures={nrow(fixtures)}, results={nrow(results)}, teams={nrow(teams)}, ratings={nrow(torp_df_total)}")

  if (nrow(fixtures) < 100) cli::cli_abort("Fixtures too small ({nrow(fixtures)} rows)")
  if (nrow(torp_df_total) < 100) cli::cli_abort("Ratings too small ({nrow(torp_df_total)} rows)")
  if (nrow(teams) < 100) cli::cli_abort("Teams too small ({nrow(teams)} rows)")

  # Resolve target weeks
  is_backfill <- identical(weeks, "all")
  if (!is.null(weeks)) {
    if (is_backfill) {
      target_weeks <- sort(unique(fixtures$round_number[fixtures$season == season]))
    } else {
      target_weeks <- weeks
    }
  } else {
    target_weeks <- week
  }
  cli::cli_inform("Season: {season}, Week{?s}: {paste(target_weeks, collapse = ', ')}")

  # Weight anchor date (deterministic)
  target_fixtures <- fixtures |>
    dplyr::filter(season == .env$season, round_number %in% target_weeks)
  weight_anchor_date <- if (nrow(target_fixtures) > 0) {
    as.Date(min(target_fixtures$utc_start_time))
  } else {
    Sys.Date()
  }
  cli::cli_inform("Weight anchor date: {weight_anchor_date}")

  # Build Shared Data ----
  cli::cli_h2("Building fixture features")
  fix_df <- .build_fixtures_df(fixtures)

  cli::cli_h2("Loading PSR")
  psr_df <- NULL

  # Strategy 1: Load pre-computed PSR from torpdata releases

  tryCatch({
    psr_df <- load_psr(TRUE)
    if (nrow(psr_df) > 0) {
      cli::cli_inform("PSR loaded from release: {nrow(psr_df)} player-rounds")
    } else {
      psr_df <- NULL
    }
  }, error = function(e) {
    cli::cli_warn("Could not load PSR from release: {e$message}")
  })

  # Strategy 2: Compute from skills + coefficients (with osr/dsr decomposition)
  # Also triggered when release PSR lacks osr/dsr columns
  if (is.null(psr_df) || !all(c("osr", "dsr") %in% names(psr_df))) {
    tryCatch({
      skills <- load_player_stat_ratings(TRUE)
      psr_df <- .compute_psr_from_stat_ratings(skills)
      if (!is.null(psr_df)) {
        cli::cli_inform("PSR computed from skills+coefficients: {nrow(psr_df)} player-rounds")
      }
    }, error = function(e) {
      cli::cli_warn("Failed to compute PSR: {e$message}")
    })
  }

  if (is.null(psr_df)) {
    cli::cli_warn("PSR unavailable - predictions will proceed without PSR features")
  }

  cli::cli_h2("Processing lineups")
  team_rt_df <- .build_team_ratings_df(teams, torp_df_total, psr_df)

  cli::cli_h2("Computing features")
  team_rt_fix_df <- .build_match_features(fix_df, team_rt_df, all_grounds)

  # Injuries ----
  cli::cli_h2("Loading injuries")
  inj_df <- get_all_injuries(season)
  cli::cli_inform("Injuries loaded: {nrow(inj_df)} rows ({sum(inj_df$source == 'weekly', na.rm = TRUE)} weekly, {sum(inj_df$source == 'preseason', na.rm = TRUE)} preseason)")
  if (nrow(inj_df) == 0 && min(target_weeks) > 1) {
    cli::cli_warn("0 injuries loaded during active season - all players will be treated as available")
  }

  # Parse return rounds and save injury snapshot to torpdata release
  if (nrow(inj_df) > 0) {
    inj_df$return_round <- parse_return_round(
      inj_df$estimated_return, season, min(target_weeks)
    )
    tryCatch(
      save_injury_data(inj_df, season),
      error = function(e) cli::cli_warn("Failed to save injury data: {conditionMessage(e)}")
    )
  }

  tr <- torp_ratings(season, min(target_weeks))
  if (nrow(tr) == 0 || !"player_name" %in% names(tr)) {
    cli::cli_alert_info("No TORP ratings available for {season} R{min(target_weeks)} (pre-season or fixtures not ready) - skipping predictions")
    elapsed <- (proc.time() - .pipeline_start)[["elapsed"]]
    cli::cli_inform("Pipeline aborted after {round(elapsed, 1)}s")
    return(invisible(NULL))
  }
  # torp_ratings() already joins injuries — drop those columns before re-joining
  # with the pipeline's own injury data (which includes return_round parsing)
  tr[c("injury", "estimated_return")] <- NULL
  tr <- match_injuries(tr, inj_df)
  if (!"estimated_return" %in% names(tr)) tr$estimated_return <- NA_character_
  tr <- dplyr::mutate(tr, estimated_return = tidyr::replace_na(estimated_return, "None"))

  # Join PSR to player-level ratings for injury-adjusted weekly summary
  if (!is.null(psr_df)) {
    tr <- tr |>
      dplyr::select(-dplyr::any_of("psr")) |>
      dplyr::left_join(
        psr_df |> dplyr::select(player_id, season, round, psr),
        by = c("player_id", "season", "round")
      ) |>
      dplyr::mutate(psr = tidyr::replace_na(psr, PSR_PRIOR_RATE))
  } else {
    tr$psr <- PSR_PRIOR_RATE
  }

  # Join return_round from injury data to player ratings
  if (nrow(inj_df) > 0 && "return_round" %in% names(inj_df)) {
    inj_return <- inj_df |>
      dplyr::select(player_norm, return_round) |>
      dplyr::distinct(player_norm, .keep_all = TRUE)
    tr <- tr |>
      dplyr::mutate(player_norm = tolower(trimws(player_name))) |>
      dplyr::left_join(inj_return, by = "player_norm") |>
      dplyr::select(-player_norm)
  } else {
    tr$return_round <- NA_real_
  }

  # Build per-week roster ratings: only exclude players still injured for
  # that week (return_round > week). Players returning by the target week
  # are included. Discount scales with prediction horizon (0.99 for next
  # week, dropping 0.01 per week, floor 0.90).
  .build_week_ratings <- function(tr_data, w) {
    weeks_ahead <- max(w - min(target_weeks), 0)
    discount <- max(INJURY_KNOWN_DISCOUNT - 0.01 * weeks_ahead,
                    INJURY_DISCOUNT_FLOOR)

    tr_data |>
      dplyr::filter(
        !is.na(epr),
        is.na(injury) | (!is.na(return_round) & return_round <= w)
      ) |>
      dplyr::mutate(team_name = team) |>
      dplyr::group_by(team_name, season, round) |>
      dplyr::mutate(
        n_players = dplyr::n(),
        team_tog_sum = sum(pred_tog, na.rm = TRUE),
        tog_wt = dplyr::if_else(team_tog_sum > 0, pred_tog * 18 / team_tog_sum, 18 / n_players)
      ) |>
      dplyr::summarise(
        epr_week = sum(epr * tog_wt, na.rm = TRUE) * discount,
        epr_recv_week = sum(recv_epr * tog_wt, na.rm = TRUE) * discount,
        epr_disp_week = sum(disp_epr * tog_wt, na.rm = TRUE) * discount,
        epr_spoil_week = sum(spoil_epr * tog_wt, na.rm = TRUE) * discount,
        epr_hitout_week = sum(hitout_epr * tog_wt, na.rm = TRUE) * discount,
        psr_week = sum(psr * tog_wt, na.rm = TRUE) * discount,
        .groups = "drop"
      ) |>
      dplyr::mutate(round = w)
  }

  tr_week <- purrr::map_dfr(target_weeks, function(w) .build_week_ratings(tr, w)) |>
    dplyr::arrange(-epr_week)

  # Overlay injury-adjusted ratings
  # When real lineups exist (count > 0), keep lineup-based ratings.
  # Otherwise use roster + pred_tog + injury-adjusted ratings (epr_week).
  team_rt_fix_df <- team_rt_fix_df |>
    dplyr::left_join(tr_week, by = c("team_name" = "team_name", "season" = "season", "round_number" = "round")) |>
    dplyr::mutate(
      use_roster = !is.na(epr_week) & (is.na(count) | count == 0),
      epr = dplyr::if_else(use_roster, epr_week, epr),
      recv_epr = dplyr::if_else(use_roster, epr_recv_week, recv_epr),
      disp_epr = dplyr::if_else(use_roster, epr_disp_week, disp_epr),
      spoil_epr = dplyr::if_else(use_roster, epr_spoil_week, spoil_epr),
      hitout_epr = dplyr::if_else(use_roster, epr_hitout_week, hitout_epr),
      psr = dplyr::if_else(use_roster, psr_week, psr),
      use_roster = NULL
    )

  # Warn if any prediction-week teams still have NA ratings
  pred_rows <- team_rt_fix_df |>
    dplyr::filter(season == .env$season, round_number %in% target_weeks)
  na_epr <- pred_rows |> dplyr::filter(is.na(epr))
  if (nrow(na_epr) > 0) {
    na_teams <- unique(na_epr$team_name)
    cli::cli_warn("Missing EPR for {length(na_teams)} team{?s} in prediction weeks: {paste(na_teams, collapse = ', ')}")
  }

  # Weather ----
  cli::cli_h2("Loading weather")
  weather_df <- .load_match_weather(fixtures, all_grounds, target_weeks, season)

  # Build Model Dataset ----
  cli::cli_h2("Building model dataset")
  team_mdl_df <- .build_team_mdl_df(team_rt_fix_df, results, xg_df, weather_df, weight_anchor_date)

  # Train GAMs & Predict ----
  cli::cli_h2("Training GAM models on completed matches")
  gam_result <- .train_match_gams(team_mdl_df)
  team_mdl_df <- gam_result$data

  # Train XGBoost & Blend ----
  xgb_result <- tryCatch({
    cli::cli_h2("Training XGBoost models")
    res <- .train_match_xgb(team_mdl_df)
    team_mdl_df <- res$data
    res
  }, error = function(e) {
    cli::cli_warn("XGBoost training failed ({conditionMessage(e)}), using GAM-only predictions")
    NULL
  })

  # Blend GAM + XGBoost if XGBoost succeeded (kept outside tryCatch so
  # GAM predict failures are not misattributed to XGBoost and data is
  # never left in a half-blended state)
  if (!is.null(xgb_result)) {
    team_mdl_df$pred_tot_xscore <- 0.5 * team_mdl_df$pred_tot_xscore +
      0.5 * team_mdl_df$xgb_pred_tot_xscore
    team_mdl_df$pred_xscore_diff <- 0.5 * team_mdl_df$pred_xscore_diff +
      0.5 * team_mdl_df$xgb_pred_xscore_diff
    team_mdl_df$pred_score_diff <- 0.5 * team_mdl_df$pred_score_diff +
      0.5 * team_mdl_df$xgb_pred_score_diff
    team_mdl_df$pred_win <- predict(
      gam_result$models$win, newdata = team_mdl_df, type = "response"
    )
    cli::cli_alert_success("Blended GAM + XGBoost inputs, derived WP from GAM model")
  }

  # Format Predictions ----
  cli::cli_h2("Generating predictions for {length(target_weeks)} week{?s}")

  # All matches (for analysis)
  all_preds <- .format_match_preds(team_mdl_df)

  # Target week predictions (for upload)
  week_gms <- all_preds |>
    dplyr::filter(season == .env$season, round %in% target_weeks) |>
    dplyr::select(-season)

  # Validate ----
  cli::cli_h2("Validating predictions")
  validation_errors <- character(0)

  if (nrow(week_gms) == 0) cli::cli_abort("No predictions generated for week{?s} {paste(target_weeks, collapse = ', ')}")
  if (any(is.na(week_gms$pred_win))) validation_errors <- c(validation_errors, "NA values in pred_win")
  if (any(week_gms$pred_win < 0 | week_gms$pred_win > 1, na.rm = TRUE)) validation_errors <- c(validation_errors, "pred_win values out of [0,1] range")
  if (any(is.na(week_gms$pred_margin))) validation_errors <- c(validation_errors, "NA values in pred_margin")

  # Margin and win probability must agree in direction
  margin_sign <- sign(week_gms$pred_margin)
  win_sign <- sign(week_gms$pred_win - 0.5)
  # Exclude near-zero margins (< 1 point) and near-50/50 win probs where sign can legitimately differ
  meaningful <- abs(week_gms$pred_margin) > 1 & abs(week_gms$pred_win - 0.5) > 0.02
  disagreements <- meaningful & margin_sign != win_sign
  if (any(disagreements, na.rm = TRUE)) {
    bad <- week_gms[disagreements, ]
    validation_errors <- c(validation_errors, paste0(
      "Margin/win probability direction disagreement for ", sum(disagreements), " match(es). ",
      "e.g. ", bad$home_team[1], " vs ", bad$away_team[1],
      " (", season, " R", bad$round[1], ", ", bad$match_id[1], ")",
      ": margin=", round(bad$pred_margin[1], 1), ", win=", round(bad$pred_win[1], 3)
    ))
  }

  # Total expected score should be in a plausible range (100-250 points)
  if (any(week_gms$pred_xtotal < 100 | week_gms$pred_xtotal > 250, na.rm = TRUE)) {
    bad_xt <- week_gms[week_gms$pred_xtotal < 100 | week_gms$pred_xtotal > 250, ]
    validation_errors <- c(validation_errors, paste0(
      "Implausible pred_xtotal for ", nrow(bad_xt), " match(es) outside 100-250 range. ",
      "e.g. ", bad_xt$home_team[1], " vs ", bad_xt$away_team[1],
      " (", season, " R", bad_xt$round[1], ", ", bad_xt$match_id[1], ")",
      ": pred_xtotal=", round(bad_xt$pred_xtotal[1], 1)
    ))
  }

  if (length(validation_errors) > 0) {
    if (interactive()) {
      cli::cli_warn(c("Prediction validation failed ({length(validation_errors)} issue{?s}):", validation_errors))
      cli::cli_alert_info("Returning models and data for debugging (predictions NOT uploaded)")
      return(invisible(list(
        predictions = all_preds,
        gam_models = gam_result$models,
        xgb_models = if (!is.null(xgb_result)) xgb_result$models else NULL,
        model_data = team_mdl_df,
        validation_errors = validation_errors
      )))
    } else {
      cli::cli_abort(c("Prediction validation failed ({length(validation_errors)} issue{?s}):", validation_errors))
    }
  }

  cli::cli_alert_success("Validation passed: {nrow(week_gms)} matches")

  # Upload ----
  cli::cli_h2("Uploading predictions")
  week_gms <- week_gms |> dplyr::ungroup() |> dplyr::rename(week = round) |> dplyr::relocate(week)

  # --- Locked predictions: frozen at game start, never overwritten ---
  pred_file_name <- paste0("predictions_", season)
  existing <- tryCatch(
    file_reader(pred_file_name, "predictions"),
    error = function(e) {
      cli::cli_warn("Could not load existing predictions ({conditionMessage(e)}). Uploading current weeks only.")
      NULL
    }
  )

  # Actual margins from completed matches (shared by locked preds + retrodictions)
  completed_margins <- team_mdl_df |>
    dplyr::filter(!is.na(score_diff), team_type_fac.x == "home") |>
    dplyr::distinct(match_id, .keep_all = TRUE) |>
    dplyr::transmute(match_id, .actual_margin = score_diff)

  if (!is.null(existing) && nrow(existing) > 0) {
    # Backward compat: existing predictions may use old providerId column
    if (!"match_id" %in% names(existing) && "providerId" %in% names(existing)) {
      data.table::setnames(existing, "providerId", "match_id")
    }

    # Backward compat: rename home_rating/away_rating/rating_diff -> home_epr/away_epr/epr_diff
    old_to_new <- c(home_rating = "home_epr", away_rating = "away_epr", rating_diff = "epr_diff")
    for (old_nm in names(old_to_new)) {
      if (old_nm %in% names(existing) && !old_to_new[old_nm] %in% names(existing)) {
        names(existing)[names(existing) == old_nm] <- old_to_new[old_nm]
      }
    }

    n_backfilled <- sum(is.na(existing$margin) & existing$match_id %in% completed_margins$match_id)
    if (n_backfilled > 0) {
      existing <- existing |>
        dplyr::left_join(completed_margins, by = "match_id") |>
        dplyr::mutate(margin = dplyr::coalesce(margin, .actual_margin)) |>
        dplyr::select(-.actual_margin)
      cli::cli_alert_success("Backfilled {n_backfilled} match margin{?s} from results")
    }

    # Only replace predictions for games that haven't started yet
    started_ids <- team_mdl_df |>
      dplyr::filter(
        season.x == season,
        round_number.x %in% target_weeks,
        team_type_fac.x == "home",
        utc_dt <= Sys.time()
      ) |>
      dplyr::pull(match_id)

    week_gms <- week_gms |>
      dplyr::filter(!match_id %in% started_ids)

    if (length(started_ids) > 0) {
      cli::cli_alert_info("Keeping locked predictions for {length(started_ids)} already-started match{?es}")
    }

    combined <- existing |>
      dplyr::ungroup() |>
      dplyr::filter(!match_id %in% week_gms$match_id) |>
      dplyr::bind_rows(dplyr::ungroup(week_gms)) |>
      dplyr::arrange(week)
  } else {
    combined <- week_gms
  }

  tryCatch(
    {
      save_to_release(combined, pred_file_name, "predictions", also_csv = TRUE)
      cli::cli_alert_success("Uploaded locked predictions ({nrow(combined)} rows, week{?s} {paste(target_weeks, collapse = ', ')} updated)")
    },
    error = function(e) {
      local_path <- file.path("data-raw", paste0(pred_file_name, ".parquet"))
      arrow::write_parquet(combined, local_path)
      cli::cli_warn(c(
        "Failed to upload locked predictions: {conditionMessage(e)}",
        "i" = "Saved locally to {local_path}",
        "i" = "Check WORKFLOW_PAT if this is a permissions issue"
      ))
    }
  )

  # --- Retrodictions: current model on all matches, fully overwritten each run ---
  retro_all <- all_preds |>
    dplyr::rename(week = round) |>
    dplyr::relocate(week)

  if (nrow(completed_margins) > 0) {
    retro_all <- retro_all |>
      dplyr::left_join(completed_margins, by = "match_id") |>
      dplyr::mutate(margin = dplyr::coalesce(.actual_margin, margin)) |>
      dplyr::select(-.actual_margin)
  }

  # Daily runs: current season only. Full backfill when weeks = "all"
  retro_seasons <- if (is_backfill) sort(unique(retro_all$season)) else season
  retro_failures <- 0L
  for (retro_s in retro_seasons) {
    retro_preds <- retro_all |> dplyr::filter(season == retro_s)
    if (nrow(retro_preds) == 0) {
      cli::cli_warn("Skipping retrodictions_{retro_s}: 0 rows")
      next
    }
    retro_file_name <- paste0("retrodictions_", retro_s)
    tryCatch(
      {
        save_to_release(retro_preds, retro_file_name, "retrodictions", also_csv = TRUE)
        cli::cli_alert_success("Uploaded retrodictions_{retro_s} ({nrow(retro_preds)} rows)")
      },
      error = function(e) {
        retro_failures <<- retro_failures + 1L
        local_path <- file.path("data-raw", paste0(retro_file_name, ".parquet"))
        arrow::write_parquet(retro_preds, local_path)
        cli::cli_warn(c(
          "Failed to upload retrodictions_{retro_s}: {conditionMessage(e)}",
          "i" = "Saved locally to {local_path}"
        ))
      }
    )
  }
  if (retro_failures > 0) {
    cli::cli_warn("Retrodictions: {retro_failures}/{length(retro_seasons)} season(s) failed to upload")
  } else {
    cli::cli_alert_success("Retrodictions uploaded for {length(retro_seasons)} season{?s}")
  }

  # Upload models to torpmodels ----
  tryCatch(
    {
      cache_dir <- getOption("torpmodels.cache_dir",
                             file.path(tools::R_user_dir("torpmodels", "cache"), "models"))

      gam_path <- file.path(tempdir(), "match_gams.rds")
      t0 <- proc.time()[["elapsed"]]
      saveRDS(.strip_gam_models(gam_result$models), gam_path)
      t1 <- proc.time()[["elapsed"]]
      gam_size <- file.size(gam_path) / 1e6
      cli::cli_alert_info("match_gams saveRDS: {round(t1 - t0, 1)}s ({round(gam_size, 1)} MB)")
      piggyback::pb_upload(gam_path, repo = "peteowen1/torpmodels", tag = "core-models")
      t2 <- proc.time()[["elapsed"]]
      cli::cli_alert_info("match_gams pb_upload: {round(t2 - t1, 1)}s")
      local_cache <- file.path(cache_dir, "core", "match_gams.rds")
      if (dir.exists(dirname(local_cache))) {
        file.copy(gam_path, local_cache, overwrite = TRUE)
      }
      cli::cli_alert_success("Uploaded match_gams to torpmodels")

      if (!is.null(xgb_result)) {
        xgb_path <- file.path(tempdir(), "match_xgb_pipeline.rds")
        t3 <- proc.time()[["elapsed"]]
        saveRDS(xgb_result$models, xgb_path)
        t4 <- proc.time()[["elapsed"]]
        xgb_size <- file.size(xgb_path) / 1e6
        cli::cli_alert_info("match_xgb saveRDS: {round(t4 - t3, 1)}s ({round(xgb_size, 1)} MB)")
        piggyback::pb_upload(xgb_path, repo = "peteowen1/torpmodels", tag = "core-models")
        t5 <- proc.time()[["elapsed"]]
        cli::cli_alert_info("match_xgb pb_upload: {round(t5 - t4, 1)}s")
        local_cache_xgb <- file.path(cache_dir, "core", "match_xgb_pipeline.rds")
        if (dir.exists(dirname(local_cache_xgb))) {
          file.copy(xgb_path, local_cache_xgb, overwrite = TRUE)
        }
        cli::cli_alert_success("Uploaded match_xgb_pipeline to torpmodels")
      }
    },
    error = function(e) {
      cli::cli_warn("Could not upload models to torpmodels: {conditionMessage(e)}")
    }
  )

  elapsed <- (proc.time() - .pipeline_start)[["elapsed"]]
  cli::cli_h2("Pipeline Complete")
  cli::cli_inform("Total elapsed: {round(elapsed, 1)}s")

  invisible(list(
    predictions = all_preds,
    gam_models = gam_result$models,
    xgb_models = if (!is.null(xgb_result)) xgb_result$models else NULL,
    model_data = team_mdl_df
  ))
}


#' Show TORP Match Predictions
#'
#' Display a formatted summary of TORP match predictions for a given round,
#' including predicted margins, win probabilities, and actual results where
#' available.
#'
#' @param season Season year (default: current via `get_afl_season()`)
#' @param week Round number (default: current via `get_afl_week()`)
#' @param refresh If `TRUE`, run `run_predictions_pipeline()` first
#' @return Predictions tibble with results joined (invisibly)
#' @export
#' @examples
#' \dontrun{
#' show_predictions()
#' show_predictions(2025, 10)
#' show_predictions(refresh = TRUE)
#' }
show_predictions <- function(season = get_afl_season(),
                             week = get_afl_week(),
                             refresh = FALSE) {
  if (isTRUE(refresh)) {
    run_predictions_pipeline(week = week, season = season)
  }

  preds <- tryCatch(
    load_predictions(season, week),
    error = function(e) {
      cli::cli_abort(c(
        "Could not load predictions for {season} Round {week}.",
        "i" = "Run {.code show_predictions(refresh = TRUE)} to generate them.",
        "x" = e$message
      ))
    }
  )

  if (is.null(preds) || nrow(preds) == 0) {
    cli::cli_alert_warning("No predictions found for {season} Round {week}")
    return(invisible(preds))
  }

  # Backfill actual margins for completed games missing results
  now <- Sys.time()
  game_duration_hrs <- 3.5
  preds$start_time_utc <- .parse_start_time(preds$start_time)
  preds$is_complete <- !is.na(preds$start_time_utc) &
    (now > preds$start_time_utc + game_duration_hrs * 3600)

  needs_results <- any(preds$is_complete & is.na(preds$margin))
  if (needs_results) {
    tryCatch({
      fresh <- get_afl_results(season)
      if (!is.null(fresh) && nrow(fresh) > 0) {
        result_margins <- fresh |>
          dplyr::transmute(
            match_id = match_id,
            .actual_margin = home_score - away_score
          )
        # Backward compat: preds may use old providerId column
        if (!"match_id" %in% names(preds) && "providerId" %in% names(preds)) {
          data.table::setnames(preds, "providerId", "match_id")
        }
        preds <- preds |>
          dplyr::left_join(result_margins, by = "match_id") |>
          dplyr::mutate(margin = dplyr::coalesce(margin, .actual_margin)) |>
          dplyr::select(-.actual_margin)
      }
    }, error = function(e) {
      cli::cli_warn("Could not fetch results to backfill: {e$message}")
    })
  }

  # Format and print
  cli::cli_h1("TORP Predictions: {season} Round {week}")

  # Column widths
  w_team <- 20
  header <- paste0(
    format("Home", width = w_team),
    format("Away", width = w_team),
    format("Pred", width = 8, justify = "right"),
    format("Win%", width = 8, justify = "right"),
    format("Result", width = 10, justify = "right")
  )
  cli::cli_text("{.strong {header}}")

  tips_correct <- 0
  tips_total <- 0
  abs_errors <- c()

  for (i in seq_len(nrow(preds))) {
    row <- preds[i, ]
    pred_str <- sprintf("%+.1f", row$pred_margin)
    win_pct <- sprintf("%.1f%%", row$pred_win * 100)

    if (!is.na(row$margin)) {
      result_str <- sprintf("%+.0f", row$margin)
      # Tip correct: predicted winner matches actual winner (same sign)
      tip_ok <- (row$pred_margin > 0 & row$margin > 0) |
        (row$pred_margin < 0 & row$margin < 0) |
        (row$margin == 0)  # draw counts as correct
      icon <- if (tip_ok) cli::col_green("\u2713") else cli::col_red("\u2717")
      result_display <- paste(result_str, icon)
      tips_total <- tips_total + 1
      tips_correct <- tips_correct + as.integer(tip_ok)
      abs_errors <- c(abs_errors, abs(row$pred_margin - row$margin))
    } else if (isTRUE(row$is_complete)) {
      result_display <- "?"
    } else {
      result_display <- "-"
    }

    line <- paste0(
      format(row$home_team, width = w_team),
      format(row$away_team, width = w_team),
      format(pred_str, width = 8, justify = "right"),
      format(win_pct, width = 8, justify = "right"),
      format(result_display, width = 10, justify = "right")
    )
    cli::cli_text(line)
  }

  # Summary
  cli::cli_text("")
  parts <- c()
  if (tips_total > 0) {
    parts <- c(parts, paste0("Tips: ", tips_correct, "/", tips_total, " correct"))
    parts <- c(parts, paste0("MAE: ", sprintf("%.1f", mean(abs_errors))))
  }
  n_complete <- sum(preds$is_complete | !is.na(preds$margin))
  parts <- c(parts, paste0("Completed: ", n_complete, "/", nrow(preds)))
  cli::cli_text(paste(parts, collapse = " | "))

  preds$start_time_utc <- NULL
  preds$is_complete <- NULL
  invisible(preds)
}


#' Parse start_time strings to POSIXct
#'
#' Handles the local-time formatted strings stored in predictions data.
#'
#' @param x Character vector of start time strings
#' @return POSIXct vector
#' @keywords internal
.parse_start_time <- function(x) {
  # start_time is formatted as "YYYY-MM-DD HH:MM:SS TZ"
  # Try parsing with timezone, fall back to UTC
  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = "Australia/Melbourne"))
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Melbourne"))
  }
  if (all(is.na(parsed)) && length(x) > 0 && !all(is.na(x))) {
    cli::cli_warn("Could not parse any start_time values. Example: {x[!is.na(x)][1]}")
  }
  parsed
}
