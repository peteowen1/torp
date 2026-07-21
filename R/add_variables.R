# Note: Model cache (.torp_model_cache) is defined in R/cache.R

#' Add Expected Points Value (EPV) Variables
#'
#' This function adds EPV-related variables to the input dataframe.
#'
#' @param df A dataframe containing play-by-play data.
#' @param reference_date Date used for computing game recency weights.
#'   Defaults to \code{Sys.Date()}. Set explicitly for reproducible historical analysis.
#'
#' @return A dataframe with additional EPV-related variables.
#' @export
#' @importFrom dplyr mutate case_when if_else lead lag group_by ungroup bind_cols
#' @importFrom lubridate as_date
#' @importFrom cli cli_abort
add_epv_vars <- function(df, reference_date = Sys.Date()) {
  # Input validation
  if (!is.data.frame(df)) {
    cli::cli_abort("Input 'df' must be a data frame.")
  }
  
  if (nrow(df) == 0) {
    cli::cli_abort("Input data frame cannot be empty.")
  }
  
  base_ep_preds <- get_epv_preds(df)

  if (nrow(base_ep_preds) != nrow(df)) {
    cli::cli_abort("EP prediction count mismatch: expected {nrow(df)}, got {nrow(base_ep_preds)}")
  }

  pbp_final <- dplyr::bind_cols(df, base_ep_preds)

  pbp_final <- pbp_final |>
    dplyr::group_by(.data$match_id, .data$period) |>
    dplyr::mutate(
      exp_pts = round(dplyr::case_when(
        .data$description == "Centre Bounce" ~ 0,
        TRUE ~ -6 * .data$opp_goal - .data$opp_behind + .data$behind + 6 * .data$goal
      ), 5),
      exp_pts = dplyr::if_else(.data$description == "Out On Full After Kick", -dplyr::lead(.data$exp_pts, default = 0), .data$exp_pts),
      kick_points = dplyr::case_when(
        (.data$shot_at_goal == TRUE & .data$disposal == "clanger") ~ 0,
        TRUE ~ .data$points_shot
      ),
      player_name = paste(.data$player_name_given_name, .data$player_name_surname),
      pos_team = dplyr::case_when(
        !is.na(.data$points_shot) ~ 1,
        dplyr::lead(.data$team_id_mdl) == .data$team_id_mdl ~ 1,
        is.na(dplyr::lead(.data$team_id_mdl)) ~ 1,
        TRUE ~ -1
      ),
      team_change = dplyr::case_when(
        dplyr::lead(.data$team_id_mdl) == .data$team_id_mdl ~ 1,
        is.na(dplyr::lead(.data$team_id_mdl)) ~ 1,
        TRUE ~ -1
      ),
      lead_points = dplyr::if_else(is.na(.data$points_shot), dplyr::lead(.data$exp_pts, default = 0), (.data$points_shot - dplyr::lead(.data$exp_pts, default = 0))),
      lead_player = dplyr::if_else(!is.na(.data$points_shot) | .data$lead_desc == "Out of Bounds" | .data$description == "Out On Full After Kick",
        .data$player_name, dplyr::lead(.data$player_name)
      ),
      lead_player_id = dplyr::if_else(!is.na(.data$points_shot) | .data$lead_desc == "Out of Bounds" | .data$description == "Out On Full After Kick",
        .data$player_id, dplyr::lead(.data$player_id)
      ),
      lead_team = dplyr::if_else(is.na(.data$points_shot), dplyr::lead(.data$team), .data$team),
      xpoints_diff = .data$points_diff + .data$exp_pts,
      delta_epv = dplyr::lead(.data$xpoints_diff, default = dplyr::last(.data$points_diff)) * .data$team_change - .data$xpoints_diff,
      weight_gm = exp(as.numeric(-(lubridate::as_date(reference_date) - lubridate::as_date(.data$utc_start_time))) / EPV_WEIGHT_DECAY_DAYS),
      round_week = sprintf("%02d", .data$round_number)
    ) |>
    dplyr::ungroup()

  # Sanity checks on derived columns
  if (any(!pbp_final$pos_team %in% c(-1L, 1L), na.rm = TRUE)) {
    cli::cli_warn("Unexpected pos_team values detected (expected -1 or 1)")
  }
  if (any(!pbp_final$team_change %in% c(-1L, 1L), na.rm = TRUE)) {
    cli::cli_warn("Unexpected team_change values detected (expected -1 or 1)")
  }

  return(pbp_final)
}

#' Add Win Probability Variables
#'
#' Adds win probability (WP) and win probability added (WPA) variables
#' using the XGBoost WP model.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with additional win probability-related variables.
#' @export
#' @importFrom dplyr mutate case_when lead group_by ungroup bind_cols arrange
#' @importFrom cli cli_abort cli_warn
add_wp_vars <- function(df) {
  if (!is.data.frame(df)) {
    cli::cli_abort("Input 'df' must be a data frame.")
  }

  if (nrow(df) == 0) {
    cli::cli_abort("Input data frame cannot be empty.")
  }

  wp_preds <- get_wp_preds(df)
  colnames(wp_preds) <- "wp"

  if (nrow(wp_preds) != nrow(df)) {
    cli::cli_abort("Prediction count mismatch: expected {nrow(df)}, got {nrow(wp_preds)}")
  }

  pbp_final <- dplyr::bind_cols(df, wp_preds)

  pbp_final <- pbp_final |>
    dplyr::group_by(.data$match_id) |>
    dplyr::arrange(.data$period, .data$period_seconds) |>
    dplyr::mutate(
      wp = round(pmax(0.001, pmin(0.999, .data$wp)), 5),

      wp_next = dplyr::lead(.data$wp, default = dplyr::last(.data$wp)),
      team_id_next = dplyr::lead(.data$team_id_mdl, default = dplyr::last(.data$team_id_mdl)),

      wpa = round(dplyr::case_when(
        .data$team_id_next == .data$team_id_mdl ~ .data$wp_next - .data$wp,
        .data$team_id_next != .data$team_id_mdl ~ (1 - .data$wp_next) - .data$wp,
        TRUE ~ 0
      ), 5),

      wp_category = dplyr::case_when(
        .data$wp >= 0.8 ~ "very_likely",
        .data$wp >= 0.6 ~ "likely",
        .data$wp >= 0.4 ~ "toss_up",
        .data$wp >= 0.2 ~ "unlikely",
        TRUE ~ "very_unlikely"
      ),

      high_leverage = abs(.data$wpa) >= 0.05 |
                     (.data$wp >= 0.2 & .data$wp <= 0.8),

      wp_next = NULL,
      team_id_next = NULL
    ) |>
    dplyr::ungroup()

  return(pbp_final)
}

#' Add Shot Variables
#'
#' This function adds shot-related variables to the input dataframe.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with additional shot-related variables.
#' @export
#' @importFrom dplyr mutate bind_cols
#' @importFrom cli cli_abort
add_shot_vars <- function(df) {
  # Input validation
  if (!is.data.frame(df)) {
    cli::cli_abort("Input 'df' must be a data frame.")
  }
  
  if (nrow(df) == 0) {
    cli::cli_abort("Input data frame cannot be empty.")
  }
  
  ocat_shot_result_preds <- as.data.frame(get_shot_result_preds(df))
  colnames(ocat_shot_result_preds) <- c("clanger_prob", "behind_prob", "goal_prob")

  pbp_final <- dplyr::bind_cols(df, ocat_shot_result_preds)

  pbp_final <- pbp_final |>
    dplyr::mutate(
      on_target_prob = .data$goal_prob + .data$behind_prob,
      xscore = .data$goal_prob * 6 + .data$behind_prob
    ) |>
    dplyr::ungroup()

  return(pbp_final)
}

#' Get Expected Points Value Predictions
#'
#' This function generates EPV predictions for the input data.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with EPV predictions.
#' @keywords internal
#' @importFrom stats predict model.matrix
#' @importFrom utils data
get_epv_preds <- function(df) {
  ep_model <- load_model_with_fallback("ep")

  # Log prediction event
  input_hash <- paste0("ep_", nrow(df), "_", ncol(df))
  log_prediction_event("ep_model", input_hash, nrow(df))

  tryCatch({
    # Detect model type and use appropriate prediction method
    model_data <- df |> select_epv_model_vars()

    if (inherits(ep_model, "xgb.Booster")) {
      # XGBoost model - use xgboost::predict
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        cli::cli_abort("xgboost package required but not available")
      }
      model_matrix <- stats::model.matrix(~ . + 0, data = model_data)
      preds_raw <- predict(ep_model, model_matrix)
    } else {
      # GAM or other model types - use stats::predict with original data frame
      preds_raw <- stats::predict(ep_model, newdata = model_data)
    }

    # Convert to proper format
    # xgboost 3.x returns a matrix directly; older versions return a flat vector
    if (is.matrix(preds_raw)) {
      preds <- as.data.frame(preds_raw)
    } else {
      preds <- as.data.frame(matrix(preds_raw, ncol = 5, byrow = TRUE))
    }
    colnames(preds) <- c("opp_goal", "opp_behind", "behind", "goal", "no_score")

    # Log successful prediction
    pred_summary <- list(
      min = min(rowSums(preds)),
      max = max(rowSums(preds)),
      mean = mean(rowSums(preds))
    )
    log_prediction_event("ep_model", input_hash, nrow(df), pred_summary)

    return(preds)

  }, error = function(e) {
    cli::cli_abort("EP model prediction failed: {e$message}")
  })
}

#' Get Win Probability Predictions
#'
#' This function generates win probability predictions for the input data.
#' Applies the WP recalibration sidecar (`wp_calibration`) immediately after
#' the raw model prediction, so every caller of this function -- not just
#' [add_wp_vars()] -- serves calibrated WP: `add_wp_vars()`'s downstream
#' `wp`/`wpa`, `create_wp_credit()`, `player_credit.R`,
#' `player_game_ratings.R`, released parquets. If the sidecar is unavailable
#' (old deployment, not yet published, network down),
#' [load_model_with_fallback()] returns `NULL` and this degrades to the
#' identity function -- never an error.
#'
#' Form dispatch (FABLE-RECAL-PLAN.md \enc{§}{Section}7): the sidecar's `$form` field
#' selects the calibration formula. Missing `$form` or `"global_v1"` --
#' the legacy 2-param Platt-on-logit, `plogis(a + b * qlogis(p))`.
#' `"leverage_interaction_v1"` (\enc{§}{Section}7 Path B, current as of 2026-07-21) --
#' `plogis(a + (b + c*w) * qlogis(p))`, where `w` is the continuous leverage
#' weight `pmax(0, 1 - minutes_remaining/ramp_mins) * pmax(0, 1 -
#' abs(points_diff)/margin_cap)`, `minutes_remaining = est_match_remaining /
#' 60` (`est_match_remaining` is a `WP_MODEL_FEATURES` column, the same
#' match-time-remaining variable the WP model itself trains on;
#' `ramp_mins`/`margin_cap` come from the sidecar, defaulting to 20/18 if
#' absent). Any OTHER `$form` value aborts loudly -- an applier that can't
#' interpret a sidecar's schema must fail the load, never silently serve
#' uncalibrated or misapplied WP. Within a recognized form, missing/
#' non-finite numeric fields (`a`/`b`/`c`/etc.) still degrade gracefully to
#' identity (transient corruption, not a schema mismatch).
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with win probability predictions.
#' @keywords internal
#' @importFrom stats predict model.matrix qlogis plogis
#' @importFrom utils data
get_wp_preds <- function(df) {
  wp_model <- load_model_with_fallback("wp")

  if (inherits(wp_model, "xgb.Booster") && !requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("xgboost package required but not available")
  }

  model_data <- df |> select_wp_model_vars()
  model_matrix <- stats::model.matrix(~ . + 0, data = model_data)
  preds_raw <- stats::predict(wp_model, model_matrix)

  # xgboost 3.x may return a matrix; flatten to vector for binary prediction
  if (is.matrix(preds_raw)) {
    preds_raw <- as.vector(preds_raw)
  }

  # Platt-on-logit recalibration, identity fallback when the sidecar is
  # absent (load_model_with_fallback() has already warn-once'd in that
  # case). Form dispatch per FABLE-RECAL-PLAN.md \enc{§}{Section}7 -- see roxygen above.
  calib <- load_model_with_fallback("wp_calibration")
  if (!is.null(calib)) {
    form <- calib$form
    calib_a <- calib$a
    calib_b <- calib$b
    calib_a_ok <- is.numeric(calib_a) && length(calib_a) == 1 && is.finite(calib_a)
    calib_b_ok <- is.numeric(calib_b) && length(calib_b) == 1 && is.finite(calib_b)

    if (is.null(form) || identical(form, "global_v1")) {
      if (calib_a_ok && calib_b_ok) {
        preds_raw <- stats::plogis(calib_a + calib_b * stats::qlogis(preds_raw))
      }
    } else if (identical(form, "leverage_interaction_v1")) {
      if (calib_a_ok && calib_b_ok) {
        calib_c <- calib$c
        if (!is.numeric(calib_c) || length(calib_c) != 1 || !is.finite(calib_c)) calib_c <- 0
        ramp_mins <- calib$ramp_mins
        if (is.null(ramp_mins) || length(ramp_mins) != 1 || !is.finite(ramp_mins)) ramp_mins <- 20
        margin_cap <- calib$margin_cap
        if (is.null(margin_cap) || length(margin_cap) != 1 || !is.finite(margin_cap)) margin_cap <- 18

        # w per row: needs est_match_remaining + points_diff on df (both are
        # WP_MODEL_FEATURES, so guaranteed present whenever model_matrix
        # above succeeded) -- missing columns fall back to w = 0 (global
        # arm), same defensive style as the retired q4close_interaction cell
        # flag.
        w <- rep(0, length(preds_raw))
        if (all(c("est_match_remaining", "points_diff") %in% names(df))) {
          minutes_remaining <- df$est_match_remaining / 60
          w <- pmax(0, 1 - minutes_remaining / ramp_mins) * pmax(0, 1 - abs(df$points_diff) / margin_cap)
          w[is.na(w)] <- 0
        }

        preds_raw <- stats::plogis(calib_a + (calib_b + calib_c * w) * stats::qlogis(preds_raw))
      }
    } else {
      cli::cli_abort(c(
        "wp_calibration sidecar has an unrecognized form: {.val {form}}",
        "i" = "Known forms: missing/\"global_v1\" (legacy a + b*qlogis(p)) and \"leverage_interaction_v1\" (FABLE-RECAL-PLAN.md §7 Path B). Refusing to silently serve uncalibrated or misapplied WP -- update torp's applier (get_wp_preds()) or check the published sidecar."
      ))
    }
  }

  preds <- data.frame(wp = preds_raw)

  return(preds)
}

#' Get Shot Result Predictions
#'
#' This function generates shot result predictions for the input data.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with shot result predictions.
#' @keywords internal
#' @importFrom stats predict
#' @importFrom utils data
get_shot_result_preds <- function(df) {
  shot_ocat_mdl <- load_model_with_fallback("shot")

  # mgcv must be attached (not just loaded) for GAM/BAM predict() — its
  # internal Xbd C function is only available on the search path when attached.
  # Attachment is done once in .onLoad(); this guard handles edge cases where
  # the package is loaded outside the normal path.
  if (inherits(shot_ocat_mdl, c("gam", "bam")) && !"mgcv" %in% .packages()) {
    if (!requireNamespace("mgcv", quietly = TRUE)) {
      cli::cli_abort("mgcv package required for shot model predictions but not available")
    }
    attachNamespace("mgcv")
  }

  preds <- stats::predict(shot_ocat_mdl, df, type = "response")
  return(preds)
}

#' Load Model from torpmodels
#'
#' Loads a model from the torpmodels package with in-memory caching.
#' Install torpmodels via `devtools::install_github("peteowen1/torpmodels")`.
#'
#' `"wp_calibration"` is the one exception to the usual abort-on-failure
#' contract: it's an optional sidecar (torpverse/docs/plans/FABLE-RECAL-PLAN.md D3/D4), so a
#' 404/absence/network failure degrades to a `cli_warn()` and `NULL` --
#' never an error. Unlike a successful load, a failed load is deliberately
#' NOT written to the model cache slot, so the next call retries the real
#' load instead of a one-off transient failure permanently downgrading the
#' rest of the session to uncalibrated WP; the warning itself is still
#' deduped to once per session via a separate cache key so retries don't
#' spam the log. Every other model name keeps the hard-abort contract.
#'
#' @param model_name Short model name: "ep", "wp", "wp_calibration", "shot",
#'   "match_gams", or "xgb_win"
#' @return The loaded model object, or `NULL` for `"wp_calibration"` when
#'   unavailable.
#' @keywords internal
load_model_with_fallback <- function(model_name) {
  # Check cache first
  if (exists(model_name, envir = .torp_model_cache)) {
    return(get(model_name, envir = .torp_model_cache))
  }

  valid_models <- c("ep", "wp", "wp_calibration", "shot", "xgb_win", "match_gams", "shot_player_df")
  if (!model_name %in% valid_models) {
    cli::cli_abort("Unknown model name: {model_name}. Must be one of: {paste(valid_models, collapse = ', ')}")
  }

  is_optional_calibration <- identical(model_name, "wp_calibration")

  # Warn-once dedup lives on its own key, separate from the model cache slot
  # itself -- a failed load must NEVER populate `model_name`'s cache slot
  # (see below), so gating the warning on that slot would silently
  # reintroduce the "one transient failure permanently serves uncalibrated
  # WP" bug it's meant to avoid. Deliberately NOT dot-prefixed: ls()
  # (used by clear_model_cache()/get_model_cache_info()) defaults to
  # all.names = FALSE and would silently skip a dot-prefixed key, leaking
  # warn-once state across a clear_model_cache() call.
  warned_key <- paste0(model_name, "__warned")
  warn_once <- function(msg) {
    if (!exists(warned_key, envir = .torp_model_cache)) {
      cli::cli_warn(msg)
      assign(warned_key, TRUE, envir = .torp_model_cache)
    }
  }

  if (!requireNamespace("torpmodels", quietly = TRUE)) {
    if (is_optional_calibration) {
      warn_once("torpmodels package not available -- serving uncalibrated WP (wp_calibration unavailable)")
      return(NULL)
    }
    cli::cli_abort(c(
      "torpmodels package is required but not installed.",
      "i" = 'Install with: devtools::install_github("peteowen1/torpmodels")'
    ))
  }

  model <- tryCatch(
    torpmodels::load_torp_model(model_name, verbose = FALSE),
    error = function(e) {
      if (is_optional_calibration) {
        warn_once(c(
          "wp_calibration unavailable -- serving uncalibrated WP",
          "x" = e$message
        ))
        return(NULL)
      }
      cli::cli_abort(c(
        "Failed to load {model_name} model from torpmodels.",
        "x" = e$message,
        "i" = "Try: torpmodels::clear_model_cache(); then retry."
      ))
    }
  )

  # Only cache a successful load. A failed optional-calibration load returns
  # NULL here without ever being written to `model_name`'s cache slot, so
  # the NEXT get_wp_preds() call retries the real load instead of a single
  # transient network blip permanently downgrading the whole session to
  # uncalibrated WP.
  if (!is.null(model)) {
    assign(model_name, model, envir = .torp_model_cache)
  }
  model
}
