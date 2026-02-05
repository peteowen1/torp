# Model cache environment - stores loaded models to avoid repeated loading
.torp_model_cache <- new.env(parent = emptyenv())

#' Clear Model Cache
#'
#' Clears all cached models from memory. Useful when you want to force
#' reloading of models, for example after updating torpmodels.
#'
#' @return Invisible NULL
#' @export
#' @examples
#' clear_model_cache()
clear_model_cache <- function() {
 rm(list = ls(envir = .torp_model_cache), envir = .torp_model_cache)
 invisible(NULL)
}

#' Add Expected Points Value (EPV) Variables
#'
#' This function adds EPV-related variables to the input dataframe.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with additional EPV-related variables.
#' @export
#' @importFrom dplyr mutate case_when if_else lead lag group_by ungroup bind_cols
#' @importFrom lubridate as_date
#' @importFrom cli cli_abort
add_epv_vars <- function(df) {
  # Input validation
  if (!is.data.frame(df)) {
    cli::cli_abort("Input 'df' must be a data frame.")
  }
  
  if (nrow(df) == 0) {
    cli::cli_abort("Input data frame cannot be empty.")
  }
  
  base_ep_preds <- get_epv_preds(df)
  pbp_final <- dplyr::bind_cols(df, base_ep_preds)

  pbp_final <- pbp_final %>%
    dplyr::group_by(.data$match_id, .data$period) %>%
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
      weight_gm = exp(as.numeric(-(lubridate::as_date(Sys.Date()) - lubridate::as_date(.data$utc_start_time))) / 365),
      round_week = sprintf("%02d", .data$round_number)
    ) %>%
    dplyr::ungroup()

  return(pbp_final)
}

#' Add Win Probability Variables (Enhanced)
#'
#' This function adds enhanced win probability (WP) and win probability added (WPA) variables 
#' using an ensemble model approach with sophisticated feature engineering.
#'
#' @param df A dataframe containing play-by-play data.
#' @param use_enhanced Logical, whether to use the enhanced ensemble model (default TRUE).
#'
#' @return A dataframe with additional win probability-related variables.
#' @export
#' @importFrom dplyr mutate case_when lead group_by ungroup bind_cols arrange
#' @importFrom cli cli_abort cli_warn
#' @importFrom zoo rollmean rollapply
add_wp_vars <- function(df, use_enhanced = TRUE) {
  # Input validation
  if (!is.data.frame(df)) {
    cli::cli_abort("Input 'df' must be a data frame.")
  }
  
  if (nrow(df) == 0) {
    cli::cli_abort("Input data frame cannot be empty.")
  }
  
  # Check for required columns for enhanced model
  required_cols <- c("match_id", "period", "period_seconds", "points_diff", 
                    "exp_pts", "goal_x", "y", "home", "team_id_mdl")
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0 && use_enhanced) {
    cli::cli_warn("Missing columns for enhanced model: {paste(missing_cols, collapse = ', ')}. Using basic model.")
    use_enhanced <- FALSE
  }
  
  # Choose prediction method with comprehensive error handling
  if (use_enhanced) {
    wp_preds <- tryCatch({
      get_wp_preds_enhanced(df)
    }, error = function(e) {
      cli::cli_warn("Enhanced model failed: {e$message}. Falling back to basic model.")
      basic_preds <- get_wp_preds(df)
      colnames(basic_preds) <- "wp"
      return(basic_preds)
    })
    
    # Validate predictions
    if (is.null(wp_preds) || !is.data.frame(wp_preds) || !"wp" %in% names(wp_preds)) {
      cli::cli_warn("Invalid enhanced predictions. Using basic model.")
      wp_preds <- get_wp_preds(df)
      colnames(wp_preds) <- "wp"
    }
  } else {
    wp_preds <- get_wp_preds(df)
    colnames(wp_preds) <- "wp"
  }
  
  # Final validation of predictions
  if (nrow(wp_preds) != nrow(df)) {
    cli::cli_abort("Prediction count mismatch: expected {nrow(df)}, got {nrow(wp_preds)}")
  }
  
  # Bind predictions to original data
  pbp_final <- dplyr::bind_cols(df, wp_preds)

  # Calculate Win Probability Added (WPA) with improved logic
  pbp_final <- pbp_final %>%
    dplyr::group_by(.data$match_id) %>%
    dplyr::arrange(.data$period, .data$period_seconds) %>%
    dplyr::mutate(
      # Round win probability to reasonable precision
      wp = round(pmax(0.001, pmin(0.999, .data$wp)), 5),  # Bound between 0.001 and 0.999
      
      # Enhanced WPA calculation
      wp_next = dplyr::lead(.data$wp, default = dplyr::last(.data$wp)),
      team_id_next = dplyr::lead(.data$team_id_mdl, default = dplyr::last(.data$team_id_mdl)),
      
      # WPA calculation accounting for team changes
      wpa = round(dplyr::case_when(
        # Same team continues possession
        .data$team_id_next == .data$team_id_mdl ~ .data$wp_next - .data$wp,
        # Possession changes to other team
        .data$team_id_next != .data$team_id_mdl ~ (1 - .data$wp_next) - .data$wp,
        # Default case (shouldn't happen)
        TRUE ~ 0
      ), 5),
      
      # Add additional context variables if using enhanced model
      wp_category = dplyr::case_when(
        .data$wp >= 0.8 ~ "very_likely",
        .data$wp >= 0.6 ~ "likely", 
        .data$wp >= 0.4 ~ "toss_up",
        .data$wp >= 0.2 ~ "unlikely",
        TRUE ~ "very_unlikely"
      ),
      
      # High leverage situations (where WPA swings are most impactful)
      high_leverage = abs(.data$wpa) >= 0.05 | 
                     (.data$wp >= 0.2 & .data$wp <= 0.8),
      
      # Remove helper columns
      wp_next = NULL,
      team_id_next = NULL
    ) %>%
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

  pbp_final <- pbp_final %>%
    dplyr::mutate(
      on_target_prob = .data$goal_prob + .data$behind_prob,
      xscore = .data$goal_prob * 6 + .data$behind_prob
    ) %>%
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
  # Try to load model from torpmodels first, then fall back to package data
  ep_model <- load_model_with_fallback("ep")

  if (is.null(ep_model)) {
    cli::cli_abort("EP model not available. Install torpmodels or ensure package data is available.")
  }

  # Log prediction event
  input_hash <- paste0("ep_", nrow(df), "_", ncol(df), "_", as.integer(Sys.time()))
  log_prediction_event("ep_model", input_hash, nrow(df))

  tryCatch({
    # Detect model type and use appropriate prediction method
    model_data <- df %>% select_epv_model_vars()

    if (inherits(ep_model, "xgb.Booster")) {
      # XGBoost model - use xgboost::predict
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        cli::cli_abort("xgboost package required but not available")
      }
      model_matrix <- stats::model.matrix(~ . + 0, data = model_data)
      preds_raw <- predict(ep_model, model_matrix)
    } else {
      # GAM or other model types - use stats::predict
      model_matrix <- stats::model.matrix(~ . + 0, data = model_data)
      preds_raw <- stats::predict(ep_model, model_matrix)
    }

    # Convert to proper format
    preds <- as.data.frame(
      matrix(preds_raw, ncol = 5, byrow = TRUE)
    )
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
    warning(paste("EP model prediction failed:", e$message, "- Input hash:", input_hash))
    cli::cli_abort("EP model prediction failed: {e$message}")
  })
}

#' Get Win Probability Predictions
#'
#' This function generates win probability predictions for the input data.
#'
#' @param df A dataframe containing play-by-play data.
#'
#' @return A dataframe with win probability predictions.
#' @keywords internal
#' @importFrom stats predict model.matrix
#' @importFrom utils data
get_wp_preds <- function(df) {
  # Try to load model from torpmodels first, then fall back to package data
  wp_model <- load_model_with_fallback("wp")

  if (is.null(wp_model)) {
    cli::cli_abort("WP model not available. Install torpmodels or ensure package data is available.")
  }

  preds <- as.data.frame(
    matrix(stats::predict(wp_model, stats::model.matrix(~ . + 0, data = df %>% select_wp_model_vars())),
      ncol = 1, byrow = TRUE
    )
  )
  colnames(preds) <- "wp"

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
  # Try to load model from torpmodels first, then fall back to package data
  shot_ocat_mdl <- load_model_with_fallback("shot")

  if (is.null(shot_ocat_mdl)) {
    cli::cli_abort("Shot model not available. Install torpmodels or ensure package data is available.")
  }

  preds <- stats::predict(shot_ocat_mdl, df, type = "response")
  return(preds)
}

#' Load Model with Fallback
#'
#' Attempts to load a model from torpmodels package first, then falls back
#' to package data if torpmodels is not available. Models are cached in memory
#' to avoid repeated loading.
#'
#' @param model_name Short model name: "ep", "wp", "shot", or "xgb_win"
#' @return The loaded model object, or NULL if not available
#' @keywords internal
load_model_with_fallback <- function(model_name) {
  # Check cache first
  if (exists(model_name, envir = .torp_model_cache)) {
    return(get(model_name, envir = .torp_model_cache))
  }

  # Map short names to full names for package data
  model_map <- list(
    ep = "ep_model",
    wp = "wp_model",
    shot = "shot_ocat_mdl",
    xgb_win = "xgb_win_model"
  )

  full_name <- model_map[[model_name]]
  if (is.null(full_name)) {
    cli::cli_warn("Unknown model name: {model_name}")
    return(NULL)
  }

  model <- NULL

  # Try torpmodels first if available
  if (requireNamespace("torpmodels", quietly = TRUE)) {
    model <- tryCatch({
      torpmodels::load_torp_model(model_name, verbose = FALSE)
    }, error = function(e) {
      cli::cli_warn("torpmodels load failed for {model_name}: {e$message}")
      NULL
    })
  }

  # Fall back to package data if torpmodels didn't work
  if (is.null(model)) {
    tryCatch({
      utils::data(list = full_name, package = "torp", envir = environment())
      model <- get(full_name, envir = environment())
    }, error = function(e) {
      cli::cli_warn("Package data load failed for {full_name}: {e$message}")
    })
  }

  # Store in cache if successfully loaded
  if (!is.null(model)) {
    assign(model_name, model, envir = .torp_model_cache)
  }

  return(model)
}
