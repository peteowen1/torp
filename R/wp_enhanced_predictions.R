# Enhanced Win Probability Prediction Functions
# ============================================
# Production-ready functions for enhanced win probability predictions

#' Create Enhanced Features for Win Probability
#'
#' Centralized feature engineering for win probability models
#'
#' @param df A dataframe containing play-by-play data
#' @param for_training Logical, whether features are for training (default FALSE)
#' @return A dataframe with enhanced features
#' @keywords internal
#' @importFrom dplyr group_by arrange mutate ungroup case_when
#' @importFrom zoo rollmean rollapply
create_wp_features_enhanced <- function(df, for_training = FALSE) {
  # Input validation
  required_cols <- c("match_id", "period", "period_seconds", "points_diff",
                    "exp_pts", "goal_x", "y", "home", "team_id_mdl")

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {paste(missing_cols, collapse = ', ')}")
  }

  # Use constants from R/constants.R (AFL_QUARTER_DURATION, AFL_TOTAL_GAME_SECONDS)

  df_enhanced <- df %>%
    dplyr::group_by(.data$match_id) %>%
    dplyr::arrange(.data$period, .data$period_seconds) %>%
    dplyr::mutate(
      # Time-based features (using constants from R/constants.R)
      time_remaining = (AFL_MAX_PERIODS - .data$period) * AFL_QUARTER_DURATION + (AFL_QUARTER_DURATION - .data$period_seconds),
      time_remaining_pct = .data$time_remaining / AFL_TOTAL_GAME_SECONDS,
      quarter_progress = .data$period_seconds / AFL_QUARTER_DURATION,
      is_final_quarter = as.numeric(.data$period == 4),
      is_final_5_mins = as.numeric(.data$time_remaining <= 300),
      is_final_2_mins = as.numeric(.data$time_remaining <= 120),
      
      # Enhanced scoring features
      points_diff_abs = abs(.data$points_diff),
      points_diff_squared = .data$points_diff^2,
      points_diff_per_time = .data$points_diff / pmax(.data$time_remaining, 1),
      
      # Expected points features
      exp_pts_momentum = .data$exp_pts - dplyr::lag(.data$exp_pts, default = 0),
      exp_pts_ma_3 = zoo::rollmean(.data$exp_pts, k = 3, fill = .data$exp_pts, align = "right"),
      exp_pts_ma_5 = zoo::rollmean(.data$exp_pts, k = 5, fill = .data$exp_pts, align = "right"),
      
      # Positional features
      field_pos_score = dplyr::case_when(
        .data$goal_x <= 30 ~ 3,
        .data$goal_x <= 50 ~ 2,
        .data$goal_x <= 80 ~ 1,
        TRUE ~ 0
      ),
      goal_distance = sqrt(.data$goal_x^2 + .data$y^2),
      goal_angle = atan2(abs(.data$y), .data$goal_x) * 180 / pi,
      
      # Momentum features
      epv_swing = abs(.data$exp_pts - dplyr::lag(.data$exp_pts, default = .data$exp_pts[1])),
      epv_volatility = zoo::rollapply(.data$exp_pts, width = 5, FUN = sd, fill = 0, align = "right"),
      
      # Game state features
      lead_size_category = dplyr::case_when(
        abs(.data$points_diff) <= 6 ~ "tight",
        abs(.data$points_diff) <= 18 ~ "moderate",
        abs(.data$points_diff) <= 30 ~ "comfortable",
        TRUE ~ "blowout"
      ),
      
      is_pressure_moment = as.numeric(
        (.data$is_final_5_mins == 1 & abs(.data$points_diff) <= 12) |
        (.data$is_final_2_mins == 1 & abs(.data$points_diff) <= 18)
      ),
      
      # Interaction features
      time_score_interaction = .data$time_remaining_pct * .data$points_diff,
      time_score_abs_interaction = .data$time_remaining_pct * abs(.data$points_diff),
      epv_time_interaction = .data$exp_pts * .data$time_remaining_pct,
      
      # Home advantage features
      home_advantage_late = .data$home * .data$is_final_quarter,
      home_advantage_pressure = .data$home * .data$is_pressure_moment,
      
      # Rolling statistics
      points_diff_ma_5 = zoo::rollmean(.data$points_diff, k = 5, fill = .data$points_diff, align = "right"),
      points_diff_volatility = zoo::rollapply(.data$points_diff, width = 10, FUN = sd, fill = 0, align = "right")
    ) %>%
    dplyr::ungroup() %>%
    # Convert categorical variables
    dplyr::mutate(
      lead_size_category = factor(.data$lead_size_category, 
                                 levels = c("tight", "moderate", "comfortable", "blowout")),
      period = factor(.data$period)
    )
  
  return(df_enhanced)
}

#' Select Enhanced Win Probability Model Variables
#'
#' Selects the appropriate variables for enhanced win probability modeling.
#' Different models handle different variable types - GAM can use factors directly,
#' while XGBoost/LightGBM need one-hot encoded variables.
#'
#' @param df A dataframe with enhanced features
#' @param model_type Type of model ("gam", "xgboost", "lightgbm", "ensemble")
#' @return A dataframe with selected model variables
#' @keywords internal
select_wp_model_vars_enhanced <- function(df, model_type = "ensemble") {
  base_vars <- c(
    "time_remaining", "time_remaining_pct", "quarter_progress",
    "is_final_quarter", "is_final_5_mins", "is_final_2_mins",
    "points_diff", "points_diff_abs", "xpoints_diff",
    "exp_pts", "exp_pts_ma_3", "exp_pts_ma_5",
    "goal_x", "goal_distance", "field_pos_score",
    "epv_swing", "epv_volatility", "points_diff_volatility",
    "is_pressure_moment", "home", "home_advantage_late",
    "time_score_interaction", "epv_time_interaction"
  )
  
  # Model-specific variable selection
  if (model_type == "gam") {
    # GAM can handle factor variables directly
    factor_vars <- c("play_type", "phase_of_play")
    available_factor_vars <- intersect(factor_vars, names(df))
    all_vars <- c(base_vars, available_factor_vars)
    
  } else {
    # XGBoost, LightGBM, and ensemble models need one-hot encoded variables
    dummy_vars <- c(
      "play_type_handball", "play_type_kick", "play_type_reception",
      "phase_of_play_handball_received", "phase_of_play_hard_ball", 
      "phase_of_play_loose_ball", "phase_of_play_set_shot"
    )
    all_vars <- c(base_vars, dummy_vars)
  }
  
  available_vars <- intersect(all_vars, names(df))
  
  # Handle categorical variables (for all model types)
  if ("lead_size_category" %in% names(df) && model_type != "gam") {
    dummy_df <- fastDummies::dummy_cols(df["lead_size_category"], remove_first_dummy = TRUE)
    categorical_vars <- setdiff(names(dummy_df), "lead_size_category")
    available_vars <- c(available_vars, categorical_vars)
  } else if ("lead_size_category" %in% names(df) && model_type == "gam") {
    # GAM can use the factor directly
    available_vars <- c(available_vars, "lead_size_category")
  }
  
  df %>% dplyr::select(dplyr::all_of(available_vars))
}

#' Load Win Probability Ensemble Model Safely
#'
#' Safely loads the enhanced win probability ensemble model with fallbacks
#'
#' @return List containing model components or NULL if loading fails
#' @keywords internal
load_wp_ensemble_safely <- function() {
  tryCatch({
    # Try to load the ensemble model
    data("wp_model_ensemble", package = "torp", envir = environment())
    
    if (!exists("wp_model_ensemble")) {
      cli::cli_warn("Enhanced ensemble model not found, falling back to basic model")
      return(NULL)
    }
    
    # Validate model components
    required_components <- c("xgb_model", "ensemble_weights", "feature_names")
    missing_components <- setdiff(required_components, names(wp_model_ensemble))
    
    if (length(missing_components) > 0) {
      cli::cli_warn("Incomplete ensemble model, missing: {paste(missing_components, collapse = ', ')}")
      return(NULL)
    }
    
    return(wp_model_ensemble)
    
  }, error = function(e) {
    cli::cli_warn("Failed to load enhanced model: {e$message}")
    return(NULL)
  })
}

#' Get Enhanced Win Probability Predictions
#'
#' Generates win probability predictions using the enhanced ensemble model
#'
#' @param df A dataframe containing play-by-play data
#' @return A dataframe with win probability predictions
#' @export
#' @importFrom dplyr bind_cols mutate
#' @importFrom cli cli_warn cli_abort
get_wp_preds_enhanced <- function(df) {
  # Input validation
  if (!is.data.frame(df) || nrow(df) == 0) {
    cli::cli_abort("Input must be a non-empty data frame")
  }
  
  # Load ensemble model
  ensemble_model <- load_wp_ensemble_safely()
  
  if (is.null(ensemble_model)) {
    cli::cli_warn("Enhanced model unavailable, falling back to basic predictions")
    return(get_wp_preds(df))
  }
  
  tryCatch({
    # Create enhanced features
    df_enhanced <- create_wp_features_enhanced(df)
    
    # Select model variables
    model_features <- select_wp_model_vars_enhanced(df_enhanced)
    
    # Prepare feature matrix
    feature_matrix <- model.matrix(~ . - 1, data = model_features)
    
    # Handle missing features gracefully
    expected_features <- ensemble_model$feature_names
    missing_features <- setdiff(expected_features, colnames(feature_matrix))
    
    if (length(missing_features) > 0) {
      # Add missing features as zeros
      missing_matrix <- matrix(0, nrow = nrow(feature_matrix), ncol = length(missing_features))
      colnames(missing_matrix) <- missing_features
      feature_matrix <- cbind(feature_matrix, missing_matrix)
    }
    
    # Reorder columns to match training
    feature_matrix <- feature_matrix[, expected_features, drop = FALSE]
    
    # Generate predictions from ensemble components
    pred_xgb <- predict(ensemble_model$xgb_model, feature_matrix)
    
    # Handle other models if available
    if (!is.null(ensemble_model$lgb_model)) {
      pred_lgb <- predict(ensemble_model$lgb_model, feature_matrix)
    } else {
      pred_lgb <- pred_xgb  # Fallback to XGBoost
    }
    
    if (!is.null(ensemble_model$gam_model)) {
      pred_gam <- predict(ensemble_model$gam_model, df_enhanced, type = "response")
    } else {
      pred_gam <- pred_xgb  # Fallback to XGBoost
    }
    
    # Ensemble prediction
    weights <- ensemble_model$ensemble_weights
    if (length(weights) != 3) {
      weights <- c(1, 0, 0)  # Fallback to XGBoost only
    }
    
    ensemble_pred <- weights[1] * pred_xgb + 
                     weights[2] * pred_lgb + 
                     weights[3] * pred_gam
    
    # Bound predictions
    ensemble_pred <- pmax(0.001, pmin(0.999, ensemble_pred))
    
    return(data.frame(wp = ensemble_pred))
    
  }, error = function(e) {
    cli::cli_warn("Enhanced prediction failed: {e$message}. Using basic model.")
    return(get_wp_preds(df))
  })
}