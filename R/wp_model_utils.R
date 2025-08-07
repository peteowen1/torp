# Win Probability Model Utilities
# ===============================
# Utilities for model versioning, validation, and management

#' Validate Win Probability Input Data
#'
#' Comprehensive validation for win probability model inputs
#'
#' @param df Input dataframe
#' @param model_type Type of model ("basic" or "enhanced")
#' @return Logical indicating if validation passed
#' @keywords internal
#' @importFrom cli cli_abort cli_warn
validate_wp_input <- function(df, model_type = "basic") {
  # Basic validation
  if (!is.data.frame(df)) {
    cli::cli_abort("Input must be a data frame")
  }
  
  if (nrow(df) == 0) {
    cli::cli_abort("Input data frame cannot be empty")
  }
  
  # Model-specific validation
  if (model_type == "enhanced") {
    required_cols <- c("match_id", "period", "period_seconds", "points_diff", 
                      "exp_pts", "goal_x", "y", "home", "team_id_mdl")
  } else {
    required_cols <- c("total_seconds", "shot_row", "home", "points_diff", "xpoints_diff")
  }
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_warn("Missing required columns for {model_type} model: {paste(missing_cols, collapse = ', ')}")
    return(FALSE)
  }
  
  # Data quality checks
  if (model_type == "enhanced") {
    # Check for reasonable values
    if (any(df$period < 1 | df$period > 4, na.rm = TRUE)) {
      cli::cli_warn("Invalid period values detected")
    }
    
    if (any(df$period_seconds < 0 | df$period_seconds > 2000, na.rm = TRUE)) {
      cli::cli_warn("Invalid period_seconds values detected") 
    }
    
    if (any(abs(df$points_diff) > 200, na.rm = TRUE)) {
      cli::cli_warn("Extreme points_diff values detected")
    }
  }
  
  return(TRUE)
}

#' Get Win Probability Model Version Information
#'
#' Returns version and metadata for the current win probability model
#'
#' @return List with model version information
#' @export
get_wp_model_info <- function() {
  tryCatch({
    # Try to load ensemble model info
    ensemble_model <- load_wp_ensemble_safely()
    
    if (!is.null(ensemble_model) && "version" %in% names(ensemble_model)) {
      return(list(
        model_type = "enhanced_ensemble",
        version = ensemble_model$version,
        created_date = ensemble_model$created_date,
        training_seasons = ensemble_model$preprocessing_info$training_seasons,
        n_training_obs = ensemble_model$preprocessing_info$n_train,
        components = names(ensemble_model)[grepl("_model$", names(ensemble_model))]
      ))
    } else {
      return(list(
        model_type = "basic",
        version = "1.0",
        created_date = NA,
        training_seasons = "unknown",
        n_training_obs = NA,
        components = "xgboost"
      ))
    }
  }, error = function(e) {
    return(list(
      model_type = "basic",
      version = "1.0",
      error = e$message
    ))
  })
}

#' Check Win Probability Model Health
#'
#' Performs basic health checks on the win probability model
#'
#' @return List with health check results
#' @export
check_wp_model_health <- function() {
  health_results <- list(
    basic_model_available = FALSE,
    enhanced_model_available = FALSE,
    data_objects_loaded = FALSE,
    errors = character(0)
  )
  
  # Check basic model
  tryCatch({
    data("wp_model", package = "torp", envir = environment())
    if (exists("wp_model")) {
      health_results$basic_model_available <- TRUE
    }
  }, error = function(e) {
    health_results$errors <- c(health_results$errors, paste("Basic model:", e$message))
  })
  
  # Check enhanced model
  tryCatch({
    ensemble_model <- load_wp_ensemble_safely()
    if (!is.null(ensemble_model)) {
      health_results$enhanced_model_available <- TRUE
    }
  }, error = function(e) {
    health_results$errors <- c(health_results$errors, paste("Enhanced model:", e$message))
  })
  
  # Check data objects
  tryCatch({
    data_objects <- data(package = "torp")$results[, "Item"]
    required_data <- c("ep_model", "fixtures", "results")
    available_data <- intersect(required_data, data_objects)
    
    if (length(available_data) == length(required_data)) {
      health_results$data_objects_loaded <- TRUE
    }
  }, error = function(e) {
    health_results$errors <- c(health_results$errors, paste("Data objects:", e$message))
  })
  
  # Overall health assessment
  health_results$overall_health <- ifelse(
    health_results$basic_model_available && health_results$data_objects_loaded,
    "healthy", "unhealthy"
  )
  
  return(health_results)
}