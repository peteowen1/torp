# Win Probability Model Utilities
# ===============================
# Utilities for model versioning, validation, and management

#' Validate Win Probability Input Data
#'
#' Validates that the input dataframe has the required columns for WP modeling.
#'
#' @param df Input dataframe
#' @return Logical indicating if validation passed
#' @keywords internal
#' @importFrom cli cli_abort cli_warn
validate_wp_input <- function(df) {
  if (!is.data.frame(df)) {
    cli::cli_abort("Input must be a data frame")
  }

  if (nrow(df) == 0) {
    cli::cli_abort("Input data frame cannot be empty")
  }

  required_cols <- c("total_seconds", "shot_row", "home", "points_diff", "xpoints_diff")

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_warn("Missing required columns: {paste(missing_cols, collapse = ', ')}")
    return(FALSE)
  }

  return(TRUE)
}

#' Get Win Probability Model Version Information
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' Returns version and metadata for the current win probability model
#'
#' @return List with model version information
#' @export
get_wp_model_info <- function() {
  list(
    model_type = "basic",
    version = "1.0",
    components = "xgboost"
  )
}

#' Check Win Probability Model Health
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' Performs basic health checks on the win probability model
#'
#' @return List with health check results
#' @export
check_wp_model_health <- function() {
  health_results <- list(
    basic_model_available = FALSE,
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

  # Check data objects
  tryCatch({
    data_objects <- data(package = "torp")$results[, "Item"]
    required_data <- c("ep_model")
    available_data <- intersect(required_data, data_objects)

    if (length(available_data) == length(required_data)) {
      health_results$data_objects_loaded <- TRUE
    }
  }, error = function(e) {
    health_results$errors <- c(health_results$errors, paste("Data objects:", e$message))
  })

  health_results$overall_health <- ifelse(
    health_results$basic_model_available && health_results$data_objects_loaded,
    "healthy", "unhealthy"
  )

  return(health_results)
}