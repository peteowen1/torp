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

  required_cols <- c("total_game_time_elapsed", "total_game_time_remaining", "shot_row", "home", "points_diff", "xpoints_diff")

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_warn("Missing required columns: {paste(missing_cols, collapse = ', ')}")
    return(FALSE)
  }

  return(TRUE)
}

#' Get Win Probability Model Version Information
#'
#' @description Internal function.
#' Returns version and metadata for the current win probability model
#'
#' @return List with model version information
#' @keywords internal
get_wp_model_info <- function() {
  list(
    model_type = "basic",
    version = "1.0",
    components = "xgboost"
  )
}

#' Check Win Probability Model Health
#'
#' @description Internal function.
#' Performs basic health checks on the win probability model
#'
#' @return List with health check results
#' @keywords internal
check_wp_model_health <- function() {
  errors <- character(0)

  # Check WP model via standard loading path
  wp_available <- FALSE
  tryCatch({
    wp <- load_model_with_fallback("wp")
    wp_available <- !is.null(wp)
  }, error = function(e) {
    errors <<- c(errors, paste("WP model:", e$message))
  })

  # Check EP model via standard loading path
  ep_available <- FALSE
  tryCatch({
    ep <- load_model_with_fallback("ep")
    ep_available <- !is.null(ep)
  }, error = function(e) {
    errors <<- c(errors, paste("EP model:", e$message))
  })

  overall <- if (wp_available && ep_available) "healthy" else "unhealthy"

  list(
    wp_model_available = wp_available,
    ep_model_available = ep_available,
    errors = errors,
    overall_health = overall
  )
}