# TORP Logging and Monitoring Framework
# ======================================
# Simple logging and monitoring for TORP models without external dependencies
# Combines logging_monitoring.R and safe_logging.R into a single file

# Package-level environment for logging state (avoids global state pollution)
.torp_logging_env <- new.env(parent = emptyenv())
.torp_logging_env$log_level <- "INFO"
.torp_logging_env$console_output <- FALSE

# Numeric severity levels for comparison (higher = more severe)
.LOG_LEVELS <- c(DEBUG = 1L, INFO = 2L, WARN = 3L, ERROR = 4L)

#' Check if a message at the given severity should be emitted
#' @keywords internal
.should_log <- function(msg_level) {
  isTRUE(.torp_logging_env$console_output) &&
    .LOG_LEVELS[[msg_level]] >= .LOG_LEVELS[[.torp_logging_env$log_level]]
}

# -----------------------------------------------------------------------------
# Core Logging Setup
# -----------------------------------------------------------------------------

#' Initialize TORP Logging (Internal)
#'
#' Sets up simple logging configuration when package loads
#' @keywords internal
.setup_torp_logging <- function() {
  .torp_logging_env$log_level <- "INFO"
  .torp_logging_env$console_output <- FALSE
}

#' Setup TORP Logging Configuration
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' Configures logging for the TORP package with appropriate levels and outputs
#'
#' @param level Logging level ("DEBUG", "INFO", "WARN", "ERROR")
#' @param log_file Deprecated; retained for backwards compatibility but unused.
#' @param console_output Logical, whether to output to console
#' @export
setup_torp_logging <- function(level = "INFO", log_file = NULL, console_output = FALSE) {
  .torp_logging_env$log_level <- toupper(level)
  .torp_logging_env$console_output <- console_output

  if (console_output) {
    message(paste("TORP logging initialized - Level:", level,
                  "| Console:", console_output))
  }
}

# -----------------------------------------------------------------------------
# Safe Logging Functions (Version-Compatible)
# -----------------------------------------------------------------------------

#' Safe Log Info
#'
#' Version-compatible logging that falls back to base R messaging
#'
#' @param message Message to log
#' @param ... Additional named parameters (ignored)
#' @keywords internal
safe_log_info <- function(message, ...) {
  if (.should_log("INFO")) {
    message(paste("INFO:", message))
  }
}

#' Safe Log Warning
#'
#' @param message Message to log
#' @param ... Additional named parameters (ignored)
#' @keywords internal
safe_log_warn <- function(message, ...) {
  if (.should_log("WARN")) {
    warning(paste("WARN:", message), call. = FALSE)
  }
}

#' Safe Log Error
#'
#' @param message Message to log
#' @param ... Additional named parameters (ignored)
#' @keywords internal
safe_log_error <- function(message, ...) {
  if (.should_log("ERROR")) {
    warning(paste("ERROR:", message), call. = FALSE)
  }
}

#' Safe Log Debug
#'
#' @param message Message to log
#' @param ... Additional named parameters (ignored)
#' @keywords internal
safe_log_debug <- function(message, ...) {
  if (.should_log("DEBUG") && interactive()) {
    message(paste("DEBUG:", message))
  }
}

# -----------------------------------------------------------------------------
# Model Performance Logging
# -----------------------------------------------------------------------------

#' Log Model Performance Metrics
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' Logs model performance metrics in a structured format for monitoring
#'
#' @param model_name Name of the model
#' @param metrics Named list of performance metrics
#' @param data_info Information about the data used
#' @param model_version Version of the model
#' @keywords internal
log_model_performance <- function(model_name, metrics, data_info = NULL, model_version = NULL) {
  if (isTRUE(.torp_logging_env$console_output)) {
    message(paste("INFO: Model performance logged - Model:", model_name,
                  "| AUC:", metrics$auc %||% NA,
                  "| Log Loss:", metrics$log_loss %||% NA,
                  "| N Obs:", data_info$n_observations %||% NA))
  }
}

#' Log Data Quality Issues
#'
#' Logs data quality problems for monitoring and alerting
#'
#' @param data_source Source of the data
#' @param issues Character vector of issues found
#' @param severity Severity level ("LOW", "MEDIUM", "HIGH", "CRITICAL")
#' @keywords internal
log_data_quality <- function(data_source, issues, severity = "MEDIUM") {
  if (!isTRUE(.torp_logging_env$console_output)) {
    return(invisible(NULL))
  }

  log_func <- switch(severity,
    "LOW" = message,
    "MEDIUM" = warning,
    "HIGH" = warning,
    "CRITICAL" = warning,
    warning
  )

  for (issue in issues) {
    log_func(paste("Data Quality", severity, "- Source:", data_source, "- Issue:", issue))
  }
}

#' Log Prediction Event
#'
#' Logs individual prediction events for debugging and monitoring
#'
#' @param model_name Name of the model making predictions
#' @param input_hash Hash of input data for tracking
#' @param n_predictions Number of predictions made
#' @param summary Optional prediction summary statistics
#' @param ... Additional parameters (ignored)
#' @keywords internal
log_prediction_event <- function(model_name, input_hash, n_predictions, summary = NULL, ...) {
  if (isTRUE(.torp_logging_env$console_output) && interactive()) {
    message(paste("DEBUG: Model prediction event - Model:", model_name,
                  "| Input Hash:", input_hash,
                  "| N Predictions:", n_predictions))
  }
}

# -----------------------------------------------------------------------------
# Model Monitoring
# -----------------------------------------------------------------------------

#' Monitor Model Drift
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' Monitors for potential model drift by comparing performance metrics
#'
#' @param model_name Name of the model to monitor
#' @param current_metrics Current performance metrics
#' @param baseline_metrics Baseline metrics for comparison
#' @param drift_threshold Threshold for triggering drift alert (default: 0.05)
#' @keywords internal
monitor_model_drift <- function(model_name, current_metrics, baseline_metrics, drift_threshold = 0.05) {
  if (is.null(current_metrics$auc) || is.null(baseline_metrics$auc)) {
    cli::cli_warn("Cannot compute drift for {model_name}: missing AUC in metrics")
    return(list(model_name = model_name, drift_detected = NA, auc_drift = NA,
                threshold = drift_threshold, current_auc = current_metrics$auc,
                baseline_auc = baseline_metrics$auc, drift_details = list()))
  }
  auc_drift <- abs(current_metrics$auc - baseline_metrics$auc)
  drift_detected <- auc_drift > drift_threshold

  if (isTRUE(.torp_logging_env$console_output)) {
    if (drift_detected) {
      warning(paste("Model drift detected - Model:", model_name,
                    "| AUC Drift:", round(auc_drift, 4),
                    "| Threshold:", drift_threshold))
    } else {
      message(paste("INFO: Model performance stable - Model:", model_name))
    }
  }

  list(
    model_name = model_name,
    drift_detected = drift_detected,
    auc_drift = auc_drift,
    threshold = drift_threshold,
    current_auc = current_metrics$auc,
    baseline_auc = baseline_metrics$auc,
    drift_details = if (drift_detected) {
      list(
        metric = "auc",
        drift_magnitude = auc_drift,
        drift_direction = if (current_metrics$auc > baseline_metrics$auc) "increase" else "decrease"
      )
    } else {
      list()
    }
  )
}

# Note: %||% operator is provided by rlang (already in Imports)
#' @importFrom rlang %||%
NULL
