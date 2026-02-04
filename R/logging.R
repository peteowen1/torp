# TORP Logging and Monitoring Framework
# ======================================
# Simple logging and monitoring for TORP models without external dependencies
# Combines logging_monitoring.R and safe_logging.R into a single file

# Package-level environment for logging state (avoids global state pollution)
.torp_logging_env <- new.env(parent = emptyenv())
.torp_logging_env$log_level <- "INFO"
.torp_logging_env$log_file <- NULL
.torp_logging_env$console_output <- FALSE

# -----------------------------------------------------------------------------
# Core Logging Setup
# -----------------------------------------------------------------------------

#' Initialize TORP Logging (Internal)
#'
#' Sets up simple logging configuration when package loads
#' @keywords internal
.setup_torp_logging <- function() {
  .torp_logging_env$log_level <- "INFO"
  .torp_logging_env$log_file <- NULL
  .torp_logging_env$console_output <- FALSE
}

#' Setup TORP Logging Configuration
#'
#' Configures logging for the TORP package with appropriate levels and outputs
#'
#' @param level Logging level ("DEBUG", "INFO", "WARN", "ERROR")
#' @param log_file Path to log file (optional)
#' @param console_output Logical, whether to output to console
#' @export
setup_torp_logging <- function(level = "INFO", log_file = NULL, console_output = FALSE) {
  .torp_logging_env$log_level <- toupper(level)
  .torp_logging_env$log_file <- log_file
  .torp_logging_env$console_output <- console_output

  if (console_output) {
    message(paste("TORP logging initialized - Level:", level,
                  "| File:", ifelse(is.null(log_file), "none", log_file),
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
  if (isTRUE(.torp_logging_env$console_output)) {
    message(paste("INFO:", message))
  }
}

#' Safe Log Warning
#'
#' @param message Message to log
#' @param ... Additional named parameters (ignored)
#' @keywords internal
safe_log_warn <- function(message, ...) {
  if (isTRUE(.torp_logging_env$console_output)) {
    warning(paste("WARN:", message), call. = FALSE)
  }
}

#' Safe Log Error
#'
#' @param message Message to log
#' @param ... Additional named parameters (ignored)
#' @keywords internal
safe_log_error <- function(message, ...) {
  if (isTRUE(.torp_logging_env$console_output)) {
    message(paste("ERROR:", message))
  }
}

#' Safe Log Debug
#'
#' @param message Message to log
#' @param ... Additional named parameters (ignored)
#' @keywords internal
safe_log_debug <- function(message, ...) {
  if (isTRUE(.torp_logging_env$console_output) &&
      .torp_logging_env$log_level == "DEBUG" &&
      interactive()) {
    message(paste("DEBUG:", message))
  }
}

# -----------------------------------------------------------------------------
# Model Performance Logging
# -----------------------------------------------------------------------------

#' Log Model Performance Metrics
#'
#' Logs model performance metrics in a structured format for monitoring
#'
#' @param model_name Name of the model
#' @param metrics Named list of performance metrics
#' @param data_info Information about the data used
#' @param model_version Version of the model
#' @export
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
#' Monitors for potential model drift by comparing performance metrics
#'
#' @param model_name Name of the model to monitor
#' @param current_metrics Current performance metrics
#' @param baseline_metrics Baseline metrics for comparison
#' @param drift_threshold Threshold for triggering drift alert (default: 0.05)
#' @export
monitor_model_drift <- function(model_name, current_metrics, baseline_metrics, drift_threshold = 0.05) {
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

# -----------------------------------------------------------------------------
# Dashboard/Health Functions (Stubs - require persistent backend)
# -----------------------------------------------------------------------------

#' Create Monitoring Dashboard Data
#'
#' Aggregates monitoring data for dashboard visualization.
#' Note: Requires a persistent logging backend to be configured.
#' Currently returns a stub structure for API compatibility.
#'
#' @param time_range Time range for aggregation (hours as numeric, or string like "24h", "7d")
#' @param time_window Time window for aggregation (for backward compatibility)
#' @param models Vector of model names to include (optional)
#' @return List containing dashboard data structure (placeholder values)
#' @keywords internal
create_monitoring_dashboard_data <- function(time_range = NULL, time_window = "24h", models = NULL) {
  if (!is.null(time_range)) {
    time_window <- if (is.numeric(time_range)) paste0(time_range, "h") else time_range
  }

  list(
    summary = list(
      time_window = time_window,
      models_monitored = 0L,
      last_updated = Sys.time(),
      status = "no_data"
    ),
    metrics = data.frame(
      model = character(0),
      auc = numeric(0),
      log_loss = numeric(0),
      predictions_made = integer(0),
      stringsAsFactors = FALSE
    ),
    time_series = data.frame(
      timestamp = as.POSIXct(character(0)),
      predictions_count = integer(0),
      error_rate = numeric(0),
      stringsAsFactors = FALSE
    ),
    message = "Monitoring dashboard requires a persistent logging backend. No historical data available."
  )
}

#' Get Model Health Status
#'
#' Retrieves the health status of models based on recent performance metrics.
#' Note: Requires a persistent logging backend to be configured.
#' Currently returns a stub structure for API compatibility.
#'
#' @param lookback_hours Number of hours to look back for health assessment (default: 24)
#' @param models Vector of model names to check (optional)
#' @return List containing model health status information (placeholder values)
#' @keywords internal
get_model_health_status <- function(lookback_hours = 24, models = NULL) {
  list(
    status = "unknown",
    models = models %||% character(0),
    timestamp = Sys.time(),
    lookback_hours = lookback_hours,
    models_checked = 0L,
    overall_health = "unknown",
    model_statuses = list(),
    alerts = list(),
    last_updated = Sys.time(),
    message = "Health monitoring requires a persistent logging backend. No status data available."
  )
}

# -----------------------------------------------------------------------------
# Utility Operators
# -----------------------------------------------------------------------------

#' @title Null coalescing operator
#' @description Returns the left operand if not NULL, otherwise the right operand
#' @param x Left operand
#' @param y Right operand
#' @return x if not NULL, otherwise y
#' @name null-coalesce
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
