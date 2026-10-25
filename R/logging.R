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
  .torp_logging_env$log_level <- match.arg(toupper(level), names(.LOG_LEVELS))
  .torp_logging_env$console_output <- console_output

  if (console_output) {
    message(paste("TORP logging initialized - Level:", level,
                  "| Console:", console_output))
  }
}

# -----------------------------------------------------------------------------
# Prediction Logging
# -----------------------------------------------------------------------------

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
  if (.should_log("DEBUG") && interactive()) {
    message(paste("DEBUG: Model prediction event - Model:", model_name,
                  "| Input Hash:", input_hash,
                  "| N Predictions:", n_predictions))
  }
}

# Note: %||% operator is provided by rlang (already in Imports)
#' @importFrom rlang %||%
NULL
