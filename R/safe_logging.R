# Safe Logging Utilities
# ======================
# Version-compatible logging functions to handle different logger versions

#' Safe Logger Functions
#' 
#' These functions provide version-compatible logging that falls back to 
#' base R messaging if logger package issues occur
#' 

#' Safe Log Info
#' @param message Message to log
#' @param ... Additional named parameters
safe_log_info <- function(message, ...) {
  tryCatch({
    if (requireNamespace("logger", quietly = TRUE)) {
      logger::log_info(message, ...)
    } else {
      message(paste("INFO:", message))
    }
  }, error = function(e) {
    message(paste("INFO:", message))
  })
}

#' Safe Log Warning  
#' @param message Message to log
#' @param ... Additional named parameters
safe_log_warn <- function(message, ...) {
  tryCatch({
    if (requireNamespace("logger", quietly = TRUE)) {
      logger::log_warn(message, ...)
    } else {
      warning(paste("WARN:", message))
    }
  }, error = function(e) {
    warning(paste("WARN:", message))
  })
}

#' Safe Log Error
#' @param message Message to log
#' @param ... Additional named parameters
safe_log_error <- function(message, ...) {
  tryCatch({
    if (requireNamespace("logger", quietly = TRUE)) {
      logger::log_error(message, ...)
    } else {
      message(paste("ERROR:", message))
    }
  }, error = function(e) {
    message(paste("ERROR:", message))
  })
}

#' Safe Log Debug
#' @param message Message to log
#' @param ... Additional named parameters
safe_log_debug <- function(message, ...) {
  tryCatch({
    if (requireNamespace("logger", quietly = TRUE)) {
      logger::log_debug(message, ...)
    } else {
      # Only show debug in interactive mode
      if (interactive()) {
        message(paste("DEBUG:", message))
      }
    }
  }, error = function(e) {
    if (interactive()) {
      message(paste("DEBUG:", message))
    }
  })
}