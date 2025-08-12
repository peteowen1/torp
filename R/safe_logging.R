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
  message(paste("INFO:", message))
}

#' Safe Log Warning  
#' @param message Message to log
#' @param ... Additional named parameters
safe_log_warn <- function(message, ...) {
  warning(paste("WARN:", message))
}

#' Safe Log Error
#' @param message Message to log
#' @param ... Additional named parameters
safe_log_error <- function(message, ...) {
  message(paste("ERROR:", message))
}

#' Safe Log Debug
#' @param message Message to log
#' @param ... Additional named parameters
safe_log_debug <- function(message, ...) {
  # Only show debug in interactive mode
  if (interactive()) {
    message(paste("DEBUG:", message))
  }
}