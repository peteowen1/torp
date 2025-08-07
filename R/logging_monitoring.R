# Logging and Monitoring Framework
# =================================
# Production-ready logging, monitoring, and observability for TORP models

#' Initialize TORP Logging (Internal)
#' 
#' Sets up logging configuration when package loads
#' @keywords internal
.setup_torp_logging <- function() {
  tryCatch({
    if (requireNamespace("logger", quietly = TRUE)) {
      # Use simple layout to avoid version compatibility issues
      logger::log_layout(logger::layout_simple)
      logger::log_threshold(logger::INFO)
      
      # Create logs directory if it doesn't exist
      log_dir <- file.path(tempdir(), "torp_logs")
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE)
      }
      
      # Set up file appender
      logger::log_appender(logger::appender_tee(file.path(log_dir, "torp.log")))
    }
  }, error = function(e) {
    # Silently fail if logger setup has issues
    NULL
  })
}

#' Setup TORP Logging Configuration
#'
#' Configures logging for the TORP package with appropriate levels and outputs
#'
#' @param level Logging level ("DEBUG", "INFO", "WARN", "ERROR")
#' @param log_file Path to log file (optional)
#' @param console_output Logical, whether to output to console
#' @export
setup_torp_logging <- function(level = "INFO", log_file = NULL, console_output = TRUE) {
  
  # Convert level to logger format
  log_level <- switch(toupper(level),
    "DEBUG" = logger::DEBUG,
    "INFO" = logger::INFO, 
    "WARN" = logger::WARN,
    "ERROR" = logger::ERROR,
    logger::INFO
  )
  
  logger::log_threshold(log_level)
  
  # Configure appenders
  if (!is.null(log_file)) {
    if (console_output) {
      logger::log_appender(logger::appender_tee(log_file))
    } else {
      logger::log_appender(logger::appender_file(log_file))
    }
  } else if (!console_output) {
    logger::log_appender(logger::appender_console)
  }
  
  logger::log_info("TORP logging initialized", level = level, log_file = log_file %||% "console")
}

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
  
  log_entry <- list(
    timestamp = Sys.time(),
    event_type = "model_performance",
    model_name = model_name,
    model_version = model_version %||% "unknown",
    metrics = metrics,
    data_info = data_info
  )
  
  logger::log_info("Model performance logged", 
                   model = model_name,
                   auc = metrics$auc %||% NA,
                   log_loss = metrics$log_loss %||% NA,
                   n_obs = data_info$n_observations %||% NA)
  
  # Store detailed metrics for monitoring system
  store_performance_metrics(log_entry)
}

#' Log Data Quality Issues
#'
#' Logs data quality problems for monitoring and alerting
#'
#' @param data_source Source of the data
#' @param issues List of data quality issues found
#' @param severity Severity level ("LOW", "MEDIUM", "HIGH", "CRITICAL")
#' @export
log_data_quality <- function(data_source, issues, severity = "MEDIUM") {
  
  log_func <- switch(toupper(severity),
    "LOW" = logger::log_info,
    "MEDIUM" = logger::log_warn,
    "HIGH" = logger::log_error,
    "CRITICAL" = logger::log_error,
    logger::log_warn
  )
  
  log_func("Data quality issues detected",
           data_source = data_source,
           severity = severity,
           n_issues = length(issues),
           issues = paste(issues, collapse = "; "))
  
  # Alert on critical issues
  if (toupper(severity) == "CRITICAL") {
    send_alert("CRITICAL DATA QUALITY ISSUE", 
               paste("Source:", data_source, "Issues:", paste(issues, collapse = ", ")))
  }
}

#' Log Model Prediction Event
#'
#' Logs individual model predictions with context for debugging and monitoring
#'
#' @param model_name Name of the model making predictions
#' @param input_hash Hash of input data for tracking
#' @param n_predictions Number of predictions made
#' @param prediction_summary Summary statistics of predictions
#' @param processing_time Time taken for predictions
#' @export
log_prediction_event <- function(model_name, input_hash, n_predictions, 
                                prediction_summary = NULL, processing_time = NULL) {
  
  logger::log_debug("Model prediction event",
                    model = model_name,
                    input_hash = input_hash,
                    n_predictions = n_predictions,
                    pred_min = prediction_summary$min %||% NA,
                    pred_max = prediction_summary$max %||% NA,
                    pred_mean = prediction_summary$mean %||% NA,
                    processing_time_ms = processing_time %||% NA)
}

#' Monitor Model Drift
#'
#' Monitors for model drift by comparing current performance to baseline
#'
#' @param model_name Name of the model
#' @param current_metrics Current performance metrics
#' @param baseline_metrics Baseline performance metrics
#' @param drift_threshold Threshold for significant drift (default: 0.05)
#' @return List indicating if drift was detected
#' @export
monitor_model_drift <- function(model_name, current_metrics, baseline_metrics, 
                               drift_threshold = 0.05) {
  
  drift_detected <- FALSE
  drift_details <- list()
  
  # Check AUC drift
  if (!is.null(current_metrics$auc) && !is.null(baseline_metrics$auc)) {
    auc_diff <- abs(current_metrics$auc - baseline_metrics$auc)
    if (auc_diff > drift_threshold) {
      drift_detected <- TRUE
      drift_details$auc <- list(
        current = current_metrics$auc,
        baseline = baseline_metrics$auc,
        difference = auc_diff
      )
    }
  }
  
  # Check Log Loss drift  
  if (!is.null(current_metrics$log_loss) && !is.null(baseline_metrics$log_loss)) {
    log_loss_ratio <- current_metrics$log_loss / baseline_metrics$log_loss
    if (log_loss_ratio > (1 + drift_threshold) || log_loss_ratio < (1 - drift_threshold)) {
      drift_detected <- TRUE
      drift_details$log_loss <- list(
        current = current_metrics$log_loss,
        baseline = baseline_metrics$log_loss,
        ratio = log_loss_ratio
      )
    }
  }
  
  if (drift_detected) {
    logger::log_warn("Model drift detected",
                     model = model_name,
                     drift_details = jsonlite::toJSON(drift_details, auto_unbox = TRUE))
    
    send_alert("MODEL DRIFT DETECTED", 
               paste("Model:", model_name, "- Performance degradation detected"))
  } else {
    logger::log_info("Model performance stable", model = model_name)
  }
  
  return(list(
    drift_detected = drift_detected,
    drift_details = drift_details
  ))
}

#' Store Performance Metrics
#'
#' Stores performance metrics in a structured format for time series analysis
#'
#' @param log_entry Performance log entry
#' @keywords internal
store_performance_metrics <- function(log_entry) {
  
  # Create metrics storage directory
  metrics_dir <- file.path(tempdir(), "torp_metrics")
  if (!dir.exists(metrics_dir)) {
    dir.create(metrics_dir, recursive = TRUE)
  }
  
  # Generate filename with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(metrics_dir, paste0("metrics_", timestamp, ".json"))
  
  # Store as JSON
  tryCatch({
    jsonlite::write_json(log_entry, filename, auto_unbox = TRUE, pretty = TRUE)
  }, error = function(e) {
    logger::log_error("Failed to store performance metrics", error = e$message)
  })
}

#' Send Alert
#'
#' Sends alerts for critical issues (placeholder for integration with alerting systems)
#'
#' @param title Alert title
#' @param message Alert message
#' @keywords internal
send_alert <- function(title, message) {
  
  # Placeholder for integration with alerting systems (Slack, email, etc.)
  logger::log_error("ALERT", title = title, message = message)
  
  # In production, this would integrate with:
  # - Slack webhooks
  # - Email notifications
  # - PagerDuty
  # - Custom monitoring systems
  
  alert_entry <- list(
    timestamp = Sys.time(),
    title = title,
    message = message,
    severity = "HIGH"
  )
  
  # Store alert for later processing
  alerts_dir <- file.path(tempdir(), "torp_alerts")
  if (!dir.exists(alerts_dir)) {
    dir.create(alerts_dir, recursive = TRUE)
  }
  
  alert_file <- file.path(alerts_dir, paste0("alert_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"))
  jsonlite::write_json(alert_entry, alert_file, auto_unbox = TRUE)
}

#' Get Model Health Status
#'
#' Retrieves current health status of all models with recent performance data
#'
#' @param lookback_hours Number of hours to look back for recent data (default: 24)
#' @return List containing health status of all models
#' @export
get_model_health_status <- function(lookback_hours = 24) {
  
  # Get recent performance metrics
  metrics_dir <- file.path(tempdir(), "torp_metrics")
  
  if (!dir.exists(metrics_dir)) {
    return(list(
      status = "unknown",
      message = "No metrics directory found",
      models = list()
    ))
  }
  
  # Find recent metric files
  cutoff_time <- Sys.time() - as.difftime(lookback_hours, units = "hours")
  metric_files <- list.files(metrics_dir, pattern = "metrics_.*\\.json", full.names = TRUE)
  
  recent_files <- metric_files[file.mtime(metric_files) > cutoff_time]
  
  if (length(recent_files) == 0) {
    return(list(
      status = "stale",
      message = paste("No recent metrics found in last", lookback_hours, "hours"),
      models = list()
    ))
  }
  
  # Parse recent metrics
  model_status <- list()
  
  for (file in recent_files) {
    tryCatch({
      metrics <- jsonlite::fromJSON(file)
      model_name <- metrics$model_name
      
      if (is.null(model_status[[model_name]])) {
        model_status[[model_name]] <- list(
          last_updated = metrics$timestamp,
          recent_metrics = list(),
          status = "healthy"
        )
      }
      
      model_status[[model_name]]$recent_metrics <- append(
        model_status[[model_name]]$recent_metrics, 
        list(metrics)
      )
      
      # Update last seen time
      if (as.POSIXct(metrics$timestamp) > as.POSIXct(model_status[[model_name]]$last_updated)) {
        model_status[[model_name]]$last_updated <- metrics$timestamp
      }
      
    }, error = function(e) {
      logger::log_warn("Failed to parse metrics file", file = file, error = e$message)
    })
  }
  
  # Assess overall health
  overall_status <- if (length(model_status) > 0) "healthy" else "unknown"
  
  return(list(
    status = overall_status,
    timestamp = Sys.time(),
    lookback_hours = lookback_hours,
    models = model_status
  ))
}

#' Create Model Monitoring Dashboard Data
#'
#' Generates data for model monitoring dashboards
#'
#' @param model_name Optional specific model name (default: all models)
#' @param time_range Number of days to include (default: 7)
#' @return List containing dashboard data
#' @export
create_monitoring_dashboard_data <- function(model_name = NULL, time_range = 7) {
  
  metrics_dir <- file.path(tempdir(), "torp_metrics")
  
  if (!dir.exists(metrics_dir)) {
    return(list(error = "No metrics data available"))
  }
  
  # Get metric files within time range
  cutoff_time <- Sys.time() - as.difftime(time_range, units = "days")
  metric_files <- list.files(metrics_dir, pattern = "metrics_.*\\.json", full.names = TRUE)
  recent_files <- metric_files[file.mtime(metric_files) > cutoff_time]
  
  if (length(recent_files) == 0) {
    return(list(error = "No recent metrics data available"))
  }
  
  # Parse all metrics
  all_metrics <- list()
  
  for (file in recent_files) {
    tryCatch({
      metrics <- jsonlite::fromJSON(file)
      if (is.null(model_name) || metrics$model_name == model_name) {
        all_metrics <- append(all_metrics, list(metrics))
      }
    }, error = function(e) {
      logger::log_debug("Skipped malformed metrics file", file = file)
    })
  }
  
  if (length(all_metrics) == 0) {
    return(list(error = "No matching metrics found"))
  }
  
  # Transform for dashboard
  dashboard_data <- list(
    time_range = time_range,
    n_measurements = length(all_metrics),
    models = unique(sapply(all_metrics, function(x) x$model_name)),
    
    # Time series data for plotting
    time_series = data.frame(
      timestamp = sapply(all_metrics, function(x) x$timestamp),
      model_name = sapply(all_metrics, function(x) x$model_name),
      auc = sapply(all_metrics, function(x) x$metrics$auc %||% NA),
      log_loss = sapply(all_metrics, function(x) x$metrics$log_loss %||% NA),
      brier_score = sapply(all_metrics, function(x) x$metrics$brier_score %||% NA),
      n_observations = sapply(all_metrics, function(x) x$data_info$n_observations %||% NA)
    ),
    
    # Summary statistics
    summary = list(
      avg_auc = mean(sapply(all_metrics, function(x) x$metrics$auc %||% NA), na.rm = TRUE),
      avg_log_loss = mean(sapply(all_metrics, function(x) x$metrics$log_loss %||% NA), na.rm = TRUE),
      total_predictions = sum(sapply(all_metrics, function(x) x$data_info$n_observations %||% 0), na.rm = TRUE)
    )
  )
  
  return(dashboard_data)
}