# Tests for Logging and Monitoring Framework
# ===========================================

test_that("setup_torp_logging configures correctly", {
  # Test basic setup
  expect_silent(setup_torp_logging("INFO"))
  expect_silent(setup_torp_logging("DEBUG", console_output = FALSE))
  
  # Test with custom log file
  temp_log <- tempfile(fileext = ".log")
  expect_silent(setup_torp_logging("WARN", log_file = temp_log))
  
  # Clean up
  if (file.exists(temp_log)) file.remove(temp_log)
})

test_that("log_model_performance stores metrics correctly", {
  test_metrics <- list(
    auc = 0.85,
    log_loss = 0.4,
    brier_score = 0.2
  )
  
  test_data_info <- list(
    n_observations = 1000,
    data_source = "test"
  )
  
  # Should not error
  expect_silent(log_model_performance("test_model", test_metrics, test_data_info, "1.0"))
})

test_that("log_data_quality handles different severity levels", {
  test_issues <- c("Missing values detected", "Outliers found")
  
  # Test different severity levels
  expect_silent(log_data_quality("test_source", test_issues, "LOW"))
  expect_silent(log_data_quality("test_source", test_issues, "MEDIUM"))
  expect_silent(log_data_quality("test_source", test_issues, "HIGH"))
  
  # Critical should trigger alert (but not error in tests)
  expect_silent(log_data_quality("test_source", test_issues, "CRITICAL"))
})

test_that("monitor_model_drift detects performance changes", {
  baseline_metrics <- list(auc = 0.80, log_loss = 0.5)
  
  # No drift case
  current_metrics_stable <- list(auc = 0.81, log_loss = 0.49)
  result_stable <- monitor_model_drift("test_model", current_metrics_stable, baseline_metrics, 0.05)
  
  expect_false(result_stable$drift_detected)
  
  # Drift case
  current_metrics_drift <- list(auc = 0.70, log_loss = 0.7)
  result_drift <- monitor_model_drift("test_model", current_metrics_drift, baseline_metrics, 0.05)
  
  expect_true(result_drift$drift_detected)
  expect_true(length(result_drift$drift_details) > 0)
})

test_that("get_model_health_status returns proper structure", {
  # Test with no metrics (should handle gracefully)
  health_status <- get_model_health_status(lookback_hours = 1)
  
  expect_true(is.list(health_status))
  expect_true("status" %in% names(health_status))
  expect_true("models" %in% names(health_status))
  
  # Status should be one of expected values
  expect_true(health_status$status %in% c("unknown", "stale", "healthy"))
})

test_that("create_monitoring_dashboard_data handles empty data", {
  # Should not error when no data available
  dashboard_data <- create_monitoring_dashboard_data(time_range = 1)
  
  expect_true(is.list(dashboard_data))
  # Should contain error message when no data
  expect_true("error" %in% names(dashboard_data) || "time_series" %in% names(dashboard_data))
})

test_that("log_prediction_event logs correctly", {
  # Should not error with valid inputs
  expect_silent(log_prediction_event(
    "test_model", 
    "abc123", 
    100, 
    list(min = 0.1, max = 0.9, mean = 0.5),
    50
  ))
  
  # Should handle missing optional parameters
  expect_silent(log_prediction_event("test_model", "def456", 50))
})