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

test_that("setup_torp_logging rejects bad levels", {
  expect_error(setup_torp_logging(level = "VERBOSE"))
})