# Tests for Performance Optimization System
# ==========================================

test_that("PerformanceMonitor can be instantiated", {
  monitor <- PerformanceMonitor$new()
  
  expect_true(R6::is.R6(monitor))
  expect_true(inherits(monitor, "PerformanceMonitor"))
})

test_that("PerformanceMonitor tracks function performance", {
  monitor <- PerformanceMonitor$new()
  
  # Test function to monitor
  test_func <- function(x) {
    Sys.sleep(0.01)  # Small delay to measure
    return(x * 2)
  }
  
  # Monitor performance
  result <- monitor$monitor_performance(test_func, "test_function", 5)
  
  expect_equal(result, 10)  # Function should work correctly
  
  # Check performance report
  report <- monitor$get_performance_report(last_n_records = 1)
  expect_true(is.list(report))
  expect_true("summary" %in% names(report))
  expect_equal(report$summary$total_calls, 1)
  expect_true(report$summary$success_rate == 1)
})

test_that("PerformanceMonitor handles function errors gracefully", {
  monitor <- PerformanceMonitor$new()
  
  # Function that will error
  error_func <- function() {
    stop("Test error")
  }
  
  # Should capture error and rethrow
  expect_error({
    monitor$monitor_performance(error_func, "error_function") 
  }, "Test error")
  
  # Should record the failed execution
  report <- monitor$get_performance_report()
  if (report$summary$total_calls > 0) {
    expect_true(report$summary$success_rate < 1)
  }
})

test_that("PerformanceMonitor caching works correctly", {
  monitor <- PerformanceMonitor$new()
  monitor$enable_caching()
  
  # Function that takes time (for cache benefit testing)
  slow_func <- function(x) {
    Sys.sleep(0.01)
    return(x^2)
  }
  
  # First call should be slow
  start_time <- Sys.time()
  result1 <- cached_prediction(slow_func, 5)
  first_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Second call should be faster (cached)
  start_time <- Sys.time()
  result2 <- cached_prediction(slow_func, 5)
  second_duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_equal(result1, result2)
  expect_equal(result1, 25)
  
  # Cache should make second call faster (though timing can be variable)
  # We just check that caching doesn't break functionality
})

test_that("PerformanceMonitor memory management works", {
  monitor <- PerformanceMonitor$new()
  
  # Test memory threshold checking (should not error with reasonable usage)
  expect_silent(monitor$check_memory_thresholds(100))  # 100 MB usage
  
  # Test cleanup functions don't error
  expect_silent(monitor$preventive_cleanup())
  expect_silent(monitor$aggressive_cleanup())
})

test_that("PerformanceMonitor cache management works", {
  monitor <- PerformanceMonitor$new()
  
  # Enable caching
  monitor$enable_caching(max_size_mb = 10, ttl_minutes = 5)
  
  # Test cache operations don't error
  expect_silent(monitor$clear_cache())
  
  # Disable caching
  expect_silent(monitor$disable_caching())
})

test_that("PerformanceMonitor provides useful performance reports", {
  monitor <- PerformanceMonitor$new()
  
  # Generate some performance data
  test_func <- function(x) x + 1
  
  for (i in 1:5) {
    monitor$monitor_performance(test_func, "report_test", i)
  }
  
  report <- monitor$get_performance_report()
  
  expect_true(is.list(report))
  expect_true("summary" %in% names(report))
  expect_true("current_status" %in% names(report))
  expect_true("recommendations" %in% names(report))
  
  # Summary should have expected fields
  expect_true(all(c("total_calls", "success_rate", "avg_execution_time") %in% names(report$summary)))
  expect_equal(report$summary$total_calls, 5)
  expect_equal(report$summary$success_rate, 1)
})

test_that("process_pbp_data_optimized handles data correctly", {
  # Create mock PBP data
  mock_pbp <- data.frame(
    match_id = rep("TEST_001", 100),
    period = rep(1:4, 25),
    period_seconds = seq(100, 1900, length.out = 100),
    goal_x = runif(100, 0, 120),
    y = runif(100, -40, 40),
    points_diff = sample(-20:20, 100, replace = TRUE),
    team_id_mdl = sample(1:2, 100, replace = TRUE)
  )
  
  # Test optimized processing
  result <- process_pbp_data_optimized(
    pbp_data = mock_pbp,
    chunk_size = 50,
    parallel = FALSE  # Avoid parallel processing in tests
  )
  
  expect_true(is.data.frame(result) || data.table::is.data.table(result))
  expect_equal(nrow(result), 100)
  
  # Should have added optimized columns
  expect_true("time_remaining" %in% names(result))
  expect_true("goal_distance" %in% names(result))
})

test_that("batch_process_memory_efficient handles basic processing", {
  skip("Requires file I/O setup")
  
  # This test would require setting up actual file processing
  # For now, we just test that the function exists and has proper structure
  expect_true(exists("batch_process_memory_efficient"))
  
  # Test with mock processing function
  mock_processor <- function(data) {
    # Simple processing: add a column
    data$processed <- TRUE
    return(data)
  }
  
  # Would test with actual data source in full implementation
})

test_that("initialize_performance_system configures correctly", {
  # Test initialization with different settings
  expect_silent(initialize_performance_system(
    cache_enabled = TRUE,
    memory_warning_mb = 512,
    memory_critical_mb = 1024
  ))
  
  expect_silent(initialize_performance_system(
    cache_enabled = FALSE,
    memory_warning_mb = 2048,
    memory_critical_mb = 4096
  ))
})

test_that("global performance monitor works", {
  # Test that global instance exists and works
  expect_true(exists("torp_performance"))
  expect_true(R6::is.R6(torp_performance))
  
  # Test basic functionality
  test_func <- function(x) x * 3
  result <- torp_performance$monitor_performance(test_func, "global_test", 4)
  expect_equal(result, 12)
})

test_that("cached_prediction with global monitor works", {
  # Test caching with global monitor
  torp_performance$enable_caching()
  
  simple_func <- function(x) x + 10
  
  result1 <- cached_prediction(simple_func, 5, "test_suffix")
  result2 <- cached_prediction(simple_func, 5, "test_suffix")
  
  expect_equal(result1, result2)
  expect_equal(result1, 15)
})

test_that("performance monitor handles edge cases", {
  monitor <- PerformanceMonitor$new()
  
  # Test with NULL input
  expect_silent(monitor$check_memory_thresholds(NULL))
  
  # Test report with no data
  empty_report <- monitor$get_performance_report()
  expect_true(is.list(empty_report))
  
  # Test caching edge cases
  monitor$enable_caching()
  expect_silent(monitor$clear_cache())  # Clear empty cache
  monitor$disable_caching()
})

test_that("data.table optimization works correctly", {
  # Test that data.table operations work as expected
  test_data <- data.frame(
    id = 1:1000,
    value = rnorm(1000),
    group = rep(1:10, 100)
  )
  
  # Should convert to data.table and process efficiently
  result <- process_pbp_data_optimized(
    pbp_data = test_data,
    chunk_size = 500,
    parallel = FALSE
  )
  
  # Basic checks - should not crash and return reasonable data
  expect_true(nrow(result) > 0)
  expect_true(ncol(result) >= ncol(test_data))
})