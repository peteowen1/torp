test_that("utility functions exist and work correctly", {
  # Test that key utility functions exist
  expect_true(exists("get_afl_season"))
  expect_true(exists("get_afl_week"))
  
  # Test AFL season/week functions return reasonable values
  current_season <- get_afl_season()
  expect_reasonable_season(current_season)
  
  current_week <- get_afl_week()
  expect_reasonable_round(current_week)
  
  # Test with specific types
  next_season <- get_afl_season(type = "next")
  expect_reasonable_season(next_season)
  expect_gte(next_season, current_season)
  
  next_week <- get_afl_week(type = "next")
  expect_reasonable_round(next_week)
  
  # Test error handling
  expect_error(get_afl_season(type = "invalid"), 'type must be one of')
  expect_error(get_afl_week(type = "invalid"), 'type must be one of')
})

test_that("harmonic_mean function works correctly", {
  # Test basic functionality
  result <- harmonic_mean(c(2, 4), c(6, 12))
  expected <- c(3, 6)  # Harmonic mean of (2,6) = 3, (4,12) = 6
  expect_equal(result, expected, tolerance = 1e-10)
  
  # Test with zeros (should return NA)
  result_with_zero <- harmonic_mean(c(0, 4), c(6, 12))
  expect_true(is.na(result_with_zero[1]))
  expect_equal(result_with_zero[2], 6, tolerance = 1e-10)
  
  # Test with negative values
  result_negative <- harmonic_mean(c(-2, 4), c(6, 12))
  expect_true(is.na(result_negative[1]))  # Harmonic mean undefined for negative values
  
  # Test edge cases
  expect_true(all(is.na(harmonic_mean(c(0, 0), c(1, 1)))))
  expect_equal(harmonic_mean(1, 1), 1)
  expect_equal(harmonic_mean(c(1, 2), c(1, 2)), c(1, 2))
  
  # Test input validation
  expect_error(harmonic_mean(c(1, 2), c(1)), "same length")
  expect_error(harmonic_mean("a", 1), "must be numeric")
  expect_error(harmonic_mean(1, "a"), "must be numeric")
})

test_that("data loading utility functions work", {
  # Test that save_to_release and file_reader functions exist
  expect_true(exists("save_to_release"))
  expect_true(exists("file_reader"))
  
  # These functions interact with external services, so we just test they exist
  # Full testing would require mocking piggyback functionality
})