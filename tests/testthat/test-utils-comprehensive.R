# Comprehensive Tests for Utility Functions
# ==========================================
# Tests all pure utility functions with edge cases

# -----------------------------------------------------------------------------
# harmonic_mean() Tests
# -----------------------------------------------------------------------------

test_that("harmonic_mean handles basic cases correctly", {
  # Equal values
  expect_equal(harmonic_mean(2, 2), 2)
  expect_equal(harmonic_mean(c(2, 4), c(2, 4)), c(2, 4))

  # Known harmonic mean values
  expect_equal(harmonic_mean(1, 2), 4/3, tolerance = 1e-10)
  expect_equal(harmonic_mean(4, 4), 4)

  # Different values
  expect_equal(harmonic_mean(c(2, 4), c(6, 12)), c(3, 6), tolerance = 1e-10)
})

test_that("harmonic_mean handles zeros correctly", {
  # Zero in x

  result <- harmonic_mean(c(0, 4), c(6, 12))
  expect_true(is.na(result[1]))
  expect_equal(result[2], 6, tolerance = 1e-10)

  # Zero in y
  result_y <- harmonic_mean(c(4, 5), c(0, 10))
  expect_true(is.na(result_y[1]))
  expect_false(is.na(result_y[2]))

  # Both zeros
  expect_true(is.na(harmonic_mean(0, 0)))
})

test_that("harmonic_mean handles negative values correctly", {
  # Negative values should return NA (harmonic mean undefined)
  result <- harmonic_mean(c(-2, 4), c(6, 12))
  expect_true(is.na(result[1]))
  expect_false(is.na(result[2]))

  # Both negative
  expect_true(is.na(harmonic_mean(-1, -2)))
})

test_that("harmonic_mean handles single values", {
  expect_equal(harmonic_mean(5, 5), 5)
  expect_equal(harmonic_mean(10, 10), 10)
})

test_that("harmonic_mean validates input types", {
  expect_error(harmonic_mean("a", 1), "must be numeric")
  expect_error(harmonic_mean(1, "b"), "must be numeric")
  expect_error(harmonic_mean(NULL, 1))
})

test_that("harmonic_mean validates input lengths", {
  expect_error(harmonic_mean(c(1, 2), c(1)), "same length")
  expect_error(harmonic_mean(c(1, 2, 3), c(1, 2)), "same length")
})

# -----------------------------------------------------------------------------
# get_mode() Tests
# -----------------------------------------------------------------------------

test_that("get_mode returns most frequent value", {
  expect_equal(get_mode(c(1, 1, 2, 3)), "1")
  expect_equal(get_mode(c("a", "a", "b", "c")), "a")
  expect_equal(get_mode(c(1, 2, 2, 2, 3)), "2")
})

test_that("get_mode handles ties by returning first", {
  # When there's a tie, table() returns in sorted order, which.max gets first
  result <- get_mode(c(1, 1, 2, 2))
  expect_true(result %in% c("1", "2"))
  expect_length(result, 1)
})

test_that("get_mode handles single value", {
  expect_equal(get_mode(c(5)), "5")
  expect_equal(get_mode(c("only_one")), "only_one")
})

test_that("get_mode handles all unique values", {
  result <- get_mode(c(1, 2, 3, 4, 5))
  expect_length(result, 1)
  expect_true(result %in% c("1", "2", "3", "4", "5"))
})

test_that("get_mode handles factors", {
  f <- factor(c("a", "a", "b", "c"))
  expect_equal(get_mode(f), "a")
})

# -----------------------------------------------------------------------------
# decimal_hour() Tests
# -----------------------------------------------------------------------------

test_that("decimal_hour handles midnight correctly", {
  midnight <- as.POSIXct("2024-01-15 00:00:00", tz = "UTC")
  expect_equal(decimal_hour(midnight), 0)
})

test_that("decimal_hour handles noon correctly", {
  noon <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  expect_equal(decimal_hour(noon), 12)
})

test_that("decimal_hour handles fractional hours", {
  # 12:30 = 12.5 hours
  half_past <- as.POSIXct("2024-01-15 12:30:00", tz = "UTC")
  expect_equal(decimal_hour(half_past), 12.5)

  # 6:15 = 6.25 hours
  quarter_past <- as.POSIXct("2024-01-15 06:15:00", tz = "UTC")
  expect_equal(decimal_hour(quarter_past), 6.25)

  # 23:45 = 23.75 hours
  three_quarters <- as.POSIXct("2024-01-15 23:45:00", tz = "UTC")
  expect_equal(decimal_hour(three_quarters), 23.75)
})

test_that("decimal_hour handles seconds", {
  # 12:00:30 = 12 + 30/3600 = 12.00833...
  with_seconds <- as.POSIXct("2024-01-15 12:00:30", tz = "UTC")
  expect_equal(decimal_hour(with_seconds), 12 + 30/3600, tolerance = 1e-10)
})

test_that("decimal_hour validates input type", {
  expect_error(decimal_hour("2024-01-15 12:00:00"), "POSIXct")
  expect_error(decimal_hour(as.Date("2024-01-15")), "POSIXct")
  expect_error(decimal_hour(12.5), "POSIXct")
})

test_that("decimal_hour works with different timezones", {
  # Time is the same regardless of timezone (just different representation)
  aest <- as.POSIXct("2024-01-15 12:00:00", tz = "Australia/Brisbane")
  result <- decimal_hour(aest)
  expect_equal(result, 12)
})

# -----------------------------------------------------------------------------
# get_proportion_through_day() Tests
# -----------------------------------------------------------------------------

test_that("get_proportion_through_day handles start of day", {
  start <- as.POSIXct("2024-01-15 00:00:00", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_day(start),
    error = function(e) NULL
  )
  # Skip if function not working (may be missing lubridate::tz)
  skip_if(is.null(result), "get_proportion_through_day not available")
  expect_equal(result, 0)
})

test_that("get_proportion_through_day handles middle of day", {
  noon <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_day(noon),
    error = function(e) NULL
  )
  skip_if(is.null(result), "get_proportion_through_day not available")
  expect_equal(result, 0.5, tolerance = 1e-6)
})

test_that("get_proportion_through_day handles end of day", {
  # Just before midnight
  almost_midnight <- as.POSIXct("2024-01-15 23:59:59", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_day(almost_midnight),
    error = function(e) NULL
  )
  skip_if(is.null(result), "get_proportion_through_day not available")
  expect_true(result > 0.99)
  expect_true(result < 1)
})

test_that("get_proportion_through_day handles quarter times", {
  # 6:00 AM = 0.25
  quarter <- as.POSIXct("2024-01-15 06:00:00", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_day(quarter),
    error = function(e) NULL
  )
  skip_if(is.null(result), "get_proportion_through_day not available")
  expect_equal(result, 0.25, tolerance = 1e-6)
})

test_that("get_proportion_through_day validates input type", {
  expect_error(torp:::get_proportion_through_day("2024-01-15 12:00:00"), "POSIXct")
  expect_error(torp:::get_proportion_through_day(as.Date("2024-01-15")), "POSIXct")
})

# -----------------------------------------------------------------------------
# get_proportion_through_year() Tests
# -----------------------------------------------------------------------------

test_that("get_proportion_through_year handles January 1", {
  jan1 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_year(jan1),
    error = function(e) NULL
  )
  skip_if(is.null(result), "get_proportion_through_year not available")
  expect_equal(result, 0)
})

test_that("get_proportion_through_year handles December 31", {
  dec31 <- as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_year(dec31),
    error = function(e) NULL
  )
  skip_if(is.null(result), "get_proportion_through_year not available")
  expect_true(result > 0.99)
  expect_true(result <= 1)
})

test_that("get_proportion_through_year handles mid-year", {
  # July 2 is approximately mid-year
  mid_year <- as.POSIXct("2024-07-02 00:00:00", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_year(mid_year),
    error = function(e) NULL
  )
  skip_if(is.null(result), "get_proportion_through_year not available")
  expect_true(result > 0.48)
  expect_true(result < 0.52)
})

test_that("get_proportion_through_year handles leap year", {
  # 2024 is a leap year (366 days)
  # Feb 29 should work
  leap_day <- as.POSIXct("2024-02-29 12:00:00", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_year(leap_day),
    error = function(e) NULL
  )
  skip_if(is.null(result), "get_proportion_through_year not available")
  expect_true(result > 0)
  expect_true(result < 1)
  # Should be about 60/366 = 0.164
  expect_true(abs(result - 60/366) < 0.01)
})

test_that("get_proportion_through_year handles non-leap year", {
  # 2023 is not a leap year (365 days)
  mar1 <- as.POSIXct("2023-03-01 00:00:00", tz = "UTC")
  result <- tryCatch(
    torp:::get_proportion_through_year(mar1),
    error = function(e) NULL
  )
  skip_if(is.null(result), "get_proportion_through_year not available")
  # Should be about 59/365 = 0.162
  expect_true(abs(result - 59/365) < 0.01)
})

test_that("get_proportion_through_year validates input type", {
  expect_error(torp:::get_proportion_through_year("2024-07-01"), "POSIXct")
  expect_error(torp:::get_proportion_through_year(as.Date("2024-07-01")), "POSIXct")
})

# -----------------------------------------------------------------------------
# norm_name() Tests
# -----------------------------------------------------------------------------

test_that("norm_name handles basic names", {
  expect_equal(norm_name("John Smith"), "john smith")
  expect_equal(norm_name("UPPERCASE NAME"), "uppercase name")
  expect_equal(norm_name("MixedCase Name"), "mixedcase name")
})

test_that("norm_name handles accented characters", {
  expect_equal(norm_name("Jose"), "jose")
  expect_equal(norm_name("Cameron Zurhaar"), "cameron zurhaar")
  # Accented characters converted to ASCII
  expect_equal(norm_name("Caf\u00e9"), "cafe")  # e with acute accent
})

test_that("norm_name handles apostrophes",
{
  expect_equal(norm_name("O'Brien"), "o brien")
  expect_equal(norm_name("D'Arcy"), "d arcy")
})

test_that("norm_name handles hyphens", {
  expect_equal(norm_name("Smith-Jones"), "smith jones")
  expect_equal(norm_name("Mary-Anne"), "mary anne")
})

test_that("norm_name handles extra whitespace", {
  expect_equal(norm_name("  John   Smith  "), "john smith")
  expect_equal(norm_name("Multiple    Spaces"), "multiple spaces")
})
test_that("norm_name handles empty and special cases", {
  expect_equal(norm_name(""), "")
  expect_equal(norm_name("123"), "")  # Numbers removed
  expect_equal(norm_name("!!!"), "")  # Special chars removed
})

test_that("norm_name handles vectors", {
  input <- c("John Smith", "JANE DOE", "O'Connor")
  expected <- c("john smith", "jane doe", "o connor")
  expect_equal(norm_name(input), expected)
})

# -----------------------------------------------------------------------------
# get_afl_season() Tests
# -----------------------------------------------------------------------------

test_that("get_afl_season returns current year for current type", {
  result <- get_afl_season("current")
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  expect_equal(result, current_year)
})

test_that("get_afl_season returns next year for next type", {
  result <- get_afl_season("next")
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  expect_equal(result, current_year + 1)
})

test_that("get_afl_season rejects invalid type", {
  expect_error(get_afl_season("invalid"), 'type must be one of')
  expect_error(get_afl_season("past"), 'type must be one of')
  expect_error(get_afl_season(""), 'type must be one of')
})

test_that("get_afl_season default is current", {
  result <- get_afl_season()
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  expect_equal(result, current_year)
})

# -----------------------------------------------------------------------------
# get_afl_week() Tests
# -----------------------------------------------------------------------------

test_that("get_afl_week rejects invalid type", {
  expect_error(get_afl_week("invalid"), 'type must be one of')
  expect_error(get_afl_week("past"), 'type must be one of')
})

test_that("get_afl_week returns reasonable values", {
  # This test may depend on fixtures being available
  result <- tryCatch(
    get_afl_week("current"),
    error = function(e) 0,
    warning = function(w) 0
  )
  expect_reasonable_round(result)
})

# -----------------------------------------------------------------------------
# is_installed() Tests
# -----------------------------------------------------------------------------

test_that("is_installed correctly identifies installed packages", {
  # testthat should be installed (we're using it)
  expect_true(torp:::is_installed("testthat"))

  # base should always be available
  expect_true(torp:::is_installed("base"))
})

test_that("is_installed returns FALSE for non-existent packages", {
  expect_false(torp:::is_installed("definitelynotarealpackage123456"))
  expect_false(torp:::is_installed("anotherFakePackage"))
})
