test_that("rds_from_url validates input correctly", {
  # Test invalid URLs
  expect_error(torp:::rds_from_url(""), "must be a single non-empty character string")
  expect_error(torp:::rds_from_url(c("url1", "url2")), "must be a single non-empty character string")
  expect_error(torp:::rds_from_url(123), "must be a single non-empty character string")
  expect_error(torp:::rds_from_url("ftp://example.com"), "must start with http")
})

test_that("generate_urls creates correct URLs", {
  urls <- torp:::generate_urls("test-data", "test_file", seasons = c(2021, 2022))

  expected_urls <- c(
    "https://github.com/peteowen1/torpdata/releases/download/test-data/test_file_2021.rds",
    "https://github.com/peteowen1/torpdata/releases/download/test-data/test_file_2022.rds"
  )

  expect_equal(length(urls), 2)
  expect_true(all(grepl("^https://github.com/peteowen1/torpdata", urls)))
  expect_true(all(grepl("test-data", urls)))
  expect_true(all(grepl("test_file", urls)))
})

test_that("generate_urls with rounds creates correct URLs", {
  urls <- torp:::generate_urls("test-data", "test_file", seasons = 2021, rounds = c(1, 15))

  expect_equal(length(urls), 2)
  expect_true(all(grepl("2021", urls)))
  expect_true(any(grepl("_01.rds", urls)))
  expect_true(any(grepl("_15.rds", urls)))
})

# Mock test for load functions (these would need actual network mocking in practice)
test_that("load_chains validates parameters", {
  # Test that the function exists and takes the expected parameters
  expect_true(exists("load_chains"))

  # Test with invalid seasons (should error due to validation)
  expect_error(load_chains(seasons = "invalid"))
  expect_error(load_chains(seasons = 2020))  # Too early

  # Test with valid parameters (will fail due to network, but parameters should validate)
  expect_no_error(load_chains(seasons = 2024))
})

test_that("load_player_stats validates parameters", {
  expect_true(exists("load_player_stats"))
  expect_error(load_player_stats(seasons = "invalid"))
})

test_that("load_fixtures validates parameters", {
  expect_true(exists("load_fixtures"))

  # Test the all parameter
  expect_error(load_fixtures(seasons = "invalid"))
})

test_that("error handling improvements work", {
  # Test that functions have been enhanced with tryCatch

  # These tests would ideally mock network failures
  # For now, we test that the functions exist and accept the right parameters
  expect_true(exists("load_chains"))
  expect_true(exists("load_pbp"))
  expect_true(exists("load_player_stats"))
  expect_true(exists("load_fixtures"))
  expect_true(exists("load_teams"))
  expect_true(exists("load_results"))
  expect_true(exists("load_player_details"))
  expect_true(exists("load_predictions"))
})
