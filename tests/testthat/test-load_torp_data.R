test_that("parquet_from_url validates input correctly", {
  # Test invalid URLs
  expect_error(torp:::parquet_from_url(""), "must be a single non-empty character string")
  expect_error(torp:::parquet_from_url(c("url1", "url2")), "must be a single non-empty character string")
  expect_error(torp:::parquet_from_url(123), "must be a single non-empty character string")
  expect_error(torp:::parquet_from_url("ftp://example.com"), "must start with http")
})

test_that("generate_urls creates correct URLs", {
  urls <- torp:::generate_urls("test-data", "test_file", seasons = c(2021, 2022))

  expected_urls <- c(
    "https://github.com/peteowen1/torpdata/releases/download/test-data/test_file_2021.parquet",
    "https://github.com/peteowen1/torpdata/releases/download/test-data/test_file_2022.parquet"
  )

  expect_equal(length(urls), 2)
  expect_true(all(grepl("^https://github.com/peteowen1/torpdata", urls)))
  expect_true(all(grepl("test-data", urls)))
  expect_true(all(grepl("test_file", urls)))
  expect_true(all(grepl("\\.parquet$", urls)))
})

test_that("generate_urls with rounds creates correct URLs for non-aggregated types", {
  urls <- torp:::generate_urls("test-data", "test_file", seasons = 2021, rounds = c(1, 15))

  expect_equal(length(urls), 2)
  expect_true(all(grepl("2021", urls)))
  expect_true(any(grepl("_01\\.parquet", urls)))
  expect_true(any(grepl("_15\\.parquet", urls)))
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

# -- New load_* functions existence and signature tests --

test_that("load_ep_wp_charts exists and has correct signature", {
  expect_true(exists("load_ep_wp_charts"))
  expect_true("load_ep_wp_charts" %in% getNamespaceExports("torp"))
  fn_args <- names(formals(load_ep_wp_charts))
  expect_true("seasons" %in% fn_args)
  expect_true("rounds" %in% fn_args)
  expect_true("columns" %in% fn_args)
})

test_that("load_player_game_ratings exists and has correct signature", {
  expect_true(exists("load_player_game_ratings"))
  expect_true("load_player_game_ratings" %in% getNamespaceExports("torp"))
  fn_args <- names(formals(load_player_game_ratings))
  expect_true("seasons" %in% fn_args)
  expect_true("columns" %in% fn_args)
})

test_that("load_player_season_ratings exists and has correct signature", {
  expect_true(exists("load_player_season_ratings"))
  expect_true("load_player_season_ratings" %in% getNamespaceExports("torp"))
  fn_args <- names(formals(load_player_season_ratings))
  expect_true("seasons" %in% fn_args)
  expect_true("columns" %in% fn_args)
})

test_that("load_team_ratings exists and has correct signature", {
  expect_true(exists("load_team_ratings"))
  expect_true("load_team_ratings" %in% getNamespaceExports("torp"))
  fn_args <- names(formals(load_team_ratings))
  expect_true("columns" %in% fn_args)
})

test_that("load_torp_ratings exists and has correct signature", {
  expect_true(exists("load_torp_ratings"))
  expect_true("load_torp_ratings" %in% getNamespaceExports("torp"))
  fn_args <- names(formals(load_torp_ratings))
  expect_true("columns" %in% fn_args)
})

test_that("download_torp_data exists and validates input", {
  expect_true(exists("download_torp_data"))
  expect_true("download_torp_data" %in% getNamespaceExports("torp"))
  fn_args <- names(formals(download_torp_data))
  expect_true("data_types" %in% fn_args)
  expect_true("seasons" %in% fn_args)
  expect_true("overwrite" %in% fn_args)
})

test_that("columns parameter is accepted by load_from_url", {
  # Mock the download to test column selection logic
  local_mocked_bindings(
    parquet_from_url = function(url) {
      data.table::data.table(
        season = 2024,
        round = 1L,
        match_id = "m1",
        team_name = "TeamA",
        score = 100
      )
    }
  )

  # Request specific columns
  result <- load_from_url(
    "https://example.com/test.parquet",
    columns = c("match_id", "score")
  )
  expect_true("match_id" %in% names(result))
  expect_true("score" %in% names(result))
  # Auto-added filter columns should be dropped
  expect_false("season" %in% names(result))
})

test_that("load_from_url uses parallel processing correctly", {
  # Test that single URL case works (sequential via parquet_from_url_cached)
  single_url <- "https://github.com/peteowen1/torpdata/releases/download/test-data/test_file.parquet"

  # Mock parquet_from_url for the single-URL path
  local_mocked_bindings(
    parquet_from_url = function(url) {
      data.table::data.table(test_col = 1:3, url_source = basename(url))
    }
  )

  result_single <- load_from_url(single_url)
  expect_s3_class(result_single, "tbl_df")
  expect_equal(nrow(result_single), 3)

  # Test multiple URLs — parquet_from_urls_parallel uses open_dataset batch reading.
  # Mock the entire parallel function to verify it's called for multi-URL case.
  multiple_urls <- c(
    "https://github.com/peteowen1/torpdata/releases/download/test-data/test_file1.parquet",
    "https://github.com/peteowen1/torpdata/releases/download/test-data/test_file2.parquet"
  )

  local_mocked_bindings(
    parquet_from_urls_parallel = function(urls, ...) {
      data.table::data.table(
        test_col = rep(1:3, length(urls)),
        url_source = rep(basename(urls), each = 3)
      )
    }
  )

  result_multiple <- load_from_url(multiple_urls)
  expect_s3_class(result_multiple, "tbl_df")
  expect_equal(nrow(result_multiple), 6)  # 3 rows from each file
  expect_true(all(c("test_file1.parquet", "test_file2.parquet") %in% result_multiple$url_source))
})
