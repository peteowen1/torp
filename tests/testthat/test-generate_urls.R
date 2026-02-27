test_that("generate_urls creates correct fixture URLs", {
  urls <- torp:::generate_urls("fixtures-data", "fixtures", seasons = c(2021, 2022))
  expect_equal(urls,
               c("https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2021.parquet",
                 "https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2022.parquet"))
})

test_that("generate_urls always uses _all for chains and pbp", {
  urls <- torp:::generate_urls("pbp-data", "pbp_data", seasons = 2021, rounds = 1:2)
  expect_equal(urls,
               "https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_2021_all.parquet")

  urls_chains <- torp:::generate_urls("chains-data", "chains_data", seasons = c(2021, 2022), rounds = 1:5)
  expect_equal(urls_chains,
               c("https://github.com/peteowen1/torpdata/releases/download/chains-data/chains_data_2021_all.parquet",
                 "https://github.com/peteowen1/torpdata/releases/download/chains-data/chains_data_2022_all.parquet"))
})

test_that("generate_urls filters future per-round URLs for current season", {
  local_mocked_bindings(
    get_afl_season = function(...) 2025,
    get_afl_week = function(...) 5,
    get_release_assets = function(...) NULL
  )

  # Past season: all rounds kept
  urls_past <- torp:::generate_urls(
    "player_stats-data", "player_stats", seasons = 2024, rounds = 1:10,
    prefer_aggregated = FALSE
  )
  expect_length(urls_past, 10)

  # Current season: only rounds <= 5 kept
  urls_current <- torp:::generate_urls(
    "player_stats-data", "player_stats", seasons = 2025, rounds = 1:10,
    prefer_aggregated = FALSE
  )
  expect_length(urls_current, 5)

  # Future season: all rounds filtered out
  urls_future <- torp:::generate_urls(
    "player_stats-data", "player_stats", seasons = 2027, rounds = 1:3,
    prefer_aggregated = FALSE
  )
  expect_length(urls_future, 0)
})

test_that("generate_urls keeps fixture URLs regardless of round", {
  local_mocked_bindings(
    get_afl_season = function(...) 2025,
    get_afl_week = function(...) 5,
    get_release_assets = function(...) NULL
  )

  urls <- torp:::generate_urls("fixtures-data", "fixtures", seasons = 2025)
  expect_length(urls, 1)
  expect_true(grepl("fixtures_2025", urls))
})

test_that("generate_urls keeps _all aggregated files for current season", {
  local_mocked_bindings(
    get_afl_season = function(...) 2025,
    get_afl_week = function(...) 5,
    get_release_assets = function(...) NULL
  )

  urls <- torp:::generate_urls("pbp-data", "pbp_data", seasons = 2025, rounds = 0:28)
  expect_length(urls, 1)
  expect_true(grepl("_all\\.parquet$", urls))
})
