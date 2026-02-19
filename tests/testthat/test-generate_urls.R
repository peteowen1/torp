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
