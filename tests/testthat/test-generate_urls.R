test_that("generate_urls creates correct fixture URLs", {
  urls <- torp:::generate_urls("fixtures-data", "fixtures", seasons = c(2021, 2022))
  expect_equal(urls,
               c("https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2021.rds",
                 "https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2022.rds"))
})

test_that("generate_urls handles rounds", {
  urls <- torp:::generate_urls("pbp-data", "pbp_data", seasons = 2021, rounds = 1:2)
  expect_equal(urls,
               c("https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_2021_01.rds",
                 "https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_2021_02.rds"))
})
