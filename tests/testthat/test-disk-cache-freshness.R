# torp H4 / ECOSYSTEM-FIX-PLAN.md T9 (verify-only -- already fixed in the
# current tree at is_disk_cached()'s local_max_age_for_url() season cap):
# a current-season disk-cache entry expires after 1 day even though the
# flat disk-cache TTL default is 7 days; historical entries get the full
# 7-day TTL. No network -- HOME is redirected to a temp dir.

test_that("is_disk_cached applies the current-season 1-day cap", {
  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home))

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  url <- sprintf(
    "https://github.com/peteowen1/torpdata/releases/download/chains-data/chains_data_%d_all.parquet",
    current_year
  )

  cache_path <- torp:::get_disk_cache_path(url)
  writeLines("x", cache_path)

  # Fresh -- within the 1-day current-season cap
  expect_true(torp:::is_disk_cached(url))

  # 2 days old -- past the 1-day cap even though the default 7-day TTL would
  # otherwise still allow it
  Sys.setFileTime(cache_path, Sys.time() - as.difftime(2, units = "days"))
  expect_false(torp:::is_disk_cached(url))
})

test_that("is_disk_cached allows historical entries up to the default 7-day TTL", {
  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home))

  url <- "https://github.com/peteowen1/torpdata/releases/download/chains-data/chains_data_2021_all.parquet"
  cache_path <- torp:::get_disk_cache_path(url)
  writeLines("x", cache_path)

  Sys.setFileTime(cache_path, Sys.time() - as.difftime(5, units = "days"))
  expect_true(torp:::is_disk_cached(url))

  Sys.setFileTime(cache_path, Sys.time() - as.difftime(8, units = "days"))
  expect_false(torp:::is_disk_cached(url))
})
