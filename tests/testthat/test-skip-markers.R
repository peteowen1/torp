# torp H3 / ECOSYSTEM-FIX-PLAN.md T8: negative-cache .skip markers must never
# be written for current-season (or year-unparseable) URLs -- those files are
# actively upserted by the same pipeline that reads them. Historical markers
# still get written but expire quickly (3h). No network -- purely local
# filesystem behaviour against a temp torpdata/data/ dir.

test_that("mark_download_skippable never creates a marker for a current-season URL", {
  tmp_dir <- withr::local_tempdir()
  withr::local_options(torp.local_data_dir = tmp_dir)

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  url <- sprintf(
    "https://github.com/peteowen1/torpdata/releases/download/chains-data/chains_data_%d_all.parquet",
    current_year
  )

  torp:::mark_download_skippable(url)

  skip_path <- paste0(file.path(tmp_dir, basename(url)), ".skip")
  expect_false(file.exists(skip_path))
  expect_false(torp:::is_download_skippable(url))
})

test_that("mark_download_skippable never creates a marker for a year-unparseable URL", {
  tmp_dir <- withr::local_tempdir()
  withr::local_options(torp.local_data_dir = tmp_dir)

  url <- "https://github.com/peteowen1/torpdata/releases/download/ratings-data/torp_ratings.parquet"

  torp:::mark_download_skippable(url)

  skip_path <- paste0(file.path(tmp_dir, basename(url)), ".skip")
  expect_false(file.exists(skip_path))
})

test_that("mark_download_skippable creates a marker for a historical URL, expiring within 3h", {
  tmp_dir <- withr::local_tempdir()
  withr::local_options(torp.local_data_dir = tmp_dir)

  url <- "https://github.com/peteowen1/torpdata/releases/download/chains-data/chains_data_2021_all.parquet"

  torp:::mark_download_skippable(url)
  skip_path <- paste0(file.path(tmp_dir, basename(url)), ".skip")
  expect_true(file.exists(skip_path))
  expect_true(torp:::is_download_skippable(url))

  # Backdate past 3h -- must no longer be considered skippable
  Sys.setFileTime(skip_path, Sys.time() - as.difftime(4, units = "hours"))
  expect_false(torp:::is_download_skippable(url))
})

test_that("clear_skip_markers empties the local data dir of .skip files", {
  tmp_dir <- withr::local_tempdir()
  withr::local_options(torp.local_data_dir = tmp_dir)

  writeLines("skip", file.path(tmp_dir, "foo.parquet.skip"))
  writeLines("skip", file.path(tmp_dir, "bar.parquet.skip"))
  expect_equal(length(list.files(tmp_dir, pattern = "\\.skip$")), 2)

  n <- clear_skip_markers()
  expect_equal(n, 2)
  expect_equal(length(list.files(tmp_dir, pattern = "\\.skip$")), 0)
})
