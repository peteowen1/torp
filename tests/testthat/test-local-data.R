# Tests for R/local_data.R

# -- set_local_data_dir / get_local_data_dir --

test_that("set_local_data_dir creates directory and sets option", {
  tmp <- file.path(tempdir(), "torp_test_local_data")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_options(torp.local_data_dir = NULL)

  result <- set_local_data_dir(tmp)
  expect_true(dir.exists(tmp))
  expect_equal(
    getOption("torp.local_data_dir"),
    normalizePath(tmp, winslash = "/")
  )
})

test_that("set_local_data_dir errors when directory creation fails", {
  skip_on_os("windows")
  withr::local_options(torp.local_data_dir = NULL)
  expect_error(
    set_local_data_dir("/nonexistent_root_xyz/impossible/path"),
    "Failed to create"
  )
})

test_that("get_local_data_dir returns NULL when nothing configured", {
  withr::local_options(torp.local_data_dir = NULL)
  withr::local_dir(tempdir())
  expect_null(get_local_data_dir())
})

test_that("get_local_data_dir returns option value when set to valid dir", {
  tmp <- file.path(tempdir(), "torp_local_opt_test")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_options(torp.local_data_dir = tmp)
  expect_equal(get_local_data_dir(), normalizePath(tmp, winslash = "/"))
})

test_that("get_local_data_dir ignores non-existent option path", {
  withr::local_options(torp.local_data_dir = "/nonexistent/path/xyz123")
  withr::local_dir(tempdir())
  expect_null(get_local_data_dir())
})

# -- get_local_path --

test_that("get_local_path returns NULL when no local dir configured", {
  withr::local_options(torp.local_data_dir = NULL)
  withr::local_dir(tempdir())
  result <- torp:::get_local_path(
    "https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_2024_01.parquet"
  )
  expect_null(result)
})

test_that("get_local_path extracts basename from URL", {
  tmp <- file.path(tempdir(), "torp_local_path_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_options(torp.local_data_dir = tmp)

  result <- torp:::get_local_path(
    "https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_2024_01.parquet"
  )
  expect_equal(basename(result), "pbp_2024_01.parquet")
})

# -- is_locally_stored --

test_that("is_locally_stored returns FALSE when file does not exist", {
  tmp <- file.path(tempdir(), "torp_stored_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_options(torp.local_data_dir = tmp)
  expect_false(torp:::is_locally_stored("https://example.com/nonexistent.parquet"))
})

test_that("is_locally_stored returns TRUE when file exists", {
  tmp <- file.path(tempdir(), "torp_stored_test2")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("placeholder", file.path(tmp, "exists.parquet"))
  withr::local_options(torp.local_data_dir = tmp)
  expect_true(torp:::is_locally_stored("https://example.com/exists.parquet"))
})

test_that("is_locally_stored respects max_age_days", {
  tmp <- file.path(tempdir(), "torp_stored_age_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  f <- file.path(tmp, "aged.parquet")
  writeLines("placeholder", f)
  # Backdate the file modification time by 10 days
  Sys.setFileTime(f, Sys.time() - as.difftime(10, units = "days"))
  withr::local_options(torp.local_data_dir = tmp)

  # Fresh enough with 30-day window
  expect_true(torp:::is_locally_stored("https://example.com/aged.parquet", max_age_days = 30))
  # Stale with 7-day window
  expect_false(torp:::is_locally_stored("https://example.com/aged.parquet", max_age_days = 7))
  # No staleness check (NULL) — always fresh
  expect_true(torp:::is_locally_stored("https://example.com/aged.parquet", max_age_days = NULL))
})

# -- save_locally round-trip --

test_that("save_locally writes file and read_local_parquet reads it back", {
  tmp <- file.path(tempdir(), "torp_roundtrip_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_options(torp.local_data_dir = tmp)

  df <- data.frame(x = 1:5, y = letters[1:5])
  result <- save_locally(df, "test_roundtrip")
  expect_true(result)
  expect_true(file.exists(file.path(tmp, "test_roundtrip.parquet")))

  read_back <- torp:::read_local_parquet("https://example.com/test_roundtrip.parquet")
  expect_equal(nrow(read_back), 5)
  expect_equal(read_back$x, 1:5)
})

test_that("save_locally returns FALSE with warning when no local dir", {
  withr::local_options(torp.local_data_dir = NULL)
  withr::local_dir(tempdir())
  df <- data.frame(x = 1)
  expect_warning(result <- save_locally(df, "no_dir_test"), "not found")
  expect_false(result)
})

# -- read_local_parquet error handling --

test_that("read_local_parquet returns NULL for corrupted file", {
  tmp <- file.path(tempdir(), "torp_corrupt_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("not a parquet file", file.path(tmp, "corrupt.parquet"))
  withr::local_options(torp.local_data_dir = tmp)
  expect_warning(
    result <- torp:::read_local_parquet("https://example.com/corrupt.parquet"),
    "Failed to read"
  )
  expect_null(result)
})

test_that("read_local_parquet returns NULL when file does not exist", {
  tmp <- file.path(tempdir(), "torp_missing_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  withr::local_options(torp.local_data_dir = tmp)
  result <- torp:::read_local_parquet("https://example.com/missing.parquet")
  expect_null(result)
})

# -- write_local_parquet --

test_that("write_local_parquet silently returns NULL when no local dir", {
  withr::local_options(torp.local_data_dir = NULL)
  withr::local_dir(tempdir())
  result <- torp:::write_local_parquet("https://example.com/test.parquet", data.frame(x = 1))
  expect_null(result)
})
