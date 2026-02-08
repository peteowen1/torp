test_that("generate_disk_cache_key returns consistent keys", {
  key1 <- generate_disk_cache_key("https://example.com/data/file.parquet")
  key2 <- generate_disk_cache_key("https://example.com/data/file.parquet")
  expect_equal(key1, key2)
})

test_that("generate_disk_cache_key returns different keys for different URLs", {
  key1 <- generate_disk_cache_key("https://example.com/data/file1.parquet")
  key2 <- generate_disk_cache_key("https://example.com/data/file2.parquet")
  expect_false(key1 == key2)
})

test_that("generate_disk_cache_key includes file prefix", {
  key <- generate_disk_cache_key("https://example.com/data/chains_2024.parquet")
  expect_true(grepl("^chains_2024_", key))
})

test_that("get_disk_cache_dir creates directory", {
  cache_dir <- get_disk_cache_dir()
  expect_true(dir.exists(cache_dir))
  expect_true(grepl("\\.torp[/\\\\]cache$", cache_dir))
})

test_that("get_disk_cache_path returns .parquet path", {
  path <- get_disk_cache_path("https://example.com/data/test.parquet")
  expect_true(grepl("\\.parquet$", path))
})

test_that("is_disk_cached returns FALSE for non-existent URL", {
  expect_false(is_disk_cached("https://example.com/data/nonexistent_test.parquet"))
})

test_that("set_disk_cache_options stores and returns settings", {
  result <- set_disk_cache_options(enabled = FALSE, max_age_days = 30)
  expect_equal(result$enabled, FALSE)
  expect_equal(result$max_age_days, 30)

  # Verify get_disk_cache_options reads them back
  opts <- get_disk_cache_options()
  expect_equal(opts$enabled, FALSE)
  expect_equal(opts$max_age_days, 30)

  # Reset
  set_disk_cache_options(enabled = TRUE, max_age_days = 7)
})

test_that("get_disk_cache_options returns defaults", {
  # Clear any stored values
  .torp_disk_cache_env <- get(".torp_disk_cache_env", envir = asNamespace("torp"))
  rm(list = ls(.torp_disk_cache_env), envir = .torp_disk_cache_env)

  opts <- get_disk_cache_options()
  expect_true(opts$enabled)
  expect_equal(opts$max_age_days, 7)
})

test_that("get_disk_cache_info returns data frame with expected columns", {
  info <- get_disk_cache_info()
  expect_s3_class(info, "data.frame")
  expect_true("file" %in% names(info))
  expect_true("size_mb" %in% names(info))
  expect_true("age_days" %in% names(info))
})

test_that("get_disk_cache_size returns numeric", {
  size <- get_disk_cache_size()
  expect_true(is.numeric(size))
  expect_true(size >= 0)
})

test_that("clear_disk_cache returns integer count", {
  result <- clear_disk_cache()
  expect_true(is.integer(result) || is.numeric(result))
  expect_true(result >= 0)
})
