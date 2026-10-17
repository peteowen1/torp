test_that("write_disk_cache writes atomically and leaves no temp files", {
  skip_if_not_installed("withr")
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home)

  url <- "https://example.com/atomic_test.parquet"
  write_disk_cache(url, data.frame(a = 1:3))
  write_disk_cache(url, data.frame(a = 4:6))  # overwrite an existing file

  expect_equal(read_disk_cache(url)$a, 4:6)
  expect_length(list_disk_cache_files(), 1)
  expect_length(list.files(get_disk_cache_dir(), pattern = "\\.tmp$"), 0)
  expect_equal(nrow(get_disk_cache_info()), 1)
  expect_equal(clear_disk_cache(), 1L)
})
