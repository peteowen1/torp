# save_to_release()'s post-upload verify (T12) regression coverage.
# See torpdata#74: GitHub's release-asset listing can lag the upload by a
# few seconds (eventual consistency), which the verify used to treat as a
# hard integrity failure on the very first check.

test_that("save_to_release retries the post-upload size verify through a stale GitHub listing, then succeeds", {
  uploaded_bytes <- NULL
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) {
      uploaded_bytes <<- file.size(file)
      invisible(NULL)
    },
    .package = "piggyback"
  )

  call_count <- 0
  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) {
      call_count <<- call_count + 1
      # First listing call reports a stale (wrong) size; second call
      # reports the real size once GitHub's listing has caught up.
      reported_size <- if (call_count == 1) uploaded_bytes + 4 else uploaded_bytes
      list(assets = list(list(name = "widget.parquet", size = reported_size,
                              updated_at = "2026-07-22T00:00:00Z", id = 1)))
    },
    .package = "gh"
  )
  testthat::local_mocked_bindings(
    .publish_bus_manifest = function(...) invisible(NULL),
    save_locally = function(...) invisible(NULL)
  )

  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  # .vb_retry's real backoff (Sys.sleep, base) isn't mockable via
  # local_mocked_bindings -- one retry here means a real ~2s sleep.
  expect_no_error(save_to_release(df, "widget", "test-tag"))
  expect_equal(call_count, 2)
})

test_that("save_to_release aborts when the post-upload size mismatch persists after retries", {
  uploaded_bytes <- NULL
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) {
      uploaded_bytes <<- file.size(file)
      invisible(NULL)
    },
    .package = "piggyback"
  )

  call_count <- 0
  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) {
      call_count <<- call_count + 1
      # Always reports the wrong size -- a genuine, non-transient mismatch.
      list(assets = list(list(name = "widget.parquet", size = uploaded_bytes + 4,
                              updated_at = "2026-07-22T00:00:00Z", id = 1)))
    },
    .package = "gh"
  )
  testthat::local_mocked_bindings(
    .publish_bus_manifest = function(...) invisible(NULL),
    save_locally = function(...) invisible(NULL)
  )

  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  expect_error(
    save_to_release(df, "widget", "test-tag"),
    class = "vb_error_integrity"
  )
  expect_equal(call_count, 3L)  # exhausted all .vb_retry attempts
})

test_that("save_to_release warns (not aborts) when the post-upload listing call itself keeps failing", {
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) invisible(NULL),
    .package = "piggyback"
  )
  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) stop("simulated network failure"),
    .package = "gh"
  )
  testthat::local_mocked_bindings(
    .publish_bus_manifest = function(...) invisible(NULL),
    save_locally = function(...) invisible(NULL)
  )

  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  expect_warning(
    save_to_release(df, "widget", "test-tag"),
    "Post-upload verify could not list"
  )
})
