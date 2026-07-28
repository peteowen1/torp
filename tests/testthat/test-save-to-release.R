# save_to_release()'s post-upload verify (T12) regression coverage.
# See torpdata#74: GitHub's release-asset listing can lag the upload by a
# few seconds (eventual consistency), which the verify used to treat as a
# hard integrity failure on the very first check. Retry budget widened
# 2026-07-25 (3 attempts/~7s -> 5 attempts/~20s, c(2,3,5,10) delays) after
# the original budget still wasn't enough during live-game upload bursts.

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

test_that("save_to_release aborts when the listing stays SMALLER than local (possible truncation)", {
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
      # Persistently SMALLER than what we wrote -- the truncation signature,
      # which must stay fatal (torpdata#74 third iteration).
      list(assets = list(list(name = "widget.parquet", size = uploaded_bytes - 4,
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
  expect_equal(call_count, 5L)  # exhausted all .vb_retry attempts
})

test_that("save_to_release warns when a LARGER listing is stamped BEFORE our upload (lagging read)", {
  # torpdata#74, third iteration: a larger listing whose updated_at predates our
  # own write is a lagging read. Treating that as fatal aborted 33 daily
  # releases between 2026-07-14 and 2026-07-27, staling torp's downstream match
  # predictions for two weeks.
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
      # Stamped well in the past -> predates this run's upload.
      list(assets = list(list(name = "widget.parquet", size = uploaded_bytes + 249,
                              updated_at = "2020-01-01T00:00:00Z", id = 1)))
    },
    .package = "gh"
  )
  testthat::local_mocked_bindings(
    .publish_bus_manifest = function(...) invisible(NULL),
    save_locally = function(...) invisible(NULL)
  )

  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  expect_warning(
    save_to_release(df, "widget", "test-tag"),
    "lagging|older-stamped"
  )
  expect_equal(call_count, 5L)  # still retried the full budget before giving up
})

test_that("save_to_release ABORTS when a LARGER listing is stamped AFTER our upload (lost write)", {
  # The case the previous iteration waved through on a false premise: a larger
  # asset stamped at or after our own upload is not a lagging read. It is a
  # failed replace (piggyback's delete-then-upload is not atomic) or a
  # concurrent writer -- i.e. our data is NOT what is live. Must stay fatal.
  uploaded_bytes <- NULL
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) {
      uploaded_bytes <<- file.size(file)
      invisible(NULL)
    },
    .package = "piggyback"
  )

  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) {
      # Stamped in the future -> unambiguously at/after our upload, beyond any
      # clock-skew tolerance.
      future_stamp <- format(Sys.time() + 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      list(assets = list(list(name = "widget.parquet", size = uploaded_bytes + 249,
                              updated_at = future_stamp, id = 1)))
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
