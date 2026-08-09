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

test_that("save_to_release ABORTS when a SMALLER listing is stamped AFTER our upload (truncation)", {
  # The row is stamped at/after our own write, so it IS our write and it is
  # short. That is the truncation signature and must stay fatal.
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
      # Far-future stamp -> unambiguously at or after this run's upload.
      list(assets = list(list(name = "widget.parquet", size = uploaded_bytes - 4,
                              updated_at = "2099-01-01T00:00:00Z", id = 1)))
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

test_that("save_to_release warns when a SMALLER listing is stamped BEFORE our upload (lagging read)", {
  # torpdata#74, FOURTH iteration (2026-08-09). The third kept every
  # smaller-than-local listing fatal on the reasoning that truncation makes a
  # file short. But the season files GROW on each round, so a listing that has
  # not caught up serves the previous, smaller asset. On 2026-08-08 that failed
  # 5 of 8 daily releases: listed 73694060 < local 74776757 while adding round
  # 22, and the live asset afterwards read 75153518 -- every write had landed.
  # Size direction carries no information here; only the timestamp does.
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
      # Smaller AND stamped well in the past -> the previous asset. (Scaled
      # down from the real 1,082,697-byte gap; the fixture frame is tiny and a
      # negative size would be testing an impossible listing.)
      list(assets = list(list(name = "widget.parquet", size = uploaded_bytes - 4,
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
  # Match the message too, not just the class: `vb_error_integrity` is shared
  # with the SMALLER/truncation branch and with the unparseable-timestamp
  # branch, so class alone would not prove we reached THIS one.
  expect_error(
    save_to_release(df, "widget", "test-tag"),
    regexp = "at or after our upload",
    class = "vb_error_integrity"
  )
})

test_that("save_to_release aborts, citing the parse failure, when updated_at is unparseable", {
  # Fail closed on an untrustworthy staleness signal -- but the message must
  # name the real cause rather than asserting a temporal relationship that was
  # never evaluated.
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
      list(assets = list(list(name = "widget.parquet", size = uploaded_bytes + 249,
                              updated_at = "not-a-timestamp", id = 1)))
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
    regexp = "could not be parsed",
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

# ---- .vb_asset_true_size(), the transport-side backstop ---------------------
# Added 2026-08-09. The 4th iteration of torpdata#74 made the stale-listing path
# warn-and-proceed, which knowingly left one hole: a genuinely short upload whose
# listing ALSO lags is indistinguishable from a lagging read of a good one. These
# cover the three ways that now resolves. The stale-listing mock returns a
# LARGER, older-stamped row so it reaches this path (either direction does).

.mock_stale_listing <- function() {
  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) {
      list(assets = list(list(name = "widget.parquet", size = 1e9,
                              updated_at = "2020-01-01T00:00:00Z", id = 1)))
    },
    .package = "gh",
    .env = parent.frame()
  )
}

test_that("a lagging listing is CONFIRMED CLEAN when the download path matches what we wrote", {
  uploaded_bytes <- NULL
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) {
      uploaded_bytes <<- file.size(file)
      invisible(NULL)
    },
    .package = "piggyback"
  )
  .mock_stale_listing()
  testthat::local_mocked_bindings(
    .publish_bus_manifest = function(...) invisible(NULL),
    save_locally = function(...) invisible(NULL),
    .vb_asset_true_size = function(repo, tag, file_name) uploaded_bytes
  )

  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  # NOT expect_no_warning: .vb_retry warns once per attempt, so retry chatter is
  # expected on any path that reaches here. What must be absent is the
  # "unconfirmed" verdict — the object is provably correct, so the caller should
  # not be told to go and check it by hand.
  warns <- character()
  msgs <- character()
  withCallingHandlers(
    save_to_release(df, "widget", "test-tag"),
    warning = function(cnd) {
      warns <<- c(warns, conditionMessage(cnd)); invokeRestart("muffleWarning")
    },
    message = function(cnd) {
      msgs <<- c(msgs, conditionMessage(cnd)); invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("proof of correctness|usable answer", warns)))
  expect_true(any(grepl("confirmed at", msgs)))
  # Sanity: assert we really did take the stale-listing path, so this cannot
  # pass by silently never reaching it.
  expect_true(any(grepl("Attempt", warns)))
})

test_that("a lagging listing ABORTS when the download path shows a genuinely short asset", {
  uploaded_bytes <- NULL
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) {
      uploaded_bytes <<- file.size(file)
      invisible(NULL)
    },
    .package = "piggyback"
  )
  .mock_stale_listing()
  testthat::local_mocked_bindings(
    .publish_bus_manifest = function(...) invisible(NULL),
    save_locally = function(...) invisible(NULL),
    .vb_asset_true_size = function(repo, tag, file_name) uploaded_bytes - 4
  )

  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  expect_error(
    suppressWarnings(save_to_release(df, "widget", "test-tag")),
    regexp = "genuinely short asset",
    class = "vb_error_integrity"
  )
})

test_that("an UNAVAILABLE authoritative size falls back to the old warn-and-proceed, inventing nothing", {
  # The contract that matters most: .vb_asset_true_size() returns NA on every
  # failure path, and NA must never be read as either a pass or a truncation.
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) invisible(NULL),
    .package = "piggyback"
  )
  .mock_stale_listing()
  testthat::local_mocked_bindings(
    .publish_bus_manifest = function(...) invisible(NULL),
    save_locally = function(...) invisible(NULL),
    .vb_asset_true_size = function(repo, tag, file_name) NA_real_
  )

  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  expect_warning(
    save_to_release(df, "widget", "test-tag"),
    "did not return a usable answer"
  )
})

test_that(".vb_asset_true_size returns NA rather than propagating a transport failure", {
  testthat::local_mocked_bindings(
    curl_fetch_memory = function(url, handle) stop("simulated DNS failure"),
    .package = "curl"
  )
  expect_true(is.na(torp:::.vb_asset_true_size("owner/repo", "tag", "f.parquet")))
})

test_that(".vb_asset_true_size reads the total off Content-Range, not the range length", {
  # A 206 reports content-length: 1 for a one-byte range. Reading THAT as the
  # size would make every healthy asset look catastrophically truncated.
  testthat::local_mocked_bindings(
    curl_fetch_memory = function(url, handle) {
      list(status_code = 206L, headers = charToRaw(paste0(
        "HTTP/1.1 206 Partial Content\r\n",
        "content-range: bytes 0-0/75153518\r\n",
        "content-length: 1\r\n\r\n")), content = raw(1))
    },
    .package = "curl"
  )
  expect_equal(torp:::.vb_asset_true_size("owner/repo", "tag", "f.parquet"), 75153518)
})
