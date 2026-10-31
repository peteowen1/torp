# .vb_safe_upload() (R/versebus.R, versebus 1.2.0): a release asset is never
# deleted before its replacement is confirmed. Driven against a fake release
# that keeps its own asset list, so the upload / list / delete / rename calls
# all act on the same state.

fake_release <- function(assets = list()) {
  env <- new.env()
  env$assets <- assets
  env$next_id <- 100
  env$upload_state <- "uploaded"
  env$fail_upload <- FALSE
  env
}

local_fake_release <- function(rel, env = parent.frame()) {
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, name = basename(file), overwrite = "use_timestamps", ...) {
      if (rel$fail_upload) stop("simulated network failure")
      rel$next_id <- rel$next_id + 1
      rel$assets[[length(rel$assets) + 1L]] <- list(
        id = rel$next_id, name = name, size = as.numeric(file.size(file)), state = rel$upload_state)
      invisible(NULL)
    },
    .package = "piggyback", .env = env
  )
  testthat::local_mocked_bindings(
    gh = function(endpoint, ..., owner, repo, tag, id, name) {
      if (startsWith(endpoint, "GET ")) return(list(assets = rel$assets))
      ids <- vapply(rel$assets, function(a) a$id, numeric(1))
      i <- which(ids == as.numeric(id))
      if (startsWith(endpoint, "DELETE ")) { rel$assets[[i]] <- NULL; return(invisible(NULL)) }
      if (startsWith(endpoint, "PATCH ")) { rel$assets[[i]]$name <- name; return(invisible(NULL)) }
      stop("unexpected endpoint ", endpoint)
    },
    .package = "gh", .env = env
  )
}

asset_names <- function(rel) vapply(rel$assets, function(a) a$name, character(1))

probe_file <- function(n) {
  f <- withr::local_tempfile(.local_envir = parent.frame(), fileext = ".parquet")
  writeBin(as.raw(rep(1L, n)), f)
  f
}

test_that("a replacement swaps in: one asset under the real name, at the new size, no temp copy left", {
  rel <- fake_release(list(list(id = 1, name = "x.parquet", size = 5, state = "uploaded")))
  local_fake_release(rel)
  f <- probe_file(40)
  .vb_safe_upload(f, "o/r", "t", name = "x.parquet", poll_delays = numeric(0))
  expect_identical(asset_names(rel), "x.parquet")
  expect_equal(rel$assets[[1]]$size, 40)
  expect_false(rel$assets[[1]]$id == 1)
})

test_that("a failed upload leaves the published asset untouched", {
  rel <- fake_release(list(list(id = 1, name = "x.parquet", size = 5, state = "uploaded")))
  rel$fail_upload <- TRUE
  local_fake_release(rel)
  f <- probe_file(40)
  expect_error(.vb_safe_upload(f, "o/r", "t", name = "x.parquet", poll_delays = numeric(0)),
               class = "vb_error_transient")
  expect_identical(asset_names(rel), "x.parquet")
  expect_equal(rel$assets[[1]]$size, 5)
})

test_that("an upload GitHub never confirms (piggyback only warned) deletes nothing", {
  rel <- fake_release(list(list(id = 1, name = "x.parquet", size = 5, state = "uploaded")))
  rel$upload_state <- "starter"  # half-finished upload
  local_fake_release(rel)
  f <- probe_file(40)
  expect_error(.vb_safe_upload(f, "o/r", "t", name = "x.parquet", poll_delays = numeric(0)),
               "never reported as[[:space:]]+uploaded", class = "vb_error_transient")
  expect_true("x.parquet" %in% asset_names(rel))
  expect_equal(rel$assets[[1]]$size, 5)
})

test_that("temp copies stranded by an earlier failed run are cleaned up; other assets are left alone", {
  rel <- fake_release(list(
    list(id = 1, name = "x.parquet", size = 5, state = "uploaded"),
    list(id = 2, name = "vbnew-old-1-9-9--x.parquet", size = 7, state = "uploaded"),
    list(id = 3, name = "y.parquet", size = 9, state = "uploaded")))
  local_fake_release(rel)
  f <- probe_file(40)
  .vb_safe_upload(f, "o/r", "t", name = "x.parquet", poll_delays = numeric(0))
  expect_setequal(asset_names(rel), c("x.parquet", "y.parquet"))
})

test_that("a first publish (no existing asset) just lands under the real name", {
  rel <- fake_release()
  local_fake_release(rel)
  f <- probe_file(12)
  .vb_safe_upload(f, "o/r", "t", name = "bus_manifest.json", poll_delays = numeric(0))
  expect_identical(asset_names(rel), "bus_manifest.json")
})

test_that("an upload GitHub refused (piggyback returned the 401 without raising) fails at once and deletes nothing", {
  rel <- fake_release(list(list(id = 1, name = "x.parquet", size = 5, state = "uploaded")))
  local_fake_release(rel)
  testthat::local_mocked_bindings(
    pb_upload = function(...) list(structure(list(status_code = 401L), class = "response")),
    .package = "piggyback"
  )
  f <- probe_file(40)
  expect_error(.vb_safe_upload(f, "o/r", "t", name = "x.parquet", poll_delays = c(60, 60)),
               "refused", class = "vb_error_transient")
  expect_identical(asset_names(rel), "x.parquet")
})

test_that("vb_publish refuses to upload when no token is visible to piggyback", {
  uploads <- 0L
  testthat::local_mocked_bindings(
    .vb_token = function() "",
    .vb_safe_upload = function(...) { uploads <<- uploads + 1L; invisible(1) }
  )
  f <- probe_file(10)
  expect_error(vb_publish(f, repo = "o/r", tag = "t", carry_forward = FALSE),
               class = "vb_error_auth")
  expect_identical(uploads, 0L)
})
