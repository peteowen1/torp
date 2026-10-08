# safe_release_upload() / confirm_fresh_start() -- R/release_upload.R.
# A stateful fake release stands in for GitHub: gh::gh serves the listing,
# DELETE and PATCH; piggyback::pb_upload adds an asset. Like the real
# piggyback 0.1.5, a failed upload POST only WARNS -- the code under test
# must find out from the listing. Audit: vault/plans/CLOBBER-AUDIT-2026-10-08.md.

.fake_release <- function(...) {
  st <- new.env()
  st$assets <- list()
  st$next_id <- 1
  st$fail <- character()
  st$add <- function(name, size, state = "uploaded") {
    st$assets[[length(st$assets) + 1L]] <- list(
      id = st$next_id, name = name, size = size, state = state,
      updated_at = "2026-10-08T00:00:00Z")
    st$next_id <- st$next_id + 1
  }
  st$names <- function() sort(vapply(st$assets, `[[`, character(1), "name"))
  st$size_of <- function(name) {
    hit <- Filter(function(a) identical(a$name, name), st$assets)
    if (length(hit)) hit[[1]]$size else NA_real_
  }
  seed <- list(...)
  for (nm in names(seed)) st$add(nm, seed[[nm]])
  st
}

.mock_release <- function(st, env = parent.frame()) {
  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) {
      a <- list(...)
      if (startsWith(endpoint, "GET")) {
        if ("list" %in% st$fail) stop("HTTP 500 listing")
        if ("tag404" %in% st$fail) {
          stop(structure(class = c("http_error_404", "error", "condition"),
                         list(message = "Not Found (HTTP 404)", call = NULL)))
        }
        return(list(assets = st$assets))
      }
      ids <- vapply(st$assets, function(x) x$id, numeric(1))
      i <- which(ids == as.numeric(a$id))
      if (startsWith(endpoint, "DELETE")) {
        if ("delete" %in% st$fail) stop("HTTP 500 delete")
        st$assets[[i]] <- NULL
        return(invisible(NULL))
      }
      if (startsWith(endpoint, "PATCH")) {
        if ("patch" %in% st$fail) stop("HTTP 500 patch")
        st$assets[[i]]$name <- a$name
        return(invisible(NULL))
      }
      stop("unexpected gh endpoint: ", endpoint)
    },
    .package = "gh", .env = env
  )
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, name = basename(file), overwrite = FALSE, ...) {
      if ("upload" %in% st$fail) {
        warning("Internal Server Error (HTTP 500)")  # piggyback only warns
        return(invisible(NULL))
      }
      st$add(name, as.numeric(file.size(file)),
             state = if ("stuck" %in% st$fail) "starter" else "uploaded")
      invisible(NULL)
    },
    .package = "piggyback", .env = env
  )
  testthat::local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base", .env = env)
}

.new_file <- function(bytes = 20) {
  f <- tempfile(fileext = ".parquet")
  writeBin(as.raw(rep(1L, bytes)), f)
  f
}

test_that("safe_release_upload replaces the asset in place and leaves no temp copy", {
  st <- .fake_release(a.parquet = 5)
  .mock_release(st)
  f <- .new_file(20)
  set.seed(42)
  seed_before <- .Random.seed
  safe_release_upload(f, "o/r", "t", name = "a.parquet")
  expect_equal(st$names(), "a.parquet")
  expect_equal(st$size_of("a.parquet"), 20)
  # A seeded simulation that uploads mid-run must draw the same numbers after.
  expect_identical(.Random.seed, seed_before)
})

test_that("a failed upload POST (piggyback only warns) keeps the old asset", {
  st <- .fake_release(a.parquet = 5)
  st$fail <- "upload"
  .mock_release(st)
  expect_error(suppressWarnings(safe_release_upload(.new_file(), "o/r", "t", name = "a.parquet")),
               class = "vb_error_transient")
  expect_equal(st$names(), "a.parquet")
  expect_equal(st$size_of("a.parquet"), 5)
})

test_that("an upload that never completes keeps the old asset", {
  st <- .fake_release(a.parquet = 5)
  st$fail <- "stuck"
  .mock_release(st)
  expect_error(safe_release_upload(.new_file(), "o/r", "t", name = "a.parquet"),
               class = "vb_error_transient")
  expect_equal(st$size_of("a.parquet"), 5)
})

test_that("a failed delete of the old asset stops before the rename", {
  st <- .fake_release(a.parquet = 5)
  st$fail <- "delete"
  .mock_release(st)
  expect_error(safe_release_upload(.new_file(), "o/r", "t", name = "a.parquet"),
               class = "vb_error_transient")
  expect_equal(st$size_of("a.parquet"), 5)
})

test_that("a failed rename strands the data under its temp name, and confirm_fresh_start refuses it", {
  st <- .fake_release(a.parquet = 5)
  st$fail <- "patch"
  .mock_release(st)
  expect_error(safe_release_upload(.new_file(20), "o/r", "t", name = "a.parquet"),
               class = "vb_error_integrity")
  expect_false("a.parquet" %in% st$names())
  stranded <- grep("^vbnew-.*--a\\.parquet$", st$names(), value = TRUE)
  expect_length(stranded, 1L)
  expect_equal(st$size_of(stranded), 20)

  testthat::local_mocked_bindings(vb_read_prev_manifest = function(...) NULL)
  expect_error(confirm_fresh_start("o/r", "t", "a.parquet"),
               "unswapped upload", class = "vb_error_integrity")

  # The next successful upload clears the stranded copy.
  st$fail <- character()
  safe_release_upload(.new_file(30), "o/r", "t", name = "a.parquet")
  expect_equal(st$names(), "a.parquet")
  expect_equal(st$size_of("a.parquet"), 30)
})

test_that("confirm_fresh_start: absent and never listed -> TRUE; present -> FALSE", {
  st <- .fake_release(b.parquet = 5)
  .mock_release(st)
  testthat::local_mocked_bindings(vb_read_prev_manifest = function(...) {
    list(assets = list(list(name = "b.parquet", rows = 3)))
  })
  expect_true(confirm_fresh_start("o/r", "t", "predictions_2027.parquet"))
  expect_false(confirm_fresh_start("o/r", "t", "b.parquet"))
})

test_that("confirm_fresh_start aborts when bus_manifest.json lists an asset the release lacks", {
  st <- .fake_release(other.parquet = 5)
  .mock_release(st)
  testthat::local_mocked_bindings(vb_read_prev_manifest = function(...) {
    list(assets = list(list(name = "predictions_2026.parquet", rows = 200)))
  })
  expect_error(confirm_fresh_start("o/r", "predictions", "predictions_2026.parquet"),
               "LOST", class = "vb_error_integrity")

  withr::local_envvar(TORP_ALLOW_FRESH_START = "predictions_2026.parquet")
  expect_true(confirm_fresh_start("o/r", "predictions", "predictions_2026.parquet"))
})

test_that("confirm_fresh_start: no manifest on a tag that holds data cannot prove 'new'", {
  st <- .fake_release(chains_data_2025_all.parquet = 5)
  .mock_release(st)
  testthat::local_mocked_bindings(vb_read_prev_manifest = function(...) NULL)
  expect_error(confirm_fresh_start("o/r", "chains-data", "chains_data_2026_all.parquet"),
               "no bus_manifest.json", class = "vb_error_integrity")

  # An empty tag (or one holding only temp copies of other names) is a real first run.
  st2 <- .fake_release()
  .mock_release(st2)
  expect_true(confirm_fresh_start("o/r", "chains-data", "chains_data_2026_all.parquet"))
})

test_that(".publish_bus_manifest never replaces a manifest it could not read", {
  f <- .new_file(20)
  uploads <- 0L
  testthat::local_mocked_bindings(
    safe_release_upload = function(...) { uploads <<- uploads + 1L; invisible(1) },
    vb_read_prev_manifest = function(...) {
      stop(structure(class = c("http_error_500", "error", "condition"),
                     list(message = "Server Error (HTTP 500)", call = NULL)))
    }
  )
  expect_error(.publish_bus_manifest("predictions", "predictions_2026.parquet", f, rows = 3))
  expect_equal(uploads, 0L)
})

test_that(".publish_bus_manifest starts a manifest only on a tag with no other data", {
  f <- .new_file(20)
  uploads <- 0L
  listing <- data.frame(id = 1, name = "predictions_2025.parquet", size = 5, state = "uploaded")
  testthat::local_mocked_bindings(
    safe_release_upload = function(...) { uploads <<- uploads + 1L; invisible(1) },
    vb_read_prev_manifest = function(...) NULL,
    .release_assets_with_state = function(...) listing
  )
  expect_error(.publish_bus_manifest("predictions", "predictions_2026.parquet", f, rows = 3),
               "one-entry manifest")
  expect_equal(uploads, 0L)

  listing <- data.frame(id = 1, name = "predictions_2026.parquet", size = 20, state = "uploaded")
  .publish_bus_manifest("predictions", "predictions_2026.parquet", f, rows = 3)
  expect_equal(uploads, 1L)
})

test_that("confirm_fresh_start: a tag that does not exist yet is a genuine first run", {
  st <- .fake_release()
  st$fail <- "tag404"
  .mock_release(st)
  expect_true(confirm_fresh_start("o/r", "new-tag", "x.parquet"))
})

test_that("confirm_fresh_start never reads a listing failure as absence", {
  st <- .fake_release()
  st$fail <- "list"
  .mock_release(st)
  expect_error(confirm_fresh_start("o/r", "t", "x.parquet"))
})
