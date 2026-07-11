# Shared versebus regression tests — vendor alongside R/versebus.R.
# Copy verbatim into each package's tests/testthat/test-versebus.R.
# Mocks piggyback + gh at the package namespace boundary so no network is hit.

make_fake_manifest <- function(tag = "test-tag", assets = list()) {
  list(
    schema_version = 1L,
    tag = tag,
    generation = "20260711T000000Z-l000000",
    produced_at_utc = "2026-07-11T00:00:00Z",
    producer = list(repo = "test/fixture", workflow = "test", run_id = "", run_attempt = ""),
    assets = assets,
    notes = ""
  )
}

write_fixture_file <- function(dir, name, content = "fixture-bytes") {
  path <- file.path(dir, name)
  writeLines(content, path)
  path
}

# A minimal but magic-valid "parquet": PAR1 ... PAR1
write_fixture_parquet <- function(dir, name = "fix.parquet") {
  path <- file.path(dir, name)
  con <- file(path, "wb")
  writeBin(charToRaw("PAR1........PAR1"), con)
  close(con)
  path
}

test_that("vb_publish aborts on a data-asset failure and never uploads the manifest", {
  dir <- withr::local_tempdir()
  p1 <- write_fixture_file(dir, "a.rds")
  p2 <- write_fixture_file(dir, "b.rds")

  upload_log <- character(0)
  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) {
      upload_log <<- c(upload_log, basename(file))
      if (basename(file) == "b.rds") stop("boom: simulated network failure")
      invisible(NULL)
    },
    .package = "piggyback"
  )
  # previous-manifest read: pretend the tag has no manifest yet
  testthat::local_mocked_bindings(
    gh = function(...) stop(structure(class = c("http_error_404", "error", "condition"),
                                      list(message = "Not Found", call = NULL))),
    .package = "gh"
  )

  expect_error(
    vb_publish(c(p1, p2), repo = "test/fixture", tag = "test-tag", max_retries = 0),
    class = "vb_error_transient"
  )
  expect_false("bus_manifest.json" %in% upload_log)  # the manifest-last gate
  expect_true("a.rds" %in% upload_log)               # data upload was attempted
})

test_that("vb_download sha mismatch raises vb_error_integrity and leaves prior dest untouched", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "model.parquet")
  writeLines("PRIOR-GOOD-CONTENT", dest)
  prior <- readLines(dest)

  entry <- list(name = "model.parquet",
                sha256 = strrep("0", 64),  # will never match
                bytes = 16, rows = NA_integer_)
  manifest <- make_fake_manifest(assets = list(entry))

  testthat::local_mocked_bindings(
    pb_download = function(file, dest, repo, tag, overwrite = TRUE, ...) {
      con <- file(file.path(dest, file), "wb")
      writeBin(charToRaw("PAR1........PAR1"), con)
      close(con)
      invisible(NULL)
    },
    .package = "piggyback"
  )

  expect_error(
    vb_download("test/fixture", "test-tag", "model.parquet", dest,
                manifest = manifest),
    class = "vb_error_integrity"
  )
  expect_identical(readLines(dest), prior)          # dest untouched
  leftovers <- list.files(dir, pattern = "^\\.vb_dl_", all.files = TRUE)
  expect_length(leftovers, 0)                       # temp cleaned up
})

test_that("vb_confirm_absent on a 500 listing raises vb_error_transient, never TRUE", {
  testthat::local_mocked_bindings(
    gh = function(...) stop(structure(class = c("http_error_500", "error", "condition"),
                                      list(message = "Server Error", call = NULL))),
    .package = "gh"
  )
  expect_error(
    vb_confirm_absent("test/fixture", "test-tag", "anything.parquet"),
    class = "vb_error_transient"
  )
})

test_that("vb_confirm_absent: tag 404 is positive absence; present asset is FALSE", {
  fake_release <- list(assets = list(
    list(name = "have.parquet", size = 10, updated_at = "2026-07-11T00:00:00Z", id = 1)
  ))
  testthat::local_mocked_bindings(
    gh = function(...) fake_release, .package = "gh"
  )
  expect_false(vb_confirm_absent("test/fixture", "test-tag", "have.parquet"))
  expect_true(vb_confirm_absent("test/fixture", "test-tag", "missing.parquet"))

  testthat::local_mocked_bindings(
    gh = function(...) stop(structure(class = c("http_error_404", "error", "condition"),
                                      list(message = "Not Found", call = NULL))),
    .package = "gh"
  )
  expect_true(vb_confirm_absent("test/fixture", "no-such-tag", "x.parquet"))
})

test_that("vb_guard_accumulate aborts on a >10% shrink and passes growth", {
  big <- data.frame(x = seq_len(1000))
  small <- data.frame(x = seq_len(400))
  expect_error(vb_guard_accumulate(big, small), class = "vb_error_integrity")
  expect_silent(vb_guard_accumulate(big, data.frame(x = seq_len(1001))))
  expect_silent(vb_guard_accumulate(big[0, , drop = FALSE], small))  # empty existing OK
})

test_that("vb_classify_error defaults ambiguity to transient", {
  expect_identical(vb_classify_error(simpleError("weird unclassifiable failure")), "transient")
  expect_identical(vb_classify_error(simpleError("connection timed out")), "transient")
  e404 <- structure(class = c("http_error_404", "error", "condition"),
                    list(message = "Not Found", call = NULL))
  expect_identical(vb_classify_error(e404), "absent")
  e429 <- structure(class = c("http_error_429", "error", "condition"),
                    list(message = "rate limited", call = NULL))
  expect_identical(vb_classify_error(e429), "transient")
})

test_that("vb_atomic_write never leaves a partial dest", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "out.json")
  writeLines("ORIGINAL", dest)
  expect_error(
    vb_atomic_write(function(p) { writeLines("HALF", p); stop("mid-write crash") }, dest)
  )
  expect_identical(readLines(dest), "ORIGINAL")
  expect_length(list.files(dir, pattern = "^\\.vb_", all.files = TRUE), 0)
})

test_that("manifest merge carries forward previous entries on partial publish", {
  prev <- make_fake_manifest(assets = list(
    list(name = "old.parquet", sha256 = strrep("a", 64), bytes = 5, rows = 10L)
  ))
  entries <- list(new.parquet = list(name = "new.parquet", sha256 = strrep("b", 64),
                                     bytes = 6, rows = 20L))
  merged <- .vb_merge_entries(prev, entries)
  nms <- vapply(merged, `[[`, character(1), "name")
  expect_setequal(nms, c("new.parquet", "old.parquet"))
})
