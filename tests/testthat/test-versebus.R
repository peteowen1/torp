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

test_that("vb_download sha mismatch vs manifest warns and trusts the download when no live listing contradicts it", {
  # A manifest sha256 mismatch is far more often a stale manifest (upload
  # succeeded, the LAST manifest publish didn't) than real corruption --
  # vb_download() downgrades this to a warning and falls back to
  # verify-by-size against a live listing. With no listing available here
  # (no gh::gh mock -- vb_list_assets fails, caught, treated as
  # "can't corroborate either way"), the download is trusted and completes.
  #
  # This is the CONTRACT, not an accident. panna#187's second fix originally
  # made a failed listing abort; that bricks the asset on an API blip, which
  # is what this fallback exists to prevent. bouncer shipped it, CI caught
  # it, and it was reverted (bouncerverse/bouncer 5edd3ac). Only the silence
  # was fixed -- see the warning regression test below.
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "model.parquet")
  writeLines("PRIOR-GOOD-CONTENT", dest)

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

  expect_warning(
    vb_download("test/fixture", "test-tag", "model.parquet", dest,
                manifest = manifest),
    "sha256 mismatch"
  )
  expect_identical(readLines(dest), "PAR1........PAR1")  # new content served
  leftovers <- list.files(dir, pattern = "^\\.vb_dl_", all.files = TRUE)
  expect_length(leftovers, 0)                            # temp cleaned up
})

test_that("vb_download sha mismatch vs manifest still raises vb_error_integrity when a live listing confirms a size mismatch too", {
  # Real corruption isn't only caught by sha256 -- when a live listing IS
  # available and shows the asset's actual size disagrees with the
  # download, that's independent corroborating evidence, not just a stale
  # manifest, and vb_download() still aborts.
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
  testthat::local_mocked_bindings(
    gh = function(endpoint, ..., owner, repo, tag) {
      list(assets = list(list(name = "model.parquet", size = 999999,
                              updated_at = "2026-01-01T00:00:00Z", id = 1)))
    },
    .package = "gh"
  )

  expect_error(
    suppressWarnings(vb_download("test/fixture", "test-tag", "model.parquet", dest,
                                 manifest = manifest)),
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

test_that(".vb_retry succeeds after transient failures within the attempt budget", {
  calls <- 0L
  result <- .vb_retry(function() {
    calls <<- calls + 1L
    if (calls < 3L) stop("simulated transient failure")
    "ok"
  }, times = 3L, delays = c(0, 0))
  expect_identical(result, "ok")
  expect_identical(calls, 3L)
})

test_that(".vb_retry gives up after exhausting attempts and raises the last error", {
  calls <- 0L
  expect_error(
    .vb_retry(function() {
      calls <<- calls + 1L
      stop("simulated transient failure")
    }, times = 3L, delays = c(0, 0)),
    "simulated transient failure"
  )
  expect_identical(calls, 3L)
})

test_that(".vb_retry does not retry when should_retry returns FALSE", {
  calls <- 0L
  expect_error(
    .vb_retry(function() {
      calls <<- calls + 1L
      stop("confirmed absent")
    }, times = 3L, delays = c(0, 0), should_retry = function(e) FALSE),
    "confirmed absent"
  )
  expect_identical(calls, 1L)
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

# ---------------------------------------------------------------------------
# panna#187 (ported from bouncer's dev->main review, peteowen1/bouncer@86e2ebc):
# four defects found in bouncer's pre-fix versebus.R, byte-identical to
# torp/panna's copy at the time. Fixed in bouncer, then panna (panna#187,
# 8e5aee6); ported here since torp is the canonical vendored copy.
# ---------------------------------------------------------------------------

test_that("vb_read_manifest retry classifies a transient listing error as transient, never absent (regression: silently disabled sha256 verification for the rest of the session)", {
  # Only the RETRY branch is under test here -- it fires when the session
  # has previously seen a manifest for this tag AND the first attempt just
  # found it (legitimately) missing from the listing. If the retry then hits
  # a genuine network blip, the old code swallowed every error as "absent"
  # and fell through to legacy mode -- a network hiccup silently disabled
  # sha256 verification for the rest of the session, the exact inversion of
  # this file's own rule that uncertain classification is transient, not
  # absent.
  repo <- "test/fixture"; tag <- "retry-regression-tag"
  key <- paste0(repo, "@", tag)
  seen_key <- paste0("seen_", key)
  assign(seen_key, TRUE, envir = .vb_state)
  withr::defer(rm(list = seen_key, envir = .vb_state))

  testthat::local_mocked_bindings(Sys.sleep = function(...) invisible(NULL), .package = "base")

  call_n <- 0L
  testthat::local_mocked_bindings(
    gh = function(...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) {
        # First attempt: listing succeeds, manifest genuinely not present.
        list(assets = list(list(name = "other.parquet", size = 1,
                                updated_at = "2026-01-01T00:00:00Z", id = 1)))
      } else {
        # Retry: the LISTING CALL ITSELF fails -- a transient blip, not a
        # confirmed absence.
        stop(simpleError("simulated network timeout on retry"))
      }
    },
    .package = "gh"
  )

  expect_error(
    vb_read_manifest(repo, tag, required = FALSE),
    class = "vb_error_transient"
  )
  expect_identical(call_n, 2L)  # confirms the retry branch actually ran
})

test_that("vb_download's verify_by_size WARNS when the listing call fails, and says the file was accepted unverified", {
  # The defect was silence, not the fallback. Trusting an unverifiable
  # download is deliberate (see the sha-mismatch test above); doing it
  # without saying so is not. Aborting instead would brick the asset on any
  # transient API failure -- bouncer tried that and reverted it in 5edd3ac.
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "unmanifested.rds")

  testthat::local_mocked_bindings(
    pb_download = function(file, dest, repo, tag, overwrite = TRUE, ...) {
      writeLines("some-content", file.path(dest, file))
      invisible(NULL)
    },
    .package = "piggyback"
  )
  # Listing fails outright (not a 404) every time verify_by_size calls it.
  testthat::local_mocked_bindings(
    gh = function(...) stop(simpleError("simulated listing failure")),
    .package = "gh"
  )

  # No manifest entry (the common, unmanifested-tag case) -- verify_by_size()
  # is the ONLY integrity check on this path, so its inability to run must be
  # audible.
  expect_warning(
    vb_download(repo = "test/fixture", tag = "verify-size-tag",
                name = "unmanifested.rds", dest = dest,
                manifest = list(assets = NULL)),
    "WITHOUT verification"
  )
  # Behaviour preserved: the download still completes.
  expect_true(file.exists(dest))
  expect_true(file.exists(paste0(dest, ".sha256")))
})

test_that("vb_publish's cache-invalidation hook failure is surfaced as a warning, never swallowed (regression: consumers served pre-publish data indefinitely with nothing recording why)", {
  dir <- withr::local_tempdir()
  p <- write_fixture_file(dir, "hookfile.rds", content = "hook-test-content")
  sz <- file.size(p)

  testthat::local_mocked_bindings(
    pb_upload = function(file, repo, tag, overwrite = TRUE, ...) invisible(NULL),
    .package = "piggyback"
  )
  testthat::local_mocked_bindings(
    gh = function(...) list(assets = list(
      list(name = "hookfile.rds", size = sz, updated_at = "2026-01-01T00:00:00Z", id = 1)
    )),
    .package = "gh"
  )
  withr::local_options(list(versebus.on_publish = function(repo, tag) stop("hook boom")))

  expect_warning(
    vb_publish(p, repo = "test/fixture", tag = "hook-regression-tag",
              carry_forward = FALSE, max_retries = 0),
    "cache-invalidation hook failed"
  )
})

test_that("vb_generation drops NA updated_at before max() instead of letting one malformed asset null the whole generation", {
  # vb_list_assets() deliberately fills NA_character_ for an asset missing
  # updated_at so ONE bad entry doesn't kill the whole listing -- but
  # max(assets$updated_at) with no na.rm propagated that NA through,
  # indistinguishable from the "no assets at all" case.
  testthat::local_mocked_bindings(
    gh = function(...) list(assets = list(
      list(name = "a.parquet", size = 1, updated_at = "2026-01-01T00:00:00Z", id = 1),
      list(name = "b.parquet", size = 1, updated_at = NULL, id = 2),  # malformed
      list(name = "c.parquet", size = 1, updated_at = "2026-03-01T00:00:00Z", id = 3)
    )),
    .package = "gh"
  )
  gen <- vb_generation("test/fixture", "generation-na-tag")
  expect_identical(gen, "2026-03-01T00:00:00Z")
})
