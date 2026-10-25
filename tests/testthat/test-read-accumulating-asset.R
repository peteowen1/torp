# .read_accumulating_asset() -- R/release_upload.R. The one definition of
# "never start fresh over season history", shared by chains, pbp,
# score_events, injuries, predictions and ratings. Before it existed the six
# copies had drifted: some treated a 0-row read as absent and some did not,
# and one ran confirm_fresh_start() inside the 404 handler. No network --
# the reader and confirm_fresh_start() are mocked.

.absent_err <- function() {
  structure(class = c("vb_error_absent", "vb_error", "error", "condition"),
            list(message = "404 not found", call = NULL))
}
.transient_err <- function() {
  structure(class = c("vb_error_transient", "vb_error", "error", "condition"),
            list(message = "simulated 500", call = NULL))
}

test_that("a present asset is returned as read, without asking confirm_fresh_start()", {
  local_mocked_bindings(
    confirm_fresh_start = function(repo, tag, name) stop("must not be called")
  )
  df <- data.frame(x = 1:3)
  out <- .read_accumulating_asset("a.parquet", "t", read = function() df, repo = "o/r")
  expect_identical(out, df)
})

test_that("the default reader is file_reader() on the name without its extension", {
  seen <- NULL
  local_mocked_bindings(
    file_reader = function(file_name, release_tag) {
      seen <<- c(file_name, release_tag)
      data.frame(x = 1)
    }
  )
  .read_accumulating_asset("chains_data_2026_all.parquet", "chains-data", repo = "o/r")
  expect_equal(seen, c("chains_data_2026_all", "chains-data"))
})

test_that("a 404 starts fresh only after confirm_fresh_start() returns TRUE", {
  asked <- NULL
  local_mocked_bindings(
    confirm_fresh_start = function(repo, tag, name) {
      asked <<- c(repo, tag, name)
      TRUE
    }
  )
  out <- .read_accumulating_asset("a.parquet", "t", read = function() stop(.absent_err()),
                                  repo = "o/r")
  expect_null(out)
  expect_equal(asked, c("o/r", "t", "a.parquet"))
})

test_that("a 404 that confirm_fresh_start() does not confirm aborts", {
  local_mocked_bindings(confirm_fresh_start = function(repo, tag, name) FALSE)
  expect_error(
    .read_accumulating_asset("a.parquet", "t", read = function() stop(.absent_err()), repo = "o/r"),
    "not confirmed absent"
  )
})

test_that("a confirm_fresh_start() error aborts rather than starting fresh", {
  local_mocked_bindings(confirm_fresh_start = function(repo, tag, name) stop("listing failed"))
  expect_error(
    .read_accumulating_asset("a.parquet", "t", read = function() stop(.absent_err()), repo = "o/r"),
    "Could not verify"
  )
})

test_that("a transient read error aborts and never reaches confirm_fresh_start()", {
  local_mocked_bindings(
    confirm_fresh_start = function(repo, tag, name) stop("must not be called")
  )
  expect_error(
    .read_accumulating_asset("a.parquet", "t", read = function() stop(.transient_err()), repo = "o/r"),
    "not a confirmed-absent"
  )
})

test_that("empty_is_absent decides whether a 0-row read must pass confirm_fresh_start()", {
  # The drift the helper exists to end: injuries, predictions and ratings
  # treated a 0-row read as ambiguous; chains, pbp and score_events kept it.
  local_mocked_bindings(confirm_fresh_start = function(repo, tag, name) FALSE)
  empty <- data.frame(x = integer(0))

  kept <- .read_accumulating_asset("a.parquet", "t", read = function() empty, repo = "o/r")
  expect_identical(kept, empty)

  expect_error(
    .read_accumulating_asset("a.parquet", "t", read = function() empty, repo = "o/r",
                             empty_is_absent = TRUE),
    "not confirmed absent"
  )
})

test_that("confirm = FALSE (ratings full rebuild) returns NULL for an absent asset unchecked", {
  local_mocked_bindings(
    confirm_fresh_start = function(repo, tag, name) stop("must not be called")
  )
  expect_null(.read_accumulating_asset("a.parquet", "t", read = function() stop(.absent_err()),
                                       repo = "o/r", confirm = FALSE))
  # ...but a transient error still aborts.
  expect_error(
    .read_accumulating_asset("a.parquet", "t", read = function() stop(.transient_err()),
                             repo = "o/r", confirm = FALSE),
    "not a confirmed-absent"
  )
})
