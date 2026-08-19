# Three silent-failure paths found reviewing the v3 promotion diff.
#
# Each one publishes or blesses WRONG DATA without raising anything, which is
# the failure mode this codebase keeps hitting, so each gets a test that fails
# without its fix rather than a comment promising care.

# --- 1. A confirmed-absent asset is not "could not tell" --------------------
# .vb_asset_true_size() answers a byte count, NA_real_ for "could not tell", and
# VB_ASSET_CONFIRMED_ABSENT for "the server said 404". Before the fix a 404 fell
# through to NA_real_, and save_to_release()'s caller reads NA_real_ as
# "unreachable -- warn and proceed". So a genuinely lost upload, for exactly the
# missing-from-listing case this file had just started routing here, reported
# success.

test_that("a 404 on the download path is confirmed-absent, not unknown", {
  testthat::local_mocked_bindings(
    curl_fetch_memory = function(url, handle) list(status_code = 404L, headers = raw()),
    parse_headers_list = function(h) list(),
    handle_setheaders = function(...) invisible(NULL),
    handle_setopt = function(...) invisible(NULL),
    new_handle = function(...) list(),
    .package = "curl")
  expect_identical(.vb_asset_true_size("o/r", "tag", "f.parquet"),
                   VB_ASSET_CONFIRMED_ABSENT)
})

test_that("an unreachable answer stays NA, so it keeps warning rather than aborting", {
  # The fail-safe direction: a 500 tells us nothing about existence, and must
  # NOT be turned into "the file is gone".
  testthat::local_mocked_bindings(
    curl_fetch_memory = function(url, handle) list(status_code = 500L, headers = raw()),
    parse_headers_list = function(h) list(),
    handle_setheaders = function(...) invisible(NULL),
    handle_setopt = function(...) invisible(NULL),
    new_handle = function(...) list(),
    .package = "curl")
  expect_true(is.na(.vb_asset_true_size("o/r", "tag", "f.parquet")))
})

# --- 2. The engine stamp survives a narrow projection ----------------------
# An unstamped frame is priced as v2 whatever produced it. .frame_epv_engine()
# only warns when the GLOBAL engine constant reads v3 -- which is exactly not
# the case while a v3 candidate is inspected before promotion -- so a dropped
# column is silent in the one workflow the candidate mechanism exists for.

test_that("a columns= projection still stamps the engine, and still honours columns", {
  seen <- NULL
  testthat::local_mocked_bindings(
    load_from_url = function(url, ..., columns = NULL) {
      seen <<- columns
      data.table::data.table(player_id = "a", epv = 1.5, epv_engine = "v3")
    })
  out <- load_player_game_data(2026, version = "v3", columns = c("player_id", "epv"))

  expect_true("epv_engine" %in% seen)          # read even though unasked for
  expect_identical(attr(out, "epv_engine"), "v3")
  expect_false("epv_engine" %in% names(out))   # and dropped again afterwards
})

test_that("a caller who asks for the engine column keeps it", {
  testthat::local_mocked_bindings(
    load_from_url = function(url, ..., columns = NULL)
      data.table::data.table(player_id = "a", epv_engine = "v3"))
  out <- load_player_game_data(2026, version = "v3",
                               columns = c("player_id", "epv_engine"))
  expect_true("epv_engine" %in% names(out))
  expect_identical(attr(out, "epv_engine"), "v3")
})

# --- 3. A partly-stamped frame is refused, not silently stamped -------------
# Seasons load as separate files and rbindlist(fill = TRUE) them together, so a
# season rebuilt under v3 supplies epv_engine while seasons not yet rebuilt
# supply NA. na.omit() then left ONE distinct engine, the whole frame was
# stamped v3, and centre_epv_by_position() applied the v3 per-channel scale to
# every row -- repricing years of v2 history on a clean run with no warning.

test_that("a frame with some rows stamped and some NA is refused", {
  part <- data.table::data.table(
    player_id = c("a", "b", "c"),
    season    = c(2024L, 2025L, 2026L),
    epv_engine = c(NA_character_, NA_character_, "v3"))   # only 2026 rebuilt
  expect_error(.restore_epv_engine_attr(part), class = "torp_error_mixed_epv_engine")
})

test_that("a fully stamped frame is still accepted", {
  full <- data.table::data.table(player_id = c("a", "b"), epv_engine = c("v3", "v3"))
  expect_identical(attr(.restore_epv_engine_attr(full), "epv_engine"), "v3")
})

test_that("a frame with no engine column at all is still treated as v2", {
  # No column means no season was rebuilt -- a genuine v2 frame, not ambiguity.
  none <- data.table::data.table(player_id = c("a", "b"))
  expect_null(attr(.restore_epv_engine_attr(none), "epv_engine"))
})
