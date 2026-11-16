# Issue #314: two quiet failure paths left over from #250. No network -- every
# fetch is mocked.

skip_if_not_installed("withr")

test_that("load_pbp stops when every download failed (not a 404)", {
  withr::local_envvar(c(VERSEBUS_STRICT = ""))   # lenient mode: the old 0-row path
  empty_transient <- data.table::data.table()
  attr(empty_transient, "skip_reason") <- "transient"
  testthat::local_mocked_bindings(
    parquet_from_url_cached = function(...) empty_transient
  )
  expect_error(load_pbp(2024, rounds = TRUE), class = "vb_error_transient")
})

test_that("load_pbp still returns an empty frame for a file that is genuinely absent (404)", {
  withr::local_envvar(c(VERSEBUS_STRICT = ""))
  empty_absent <- data.table::data.table()
  attr(empty_absent, "skip_reason") <- "not_found"
  testthat::local_mocked_bindings(
    parquet_from_url_cached = function(...) empty_absent
  )
  out <- suppressWarnings(load_pbp(2024, rounds = TRUE))
  expect_equal(nrow(out), 0)
})

test_that("load_pbp stops when a multi-season download fails entirely in lenient mode", {
  withr::local_envvar(c(VERSEBUS_STRICT = ""))
  testthat::local_mocked_bindings(
    is_download_skippable = function(...) FALSE,
    is_locally_stored = function(...) FALSE
  )
  testthat::local_mocked_bindings(
    multi_download = function(urls, destfiles, ...) {
      data.frame(success = rep(FALSE, length(urls)), status_code = rep(500L, length(urls)))
    },
    .package = "curl"
  )
  expect_error(
    suppressWarnings(load_pbp(2023:2024, rounds = TRUE)),
    class = "vb_error_transient"
  )
})

test_that("other loaders keep the lenient empty-frame behaviour (opt-in only)", {
  withr::local_envvar(c(VERSEBUS_STRICT = ""))
  empty_transient <- data.table::data.table()
  attr(empty_transient, "skip_reason") <- "transient"
  testthat::local_mocked_bindings(
    parquet_from_url_cached = function(...) empty_transient
  )
  out <- suppressWarnings(load_chains(2024, rounds = TRUE))
  expect_equal(nrow(out), 0)
})

test_that("get_players says what went wrong when player details came back empty", {
  testthat::local_mocked_bindings(load_player_details = function(...) tibble::tibble())
  expect_error(get_players(season = 2026), "No player details were loaded")
})

test_that("get_match_chains stops with a clear message when player details time out", {
  testthat::local_mocked_bindings(
    get_round_games = function(...) data.frame(matchId = "CD_M20260140101", season = 2026),
    get_many_game_chains = function(...) data.frame(matchId = "CD_M20260140101", playerId = "CD_I1"),
    load_player_details = function(...) tibble::tibble()
  )
  expect_error(
    suppressMessages(get_match_chains(2026, 1)),
    "No player details were loaded"
  )
})
