# Stage 3 must read the player-game data Stage 2 wrote -- including its vintage.
#
# WHY THIS FILE EXISTS. Stage 2 saves a candidate vintage as
# player_game_<season>_v3.parquet; Stage 3 called load_player_game_data(TRUE)
# with no version and so read canonical player_game_<season>.parquet. A run
# labelled v3 therefore built its ratings from v2 player-game data, with every
# guard passing and nothing failing (2026-08-18, staging EPV v3). The engine
# flip is only observable through these filenames, so this is where it is
# asserted.

capture_urls <- function(...) {
  seen <- NULL
  testthat::local_mocked_bindings(
    load_from_url = function(url, ...) { seen <<- url; data.table::data.table() })
  load_player_game_data(...)
  seen
}

test_that("a vintage load asks for the vintage's own files", {
  urls <- capture_urls(2024, version = "v3")
  expect_length(urls, 1)
  expect_match(urls, "player_game_2024_v3[.]parquet$")
})

test_that("no vintage still asks for canonical", {
  urls <- capture_urls(2024)
  expect_match(urls, "player_game_2024[.]parquet$")
  expect_false(any(grepl("_v3", urls)))
})

test_that("the engine stamp survives the parquet round-trip", {
  # Parquet keeps columns and drops attributes, so this is what a loaded frame
  # actually looks like: the engine present as data, absent as an attribute.
  round_tripped <- data.table::data.table(player_id = "a", epv_engine = "v3")
  expect_null(attr(round_tripped, "epv_engine"))
  restored <- .restore_epv_engine_attr(round_tripped)
  expect_identical(attr(restored, "epv_engine"), "v3")
})

test_that("a frame with no engine column is left alone", {
  # Files written before the column existed are all v2, and v2 is what a NULL
  # attribute already means -- so this must not invent a stamp.
  out <- .restore_epv_engine_attr(data.table::data.table(player_id = "a"))
  expect_null(attr(out, "epv_engine"))
})

test_that("mixing two engines in one frame is refused", {
  expect_error(
    .restore_epv_engine_attr(
      data.table::data.table(player_id = c("a", "b"), epv_engine = c("v2", "v3"))),
    class = "torp_error_mixed_epv_engine"
  )
})
