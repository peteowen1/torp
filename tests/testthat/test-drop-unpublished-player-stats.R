# A318: player-stats publish paths drop a FIXED column list, never "constant in
# this fetch" (which dropped round_number after the Opening Round).

test_that("a constant round_number survives; the fixed list is dropped", {
  dt <- data.table::data.table(
    match_id = c("m1", "m2"), round_number = c(0L, 0L), goals = c(1L, 2L),
    captain = c(FALSE, FALSE), super_goals = NA, ranking = NA
  )
  out <- .drop_unpublished_player_stats_columns(dt)
  expect_identical(names(out), c("match_id", "round_number", "goals"))
  expect_identical(out$round_number, c(0L, 0L))
})

test_that("a listed column that is now populated is dropped and reported", {
  dt <- data.table::data.table(goals = 1:2, super_goals = c(NA, 1L), captain = c(TRUE, FALSE))
  expect_message(out <- .drop_unpublished_player_stats_columns(dt), "captain, super_goals")
  expect_identical(names(out), "goals")
})

test_that("an empty or absent list is a no-op, silently", {
  dt <- data.table::data.table(goals = 1:2)
  expect_silent(out <- .drop_unpublished_player_stats_columns(dt))
  expect_identical(names(out), "goals")
})

test_that("no publish path selects on n_distinct any more", {
  root <- testthat::test_path("..", "..")
  files <- file.path(root, c("R/load_data.R", "data-raw/01-data/daily_release.R", "data-raw/rebuild_everything.R"))
  skip_if_not(all(file.exists(files)), "source tree not available (installed package)")
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    expect_false(any(grepl("n_distinct(.) > 1", lines, fixed = TRUE)), info = f)
    expect_true(any(grepl(".drop_unpublished_player_stats_columns(", lines, fixed = TRUE)), info = f)
  }
})
