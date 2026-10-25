# Absent extended stats: NA on the AFLW path, 0 on the AFLM path (Pete 2026-10-26).
# The AFLW feed's extendedStats block is empty, so those stats are "not
# recorded", and NA lets the stat-rating NA exclusion (A198) skip them.

# A frame shaped like the AFLW playerStats/match feed after flattening:
# basic stats present (dot-notation, as the live API returns), and
# `stats.extendedStats` a single all-NA logical column.
.aflw_like_feed <- function() {
  data.frame(
    match_id = c("CD_M20252640101", "CD_M20252640101"),
    player.player.player.playerId = c("CD_I1", "CD_I2"),
    teamStatus = c("home", "away"),
    stats.goals = c(2L, 0L),
    stats.intercepts = c(3L, 0L),
    stats.metresGained = c(250L, 40L),
    stats.extendedStats = c(NA, NA),
    check.names = FALSE
  )
}

absent_ext <- c(
  "spoils", "pressure_acts", "def_half_pressure_acts", "hitouts_to_advantage",
  "ruck_contests", "ground_ball_gets", "contest_def_one_on_ones",
  "contest_off_one_on_ones", "contest_off_wins", "contest_def_losses"
)

test_that("absent extended stats become NA when absent_fill = NA_integer_", {
  out <- suppressMessages(
    .normalise_player_stats_columns(.aflw_like_feed(), absent_fill = NA_integer_)
  )
  for (col in absent_ext) {
    expect_true(col %in% names(out), info = col)
    expect_true(all(is.na(out[[col]])), info = col)
    expect_type(out[[col]], "integer")
  }
})

test_that("present stats are untouched on the AFLW path", {
  out <- suppressMessages(
    .normalise_player_stats_columns(.aflw_like_feed(), absent_fill = NA_integer_)
  )
  expect_identical(out$goals, c(2L, 0L))
  expect_identical(out$intercepts, c(3L, 0L))   # a real zero stays zero
  expect_identical(out$metres_gained, c(250L, 40L))
})

test_that("the NA-fill is announced as NA-filling, not zero-filling", {
  expect_message(
    .normalise_player_stats_columns(.aflw_like_feed(), absent_fill = NA_integer_),
    "NA-filling missing API columns"
  )
  expect_message(
    .normalise_player_stats_columns(.aflw_like_feed()),
    "Zero-filling missing API columns"
  )
})

test_that("default (AFLM) path still zero-fills and is otherwise unchanged", {
  dflt <- suppressMessages(.normalise_player_stats_columns(.aflw_like_feed()))
  expl <- suppressMessages(.normalise_player_stats_columns(.aflw_like_feed(), absent_fill = 0L))
  expect_identical(dflt, expl)
  for (col in absent_ext) expect_identical(dflt[[col]], c(0L, 0L), info = col)

  # A complete AFLM-shaped frame triggers no fill at all, and the two fill
  # settings give identical output.
  full <- data.table::as.data.table(stats::setNames(
    rep(list(c(1L, 2L)), length(absent_ext) + 10L),
    c(absent_ext, "intercepts", "one_percenters", "rebound50s", "frees_against",
      "frees_for", "score_involvements", "metres_gained", "turnovers",
      "goal_assists", "shots_at_goal")
  ))
  expect_no_message(a <- .normalise_player_stats_columns(data.table::copy(full)))
  b <- .normalise_player_stats_columns(data.table::copy(full), absent_fill = NA_integer_)
  expect_identical(a, b)
})

test_that("absent_fill rejects anything but 0L or NA_integer_", {
  expect_error(.normalise_player_stats_columns(.aflw_like_feed(), absent_fill = 0), "absent_fill")
  expect_error(.normalise_player_stats_columns(.aflw_like_feed(), absent_fill = NA), "absent_fill")
})

test_that("load_player_stats() NA-fills for AFLW and zero-fills for AFLM", {
  local_mocked_bindings(
    .load_with_cache = function(...) tibble::as_tibble(.aflw_like_feed()),
    .normalise_team_values = function(x) x
  )
  aflw <- suppressMessages(load_player_stats(2025, comp = "AFLW"))
  aflm <- suppressMessages(load_player_stats(2025, comp = "AFLM"))
  for (col in absent_ext) {
    expect_true(all(is.na(aflw[[col]])), info = col)
    expect_identical(aflm[[col]], c(0L, 0L), info = col)
  }
  expect_identical(aflw$intercepts, aflm$intercepts)
})
