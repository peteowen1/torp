# Weekly AFLW season-stat snapshots (R/aflw_season_snapshots.R) -- the
# differencing that turns cumulative season-to-date totals into round-level
# figures. The edge cases here are the whole point of the function: a delta
# that silently drops a player, invents a negative, or differences a
# percentage is a wrong round-level number that still looks plausible.

.mock_snapshot <- function(player_ids, games, disposals, spoils,
                           snapshot_date = NULL, season = 2026L,
                           disposal_efficiency = NULL) {
  d <- data.table::data.table(
    player_id = player_ids,
    season = season,
    comp = "AFLW",
    team_abbr = rep("RICH", length(player_ids)),
    games_played = games,
    disposals = disposals,
    spoils = spoils,
    disposal_efficiency = if (is.null(disposal_efficiency)) rep(60, length(player_ids)) else disposal_efficiency,
    disposals_avg = disposals / pmax(games, 1)
  )
  if (!is.null(snapshot_date)) data.table::setattr(d, "snapshot_date", as.Date(snapshot_date))
  d
}

test_that("differencing returns what accrued between snapshots", {
  earlier <- .mock_snapshot(c("A", "B"), games = c(3, 4), disposals = c(30, 40), spoils = c(3, 8))
  later <- .mock_snapshot(c("A", "B"), games = c(4, 5), disposals = c(42, 51), spoils = c(5, 9))

  d <- diff_aflw_season_snapshots(earlier, later, recompute_averages = FALSE)

  expect_equal(d[player_id == "A"]$disposals, 12)
  expect_equal(d[player_id == "A"]$spoils, 2)
  expect_equal(d[player_id == "B"]$disposals, 11)
  expect_equal(d[player_id == "A"]$games_played, 1)
})

test_that("a player who did not play is kept with zero deltas, not dropped", {
  # "played no games" and "absent from the data" are different facts; collapsing
  # them would make a delta table silently unable to distinguish rested from missing.
  earlier <- .mock_snapshot(c("A", "B"), games = c(3, 4), disposals = c(30, 40), spoils = c(3, 8))
  later <- .mock_snapshot(c("A", "B"), games = c(4, 4), disposals = c(42, 40), spoils = c(5, 8))

  d <- diff_aflw_season_snapshots(earlier, later, recompute_averages = FALSE)

  expect_equal(nrow(d), 2)
  b <- d[player_id == "B"]
  expect_equal(b$games_played, 0)
  expect_equal(b$disposals, 0)
  expect_equal(b$spoils, 0)
  expect_false(is.na(b$disposals))
})

test_that("drop_unplayed removes them only when asked", {
  earlier <- .mock_snapshot(c("A", "B"), games = c(3, 4), disposals = c(30, 40), spoils = c(3, 8))
  later <- .mock_snapshot(c("A", "B"), games = c(4, 4), disposals = c(42, 40), spoils = c(5, 8))

  d <- diff_aflw_season_snapshots(earlier, later, drop_unplayed = TRUE, recompute_averages = FALSE)

  expect_equal(nrow(d), 1)
  expect_equal(d$player_id, "A")
})

test_that("a player new since the earlier snapshot gets her full total, never a negative", {
  # Merging without an explicit zero prior would leave her stats NA and drop her
  # from the delta entirely -- the debutant would simply vanish from round-level data.
  earlier <- .mock_snapshot("A", games = 3, disposals = 30, spoils = 3)
  later <- .mock_snapshot(c("A", "NEW"), games = c(4, 1), disposals = c(42, 9), spoils = c(5, 2))

  d <- diff_aflw_season_snapshots(earlier, later, recompute_averages = FALSE)

  expect_equal(nrow(d), 2)
  new <- d[player_id == "NEW"]
  expect_equal(new$disposals, 9)
  expect_equal(new$spoils, 2)
  expect_equal(new$games_played, 1)
  expect_true(all(d$disposals >= 0))
  expect_equal(attr(d, "n_players_new_since_earlier"), 1L)
})

test_that("non-cumulative columns are dropped rather than differenced", {
  # 62.1 - 60.4 = 1.7 is not "1.7% efficiency in the games between" -- it is
  # nothing at all, and shipping it would be a plausible-looking wrong number.
  earlier <- .mock_snapshot("A", games = 3, disposals = 30, spoils = 3, disposal_efficiency = 60.4)
  later <- .mock_snapshot("A", games = 4, disposals = 42, spoils = 5, disposal_efficiency = 62.1)

  d <- diff_aflw_season_snapshots(earlier, later, recompute_averages = FALSE)

  expect_false("disposal_efficiency" %in% names(d))
  expect_false("disposals_avg" %in% names(d))
  expect_true("disposal_efficiency" %in% attr(d, "rate_cols_dropped"))
  expect_true("disposals_avg" %in% attr(d, "rate_cols_dropped"))
})

test_that("per-game averages are recomputed from the window, not differenced", {
  earlier <- .mock_snapshot("A", games = 3, disposals = 30, spoils = 3)
  later <- .mock_snapshot("A", games = 5, disposals = 50, spoils = 7)

  d <- diff_aflw_season_snapshots(earlier, later, recompute_averages = TRUE)

  # 20 disposals across the 2 games in the window
  expect_equal(d$disposals_avg, 10)
  expect_equal(d$spoils_avg, 2)
})

test_that("a player with no games in the window gets NA average, not a divide-by-zero", {
  earlier <- .mock_snapshot("A", games = 4, disposals = 40, spoils = 4)
  later <- .mock_snapshot("A", games = 4, disposals = 40, spoils = 4)

  d <- diff_aflw_season_snapshots(earlier, later, recompute_averages = TRUE)

  expect_equal(d$games_played, 0)
  expect_true(is.na(d$disposals_avg))
  expect_false(is.infinite(d$disposals_avg))
})

test_that("a falling cumulative total warns and is left unclamped", {
  # Cumulative totals cannot decrease within a season, so a fall means the
  # source table was revised. Clamping would hide a real upstream correction.
  earlier <- .mock_snapshot("A", games = 4, disposals = 45, spoils = 6)
  later <- .mock_snapshot("A", games = 5, disposals = 42, spoils = 7)

  expect_warning(
    d <- diff_aflw_season_snapshots(earlier, later, recompute_averages = FALSE),
    "negative delta"
  )
  expect_equal(d$disposals, -3)
})

test_that("snapshot dates are carried onto the delta", {
  earlier <- .mock_snapshot("A", games = 3, disposals = 30, spoils = 3, snapshot_date = "2026-08-18")
  later <- .mock_snapshot("A", games = 4, disposals = 42, spoils = 5, snapshot_date = "2026-08-25")

  d <- diff_aflw_season_snapshots(earlier, later, recompute_averages = FALSE)

  expect_equal(d$snapshot_from, "2026-08-18")
  expect_equal(d$snapshot_to, "2026-08-25")
})

test_that("differencing across seasons is refused", {
  # Cumulative totals restart each season; the delta would be this season's
  # running total minus an unrelated one.
  earlier <- .mock_snapshot("A", games = 9, disposals = 90, spoils = 9, season = 2025L)
  later <- .mock_snapshot("A", games = 2, disposals = 20, spoils = 2, season = 2026L)

  expect_error(diff_aflw_season_snapshots(earlier, later), "multiple seasons")
})

test_that("an empty later snapshot is an error, not an empty delta", {
  earlier <- .mock_snapshot("A", games = 3, disposals = 30, spoils = 3)
  later <- earlier[0]

  expect_error(diff_aflw_season_snapshots(earlier, later), "0 rows")
})

test_that("column classification splits identity, cumulative and rate", {
  cls <- .aflw_snapshot_classify_cols(c(
    "player_id", "season", "comp", "team_abbr",
    "disposals", "spoils", "games_played",
    "disposals_avg", "disposal_efficiency", "hitout_win_percentage",
    "contested_possession_rate", "goal_accuracy", "ranking"
  ))

  expect_setequal(cls$id, c("player_id", "season", "comp", "team_abbr"))
  expect_setequal(cls$cumulative, c("disposals", "spoils", "games_played"))
  expect_setequal(cls$rate, c("disposals_avg", "disposal_efficiency", "hitout_win_percentage",
                              "contested_possession_rate", "goal_accuracy", "ranking"))
})

test_that("snapshot file names are the documented shape", {
  expect_equal(
    .aflw_snapshot_file_name(2026, as.Date("2026-08-25")),
    "aflw_season_stats_2026_asof_2026-08-25"
  )
})

test_that("listing snapshots parses season and date, and is empty when the release has none", {
  local_mocked_bindings(get_release_assets = function(...) {
    c("aflw_season_stats_2026_asof_2026-08-25.parquet",
      "aflw_season_stats_2026_asof_2026-08-18.parquet",
      "aflw_season_stats_2025_asof_2025-09-01.parquet",
      "some_unrelated_file.parquet")
  })

  all_s <- list_aflw_season_stat_snapshots()
  expect_equal(nrow(all_s), 3)
  expect_true(all(all_s$snapshot_date == sort(all_s$snapshot_date)))

  s2026 <- list_aflw_season_stat_snapshots(2026)
  expect_equal(nrow(s2026), 2)
  expect_setequal(as.character(s2026$snapshot_date), c("2026-08-18", "2026-08-25"))
})

test_that("listing returns zero rows when the release does not exist yet", {
  local_mocked_bindings(get_release_assets = function(...) NULL)

  out <- list_aflw_season_stat_snapshots(2026)
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 0)
})

test_that("loading without any published snapshot fails with an actionable message", {
  local_mocked_bindings(get_release_assets = function(...) NULL)

  expect_error(
    load_aflw_season_stats_snapshot(2026),
    "no snapshots published"
  )
})
