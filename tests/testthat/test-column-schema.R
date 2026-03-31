# Tests for R/column_schema.R
# Column normalisation is foundational — a bug here silently corrupts all data.

test_that(".normalise_columns renames variant to canonical", {
  dt <- data.table::data.table(utcStartTime = "2024-01-01T12:00", foo = 1L)
  col_map <- c("utcStartTime" = "utc_start_time")

  torp:::.normalise_columns(dt, col_map)

  expect_true("utc_start_time" %in% names(dt))
  expect_false("utcStartTime" %in% names(dt))
  expect_equal(dt$utc_start_time, "2024-01-01T12:00")
})

test_that(".normalise_columns skips rename when canonical already exists", {
  dt <- data.table::data.table(utcStartTime = "old", utc_start_time = "existing")
  col_map <- c("utcStartTime" = "utc_start_time")

  torp:::.normalise_columns(dt, col_map)

  # Canonical was already present with real data — don't overwrite

  expect_equal(dt$utc_start_time, "existing")
  expect_true("utcStartTime" %in% names(dt))
})

test_that(".normalise_columns replaces all-NA canonical with real source data", {
  dt <- data.table::data.table(utcStartTime = "2024-01-01T12:00", utc_start_time = NA_character_)
  col_map <- c("utcStartTime" = "utc_start_time")

  torp:::.normalise_columns(dt, col_map)

  expect_equal(dt$utc_start_time, "2024-01-01T12:00")
  expect_false("utcStartTime" %in% names(dt))
})

test_that(".normalise_columns works on plain data.frames via setnames", {
  df <- data.frame(utcStartTime = "2024-01-01T12:00", stringsAsFactors = FALSE)
  col_map <- c("utcStartTime" = "utc_start_time")

  torp:::.normalise_columns(df, col_map)

  # setnames modifies data.frames by reference too
  expect_true("utc_start_time" %in% names(df))
})

test_that(".normalise_columns handles NULL and empty input gracefully", {
  expect_invisible(torp:::.normalise_columns(NULL, c("a" = "b")))
  expect_invisible(torp:::.normalise_columns(data.table::data.table(), c("a" = "b")))
})

test_that(".normalise_columns handles multiple renames in one pass", {
  dt <- data.table::data.table(
    homeTeamScore = 100L,
    awayTeamScore = 85L,
    matchId = "CD_M123"
  )
  col_map <- c(
    "homeTeamScore" = "home_score",
    "awayTeamScore" = "away_score",
    "matchId" = "match_id"
  )

  torp:::.normalise_columns(dt, col_map)

  expect_equal(names(dt), c("home_score", "away_score", "match_id"))
  expect_equal(dt$home_score, 100L)
})

test_that(".to_snake_case converts common patterns", {
  expect_equal(torp:::.to_snake_case("utcStartTime"), "utc_start_time")
  expect_equal(torp:::.to_snake_case("homeTeamScore.totalScore"), "home_team_score_total_score")
  expect_equal(torp:::.to_snake_case("already_snake"), "already_snake")
  expect_equal(torp:::.to_snake_case("ALLCAPS"), "allcaps")
  expect_equal(torp:::.to_snake_case("XMLParser"), "xmlparser")
})

test_that(".bulk_snake_case converts remaining camelCase columns", {
  dt <- data.table::data.table(playerName = "Test", round_number = 1L)

  torp:::.bulk_snake_case(dt, verbose = FALSE)

  expect_true("player_name" %in% names(dt))
  expect_true("round_number" %in% names(dt))
})

test_that("FIXTURE_COL_MAP contains expected mappings", {
  expect_true("utcStartTime" %in% names(torp:::FIXTURE_COL_MAP))
  expect_equal(unname(torp:::FIXTURE_COL_MAP["utcStartTime"]), "utc_start_time")
})

test_that("PBP_COL_MAP contains expected mappings", {
  expect_true("home_score_total_score" %in% names(torp:::PBP_COL_MAP))
  expect_equal(unname(torp:::PBP_COL_MAP["home_score_total_score"]), "home_score")
})

test_that("PLAYER_STATS_COL_MAP handles nested API variants", {
  # The most deeply nested variant should resolve to player_id
  expect_equal(
    unname(torp:::PLAYER_STATS_COL_MAP["player_player_player_player_id"]),
    "player_id"
  )
})
