# Tests for Team Profile Functions
# ================================

# -----------------------------------------------------------------------------
# Function existence and exports
# -----------------------------------------------------------------------------

test_that("team profile functions are exported", {
  expect_true(is.function(team_profile))
  expect_true(is.function(team_stat_rating_profile))
  expect_true(is.function(get_team_stat_ratings))
})

test_that("backward compatibility aliases exist", {
  expect_true(is.function(get_team_skills))
  expect_true(is.function(team_skill_profile))
  expect_identical(get_team_skills, get_team_stat_ratings)
  expect_identical(team_skill_profile, team_stat_rating_profile)
})

# -----------------------------------------------------------------------------
# Function signatures
# -----------------------------------------------------------------------------

test_that("team_profile has expected signature", {
  args <- formals(team_profile)
  expect_true(all(c("team_name", "seasons", "top_n") %in% names(args)))
  expect_equal(args$top_n, 10)
})

test_that("get_team_stat_ratings has expected signature", {
  args <- formals(get_team_stat_ratings)
  expect_true(all(c("team_name", "top_n") %in% names(args)))
  expect_null(args$team_name)
  expect_equal(args$top_n, 22)
})

test_that("team_stat_rating_profile has expected signature", {
  args <- formals(team_stat_rating_profile)
  expect_true(all(c("team_name", "top_n") %in% names(args)))
  expect_equal(args$top_n, 22)
})

# -----------------------------------------------------------------------------
# resolve_team (internal helper)
# -----------------------------------------------------------------------------

test_that("resolve_team resolves canonical names", {
  result <- torp:::resolve_team("Sydney Swans")
  expect_type(result, "list")
  expect_equal(result$name, "Sydney Swans")
  expect_equal(result$abbr, "SYD")
  expect_true(all(c("name", "full", "abbr") %in% names(result)))
})

test_that("resolve_team resolves abbreviations", {
  result <- torp:::resolve_team("ADEL")
  expect_equal(result$name, "Adelaide Crows")
  expect_equal(result$abbr, "ADEL")
})

test_that("resolve_team resolves partial names via fuzzy match", {
  result <- torp:::resolve_team("Sydney")
  expect_equal(result$name, "Sydney Swans")
})

test_that("resolve_team rejects non-character input", {
  expect_error(torp:::resolve_team(123), "non-empty character")
  expect_error(torp:::resolve_team(NULL), "non-empty character")
  expect_error(torp:::resolve_team(c("Sydney", "Carlton")), "non-empty character")
})

test_that("resolve_team rejects empty string", {
  expect_error(torp:::resolve_team(""), "non-empty character")
  expect_error(torp:::resolve_team("   "), "non-empty character")
})

test_that("resolve_team errors on unknown team", {
  expect_error(torp:::resolve_team("Nonexistent FC"), "No team found")
})

test_that("resolve_team errors on ambiguous input", {
  # "e" matches multiple teams (Essendon, Melbourne, etc.)
  expect_error(torp:::resolve_team("e"), "Multiple teams match")
})

# -----------------------------------------------------------------------------
# .compute_season_record (internal helper)
# -----------------------------------------------------------------------------

test_that(".compute_season_record computes correct W/L/D", {
  results_dt <- data.table::data.table(
    season = rep(2025, 4),
    home_team_name = c("Sydney Swans", "Sydney Swans", "Carlton Blues", "Carlton Blues"),
    away_team_name = c("Carlton Blues", "Melbourne Demons", "Sydney Swans", "Melbourne Demons"),
    home_score = c(100, 80, 90, 70),
    away_score = c(90, 80, 100, 60)
  )

  record <- torp:::.compute_season_record(results_dt, "Sydney Swans")
  expect_s3_class(record, "data.table")
  expect_equal(nrow(record), 1)
  expect_equal(record$season, 2025)
  # Sydney: home W (100>90), home D (80=80), away W (90<100 is loss for away? No: away_score=100 > home_score=90)
  # Game 1: Sydney home, 100-90 => W
  # Game 2: Sydney home, 80-80 => D
  # Game 3: Sydney away, away_score=100 > home_score=90 => W
  expect_equal(record$wins, 2)
  expect_equal(record$draws, 1)
  expect_equal(record$losses, 0)
  expect_equal(record$games, 3)
  expect_equal(record$points_for, 100 + 80 + 100)
  expect_equal(record$points_against, 90 + 80 + 90)
})

test_that(".compute_season_record returns empty for missing columns", {
  bad_dt <- data.table::data.table(
    season = 2025,
    foo = "bar"
  )
  result <- torp:::.compute_season_record(bad_dt, "Sydney Swans")
  expect_equal(nrow(result), 0)
})

test_that(".compute_season_record returns empty for team with no games", {
  results_dt <- data.table::data.table(
    season = 2025,
    home_team_name = "Carlton Blues",
    away_team_name = "Melbourne Demons",
    home_score = 100,
    away_score = 90
  )
  result <- torp:::.compute_season_record(results_dt, "Sydney Swans")
  expect_equal(nrow(result), 0)
})

test_that(".compute_season_record handles multiple seasons", {
  results_dt <- data.table::data.table(
    season = c(2024, 2024, 2025, 2025),
    home_team_name = c("Sydney Swans", "Carlton Blues", "Sydney Swans", "Sydney Swans"),
    away_team_name = c("Carlton Blues", "Sydney Swans", "Carlton Blues", "Melbourne Demons"),
    home_score = c(100, 90, 80, 120),
    away_score = c(90, 100, 70, 60)
  )
  record <- torp:::.compute_season_record(results_dt, "Sydney Swans")
  expect_equal(nrow(record), 2)
  expect_equal(record$season, c(2024, 2025))
})

test_that(".compute_season_record computes percentage correctly", {
  results_dt <- data.table::data.table(
    season = c(2025, 2025),
    home_team_name = c("Sydney Swans", "Carlton"),
    away_team_name = c("Carlton", "Sydney Swans"),
    home_score = c(100, 70),
    away_score = c(80, 90)
  )
  record <- torp:::.compute_season_record(results_dt, "Sydney Swans")
  # Sydney: home W 100-80, away W 90-70; total PF=190, PA=150
  expect_equal(record$percentage, round(190 / 150 * 100, 1))
})

# -----------------------------------------------------------------------------
# match_local_time (internal helper)
# -----------------------------------------------------------------------------

test_that("match_local_time converts UTC to Melbourne time", {
  result <- torp:::match_local_time("2026-03-05T08:30:00.000+0000", "Australia/Melbourne")
  expect_type(result, "character")
  expect_true(grepl("2026-03-05", result))
})

test_that("match_local_time defaults to Melbourne when timezone is NULL", {
  result_null <- torp:::match_local_time("2026-03-05T08:30:00.000+0000", NULL)
  result_melb <- torp:::match_local_time("2026-03-05T08:30:00.000+0000", "Australia/Melbourne")
  expect_equal(result_null, result_melb)
})

test_that("match_local_time returns NA for NA input", {
  expect_true(is.na(torp:::match_local_time(NA, "Australia/Sydney")))
  expect_true(is.na(torp:::match_local_time(NULL, "Australia/Sydney")))
})

test_that("match_local_time returns NA for unparseable input", {
  expect_true(is.na(torp:::match_local_time("not-a-date", "Australia/Sydney")))
})

test_that("match_local_time handles Perth timezone", {
  result <- torp:::match_local_time("2026-03-05T08:30:00.000+0000", "Australia/Perth")
  expect_type(result, "character")
  # Perth is UTC+8, so 08:30 UTC = 16:30 AWST
  expect_true(grepl("16:30", result))
})

# -----------------------------------------------------------------------------
# team_profile structure (mocked data loading)
# -----------------------------------------------------------------------------

test_that("team_profile returns correct class and structure", {
  local_mocked_bindings(
    load_team_ratings = function(...) data.frame(),
    load_results = function(...) data.frame(),
    load_torp_ratings = function(...) data.frame(),
    load_player_game_ratings = function(...) data.frame()
  )

  result <- team_profile("Sydney Swans")
  expect_s3_class(result, "torp_team_profile")
  expect_true(all(c("team_info", "team_rating", "season_record", "top_players", "psv_season") %in% names(result)))
  expect_equal(result$team_info$name, "Sydney Swans")
  expect_equal(result$team_info$abbr, "SYD")
})

test_that("team_profile accepts abbreviations", {
  local_mocked_bindings(
    load_team_ratings = function(...) data.frame(),
    load_results = function(...) data.frame(),
    load_torp_ratings = function(...) data.frame(),
    load_player_game_ratings = function(...) data.frame()
  )

  result <- team_profile("CARL")
  expect_s3_class(result, "torp_team_profile")
  expect_equal(result$team_info$name, "Carlton Blues")
})

test_that("team_profile errors on bad team name", {
  expect_error(team_profile("Nonexistent FC"), "No team found")
  expect_error(team_profile(123), "non-empty character")
  expect_error(team_profile(""), "non-empty character")
})

# -----------------------------------------------------------------------------
# print methods
# -----------------------------------------------------------------------------

test_that("print.torp_team_profile produces output", {
  profile <- structure(
    list(
      team_info = data.frame(name = "Sydney Swans", full = "Sydney Swans", abbr = "SYD"),
      team_rating = data.frame(),
      season_record = data.frame(),
      top_players = data.frame(),
      psv_season = data.table::data.table()
    ),
    class = "torp_team_profile"
  )

  output <- capture.output(print(profile))
  expect_true(any(grepl("Sydney Swans", output)))
  expect_true(any(grepl("SYD", output)))
})

test_that("print.torp_team_stat_rating_profile produces output", {
  profile <- structure(
    list(
      team_info = data.frame(name = "Geelong Cats", full = "Geelong Cats", abbr = "GEEL"),
      stat_ratings = data.frame(
        category = "disposal",
        stat = "kick",
        team_mean = 0.5,
        team_sum = 11.0,
        league_avg = 0.45,
        league_pct = 72.0
      ),
      n_players = 22L,
      lineup_info = NULL
    ),
    class = "torp_team_stat_rating_profile"
  )

  output <- capture.output(print(profile))
  expect_true(any(grepl("Geelong Cats", output)))
  expect_true(any(grepl("GEEL", output)))
  expect_true(any(grepl("22", output)))
})

# -----------------------------------------------------------------------------
# .get_latest_lineup_ids (internal helper)
# -----------------------------------------------------------------------------

test_that(".get_latest_lineup_ids returns NULL when load_teams fails", {
  local_mocked_bindings(
    load_teams = function(...) stop("no data")
  )
  result <- torp:::.get_latest_lineup_ids()
  expect_null(result)
})

test_that(".get_latest_lineup_ids returns NULL for empty lineups", {
  local_mocked_bindings(
    load_teams = function(...) data.frame()
  )
  result <- torp:::.get_latest_lineup_ids()
  expect_null(result)
})

# -----------------------------------------------------------------------------
# .resolve_lineup_info (internal helper)
# -----------------------------------------------------------------------------

test_that(".resolve_lineup_info returns NULL when no match_info attribute", {
  dt <- data.table::data.table(team = "Sydney Swans", n_players = 22)
  result <- torp:::.resolve_lineup_info(dt, "Sydney Swans")
  expect_null(result)
})
