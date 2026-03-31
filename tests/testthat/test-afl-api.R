# Tests for AFL API Functions
# ===========================

# -----------------------------------------------------------------------------
# Function existence and exports
# -----------------------------------------------------------------------------

test_that("all AFL API functions are exported", {
  expect_true(is.function(get_afl_fixtures))
  expect_true(is.function(get_afl_results))
  expect_true(is.function(get_afl_lineups))
  expect_true(is.function(get_afl_player_stats))
  expect_true(is.function(get_afl_player_details))
  expect_true(is.function(get_afl_ladder))
})

test_that("team/venue name helpers are exported", {
  expect_true(is.function(torp_replace_teams))
  expect_true(is.function(torp_replace_venues))
  expect_true(is.function(torp_team_abbr))
  expect_true(is.function(torp_team_full))
})

# -----------------------------------------------------------------------------
# Function signatures
# -----------------------------------------------------------------------------

test_that("get_afl_fixtures has expected signature", {
  args <- formals(get_afl_fixtures)
  expect_true("season" %in% names(args))
  expect_null(args$season)
})

test_that("get_afl_results has expected signature", {
  args <- formals(get_afl_results)
  expect_true("season" %in% names(args))
  expect_null(args$season)
})

test_that("get_afl_lineups has expected signature", {
  args <- formals(get_afl_lineups)
  expect_true("season" %in% names(args))
  expect_true("round" %in% names(args))
  expect_null(args$season)
  expect_null(args$round)
})

test_that("get_afl_player_stats has expected signature", {
  args <- formals(get_afl_player_stats)
  expect_true("season" %in% names(args))
  expect_null(args$season)
})

test_that("get_afl_player_details has expected signature", {
  args <- formals(get_afl_player_details)
  expect_true("season" %in% names(args))
  expect_null(args$season)
})

test_that("get_afl_ladder has expected signature", {
  args <- formals(get_afl_ladder)
  expect_true("season" %in% names(args))
  expect_null(args$season)
})

# -----------------------------------------------------------------------------
# Internal helpers: .fetch_cfs_batch
# -----------------------------------------------------------------------------

test_that(".fetch_cfs_batch returns empty tibble for empty ids", {
  result <- torp:::.fetch_cfs_batch(
    ids = character(0),
    url_template = "https://example.com/%s",
    token = "fake",
    parse_fn = function(json, id) data.frame(),
    label = "test"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# -----------------------------------------------------------------------------
# Internal helpers: .parse_match_stats
# -----------------------------------------------------------------------------

test_that(".parse_match_stats returns empty tibble for empty input", {
  result <- torp:::.parse_match_stats(list(), "CD_M00000000000")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that(".parse_match_stats handles data.frame team stats", {
  mock_resp <- list(
    homeTeamPlayerStats = data.frame(
      playerId = c("P1", "P2"),
      kicks = c(10, 15),
      stringsAsFactors = FALSE
    ),
    awayTeamPlayerStats = data.frame(
      playerId = c("P3", "P4"),
      kicks = c(8, 12),
      stringsAsFactors = FALSE
    )
  )
  result <- torp:::.parse_match_stats(mock_resp, "CD_M20260140001")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true("teamStatus" %in% names(result))
  expect_true("providerId" %in% names(result))
  expect_equal(unique(result$providerId), "CD_M20260140001")
})

test_that(".parse_match_stats strips playerStats. prefix from columns", {
  mock_resp <- list(
    homeTeamPlayerStats = data.frame(
      playerStats.kicks = c(10),
      playerStats.handballs = c(5),
      stringsAsFactors = FALSE
    ),
    awayTeamPlayerStats = NULL
  )
  result <- torp:::.parse_match_stats(mock_resp, "CD_M20260140001")
  expect_true("kicks" %in% names(result))
  expect_true("handballs" %in% names(result))
  expect_false(any(grepl("^playerStats\\.", names(result))))
})

# -----------------------------------------------------------------------------
# Internal helpers: .parse_squad_json
# -----------------------------------------------------------------------------

test_that(".parse_squad_json returns NULL for empty squad", {
  expect_null(torp:::.parse_squad_json(list(squad = list(players = NULL))))
  expect_null(torp:::.parse_squad_json(list(squad = list(players = data.frame()))))
  expect_null(torp:::.parse_squad_json(list()))
})

test_that(".parse_squad_json extracts players with team info", {
  mock_json <- list(
    squad = list(
      players = data.frame(
        firstName = "Tom",
        surname = "Mitchell",
        stringsAsFactors = FALSE
      ),
      team = list(name = "Hawthorn", providerId = "CD_T90")
    )
  )
  result <- torp:::.parse_squad_json(mock_json)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$team.name, "Hawthorn")
  expect_equal(result$team.providerId, "CD_T90")
})

# -----------------------------------------------------------------------------
# Internal helpers: .post_process_squad_result
# -----------------------------------------------------------------------------

test_that(".post_process_squad_result returns empty tibble for empty input", {
  result <- torp:::.post_process_squad_result(data.frame(), scalar_season = 2025)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# -----------------------------------------------------------------------------
# get_afl_results filtering logic
# -----------------------------------------------------------------------------

test_that("get_afl_results filters to CONCLUDED status", {
  # Mock get_afl_fixtures to return a mix of statuses
  mock_fixtures <- tibble::tibble(
    match_id = paste0("CD_M", 1:5),
    status = c("CONCLUDED", "UPCOMING", "CONCLUDED", "LIVE", "CONCLUDED"),
    home_team_name = rep("Sydney Swans", 5),
    away_team_name = rep("Carlton Blues", 5),
    home_score = c(100, NA, 80, 50, 120),
    away_score = c(90, NA, 70, 40, 110),
    round_number = 1:5
  )

  local_mocked_bindings(
    get_afl_fixtures = function(season = NULL) mock_fixtures
  )

  results <- get_afl_results(2025)
  expect_equal(nrow(results), 3)
  expect_true(all(results$status == "CONCLUDED"))
})

test_that("get_afl_results returns empty tibble when no concluded games", {
  mock_fixtures <- tibble::tibble(
    match_id = paste0("CD_M", 1:2),
    status = c("UPCOMING", "UPCOMING"),
    home_team_name = rep("Sydney Swans", 2),
    away_team_name = rep("Carlton Blues", 2),
    home_score = c(NA, NA),
    away_score = c(NA, NA)
  )

  local_mocked_bindings(
    get_afl_fixtures = function(season = NULL) mock_fixtures
  )

  results <- get_afl_results(2025)
  expect_equal(nrow(results), 0)
})

test_that("get_afl_results returns empty tibble when fixtures are empty", {
  local_mocked_bindings(
    get_afl_fixtures = function(season = NULL) tibble::tibble()
  )

  results <- get_afl_results(2025)
  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 0)
})

test_that("get_afl_results falls back to score-based filter without status column", {
  mock_fixtures <- tibble::tibble(
    match_id = paste0("CD_M", 1:4),
    home_team_name = rep("Sydney Swans", 4),
    away_team_name = rep("Carlton Blues", 4),
    home_score = c(100, 0, 80, NA),
    away_score = c(90, 0, 70, NA)
  )

  local_mocked_bindings(
    get_afl_fixtures = function(season = NULL) mock_fixtures
  )

  results <- suppressMessages(get_afl_results(2025))
  # Score-based fallback: home_score > 0 gives rows 1 (100) and 3 (80)
  expect_equal(nrow(results), 2)
  expect_true(all(results$home_score > 0))
})

# -----------------------------------------------------------------------------
# torp_replace_venues
# -----------------------------------------------------------------------------

test_that("torp_replace_venues maps known sponsor names", {
  expect_equal(torp_replace_venues("Marvel Stadium"), "Docklands")
  expect_equal(torp_replace_venues("Etihad Stadium"), "Docklands")
  expect_equal(torp_replace_venues("Docklands Stadium"), "Docklands")
  expect_equal(torp_replace_venues("MCG"), "M.C.G.")
  expect_equal(torp_replace_venues("Melbourne Cricket Ground"), "M.C.G.")
  expect_equal(torp_replace_venues("SCG"), "S.C.G.")
  expect_equal(torp_replace_venues("Optus Stadium"), "Perth Stadium")
  expect_equal(torp_replace_venues("GMHBA Stadium"), "Kardinia Park")
})

test_that("torp_replace_venues passes through unknown venues", {
  expect_equal(torp_replace_venues("Unknown Stadium"), "Unknown Stadium")
  expect_equal(torp_replace_venues("M.C.G."), "M.C.G.")
})

test_that("torp_replace_venues handles vectors", {
  input <- c("MCG", "Optus Stadium", "Gabba")
  expected <- c("M.C.G.", "Perth Stadium", "Gabba")
  expect_equal(torp_replace_venues(input), expected)
})

# -----------------------------------------------------------------------------
# torp_team_abbr / torp_team_full
# -----------------------------------------------------------------------------

test_that("torp_team_abbr returns correct abbreviations", {
  expect_equal(torp_team_abbr("Sydney Swans"), "SYD")
  expect_equal(torp_team_abbr("Adelaide Crows"), "ADEL")
  expect_equal(torp_team_abbr("Western Bulldogs"), "WB")
})

test_that("torp_team_abbr handles aliases as input", {
  expect_equal(torp_team_abbr("Swans"), "SYD")
  expect_equal(torp_team_abbr("ADEL"), "ADEL")
})

test_that("torp_team_full returns correct full names", {
  expect_equal(torp_team_full("Adelaide Crows"), "Adelaide Crows")
  expect_equal(torp_team_full("ADEL"), "Adelaide Crows")
  expect_equal(torp_team_full("Crows"), "Adelaide Crows")
})

test_that("torp_team_abbr returns NA for unknown teams", {
  expect_true(is.na(torp_team_abbr("Nonexistent FC")))
})

# -----------------------------------------------------------------------------
# .drop_list_cols helper
# -----------------------------------------------------------------------------

test_that(".drop_list_cols removes list columns", {
  df <- data.frame(a = 1:3, b = I(list(1, 2, 3)), c = c("x", "y", "z"))
  result <- torp:::.drop_list_cols(df)
  expect_false("b" %in% names(result))
  expect_true("a" %in% names(result))
  expect_true("c" %in% names(result))
})

test_that(".drop_list_cols preserves df with no list columns", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  result <- torp:::.drop_list_cols(df)
  expect_equal(names(result), c("a", "b"))
  expect_equal(nrow(result), 3)
})

# -----------------------------------------------------------------------------
# Live API tests (skipped without internet)
# -----------------------------------------------------------------------------

test_that("get_afl_fixtures returns data for a known season", {
  skip_if(!curl::has_internet(), "No internet connection")
  skip_on_cran()

  fixtures <- get_afl_fixtures(2025)
  expect_s3_class(fixtures, "tbl_df")
  expect_gt(nrow(fixtures), 0)
  expect_true("match_id" %in% names(fixtures))
  expect_true("round_number" %in% names(fixtures))
})

test_that("get_afl_results returns concluded games", {
  skip_if(!curl::has_internet(), "No internet connection")
  skip_on_cran()

  results <- get_afl_results(2025)
  expect_s3_class(results, "tbl_df")
  if (nrow(results) > 0) {
    expect_true(all(results$status == "CONCLUDED"))
  }
})
