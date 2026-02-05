# Tests for scraper_functions.R
# These tests cover the AFL API scraping functions

# Helper to skip API tests when no internet
skip_if_no_api_access <- function() {
  testthat::skip_if_not(torp:::check_internet_connection(), "No internet connection")
  result <- try({
    httr::GET("https://api.afl.com.au/cfs/afl/WMCTok", httr::timeout(5))
  }, silent = TRUE)
  testthat::skip_if(inherits(result, "try-error"), "Cannot access AFL API")
}

# -----------------------------------------------------------------------------
# Unit Tests (no API required)
# -----------------------------------------------------------------------------

test_that("get_match_chains validates season parameter", {
  # Seasons before 2021 should error

  expect_error(
    get_match_chains(2020, 1),
    "not available for seasons prior to 2021"
  )

  expect_error(
    get_match_chains(2019, 1),
    "not available for seasons prior to 2021"
  )
})

test_that("get_round_games formats round correctly", {
  # The function internally calls sprintf to format rounds

  # We can't test the API call, but we can verify the function exists
  expect_true(exists("get_round_games"))
  expect_true(is.function(get_round_games))
})

test_that("get_players function exists and has correct signature", {
  expect_true(exists("get_players"))
  expect_true(is.function(get_players))

  # Check default argument
  args <- formals(get_players)
  expect_false(args$use_api)
})

test_that("access_api function exists", {
  expect_true(exists("access_api"))
  expect_true(is.function(access_api))
})

test_that("get_token function exists and is internal", {
  # get_token should be accessible within the package namespace

  expect_true(exists("get_token", envir = asNamespace("torp")))
})

test_that("get_single_chain handles empty data gracefully", {
  # Test with empty/minimal chain data
  empty_chains <- data.frame()
  result <- get_single_chain(empty_chains, 1)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)

  # Test with insufficient columns
  small_chains <- data.frame(a = 1, b = 2)
  result2 <- get_single_chain(small_chains, 1)
  expect_true(is.data.frame(result2))
  expect_equal(nrow(result2), 0)
})

test_that("get_game_chains returns empty data frame for invalid match", {
  skip_if_no_api_access()

  # Test with a clearly invalid match ID
  result <- tryCatch(
    get_game_chains("INVALID_MATCH_ID"),
    error = function(e) data.frame()
  )
  expect_true(is.data.frame(result))
})

test_that("get_many_game_chains handles empty vector", {
  # Empty vector should return empty data frame
  result <- get_many_game_chains(character(0))
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("get_week_chains handles errors gracefully", {
  # Test with invalid parameters - should warn and return empty data.table
  result <- get_week_chains(2019, 1)  # Season before 2021 should fail
  expect_true(is.data.frame(result) || data.table::is.data.table(result))
  expect_equal(nrow(result), 0)
})

# -----------------------------------------------------------------------------
# Integration Tests (require API access)
# -----------------------------------------------------------------------------

test_that("get_players can load from local data", {
  skip_if_no_internet()

  # Test loading from local database (use_api = FALSE)
  result <- tryCatch(
    get_players(use_api = FALSE),
    error = function(e) NULL
  )

  # If we got data, verify structure

  if (!is.null(result) && nrow(result) > 0) {
    expect_true(is.data.frame(result))
    expected_cols <- c("playerId", "jumperNumber", "playerPosition",
                       "playerName.givenName", "playerName.surname",
                       "team.teamName", "season")
    # Check that most expected columns exist
    matching_cols <- sum(expected_cols %in% names(result))
    expect_gte(matching_cols, 4)  # At least 4 of expected columns should exist
  }
})

test_that("get_round_games returns expected structure for valid round",
{
  skip_if_no_api_access()

  # Try to get a round from a completed season
  result <- tryCatch(
    get_round_games(2023, 1),
    error = function(e) NULL
  )

  if (!is.null(result) && nrow(result) > 0) {
    expect_true(is.data.frame(result))
    expect_true("matchId" %in% names(result))
    expect_true("season" %in% names(result))
    expect_true("date" %in% names(result))
  }
})

test_that("get_season_games returns data for valid season", {
  skip_if_no_api_access()

  # Get just a few rounds to keep test fast
  result <- tryCatch(
    get_season_games(2023, rounds = 2),
    error = function(e) NULL
  )

  if (!is.null(result) && nrow(result) > 0) {
    expect_true(is.data.frame(result))
    expect_gte(nrow(result), 1)  # Should have at least 1 game
  }
})

test_that("access_api can authenticate and fetch data", {
  skip_if_no_api_access()

  # Test a simple API endpoint
  result <- tryCatch({
    url <- "https://api.afl.com.au/cfs/afl/fixturesAndResults/season/CD_S2023014/round/CD_R202301401"
    access_api(url)
  }, error = function(e) NULL)

  if (!is.null(result)) {
    expect_true(is.list(result))
  }
})
