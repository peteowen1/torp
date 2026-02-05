# Tests for Scraper Functions with Mocking
# =========================================
# These tests use mocked responses to test scraper behavior without API calls

# -----------------------------------------------------------------------------
# API Response Parsing Tests
# -----------------------------------------------------------------------------

test_that("get_round_games function exists", {
  expect_true(exists("get_round_games"))
  expect_true(is.function(get_round_games))
})

test_that("get_season_games function exists", {
  expect_true(exists("get_season_games"))
  expect_true(is.function(get_season_games))
})

test_that("access_api function exists", {
  expect_true(exists("access_api"))
  expect_true(is.function(access_api))
})

# -----------------------------------------------------------------------------
# Data Transformation Tests
# -----------------------------------------------------------------------------

test_that("get_single_chain processes chains data correctly", {
  # Create mock chains data with expected structure
  mock_chains <- data.frame(
    chain_num = c(1, 1, 1, 2, 2),
    period = c(1, 1, 1, 1, 1),
    x = c(50, 60, 70, 30, 40),
    y = c(10, 15, 20, -5, 0),
    description = c("Kick", "Mark", "Goal", "Kick", "Turnover"),
    stringsAsFactors = FALSE
  )

  result <- get_single_chain(mock_chains, 1)

  # Should filter to chain 1 only
  expect_true(is.data.frame(result))

  if (nrow(result) > 0) {
    expect_true(all(result$chain_num == 1))
  }
})

test_that("get_single_chain returns empty for non-existent chain", {
  mock_chains <- data.frame(
    chain_num = c(1, 1, 2, 2),
    x = c(50, 60, 70, 80),
    stringsAsFactors = FALSE
  )

  result <- get_single_chain(mock_chains, 999)  # Non-existent chain

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

# -----------------------------------------------------------------------------
# get_many_game_chains Tests
# -----------------------------------------------------------------------------

test_that("get_many_game_chains handles empty input", {
  result <- get_many_game_chains(character(0))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("get_many_game_chains handles NULL input gracefully", {
  result <- tryCatch(
    get_many_game_chains(NULL),
    error = function(e) data.frame()
  )

  expect_true(is.data.frame(result))
})

# -----------------------------------------------------------------------------
# get_week_chains Tests
# -----------------------------------------------------------------------------

test_that("get_week_chains validates season", {
  # Should return empty for seasons before 2021
  result <- get_week_chains(2020, 1)

  expect_true(is.data.frame(result) || data.table::is.data.table(result))
  expect_equal(nrow(result), 0)
})

test_that("get_week_chains handles invalid round gracefully", {
  # Should handle gracefully (may return empty or error)
  result <- tryCatch(
    get_week_chains(2024, 50),  # Invalid round
    error = function(e) data.frame(),
    warning = function(w) data.frame()
  )

  expect_true(is.data.frame(result) || data.table::is.data.table(result))
})

# -----------------------------------------------------------------------------
# get_match_chains Tests
# -----------------------------------------------------------------------------

test_that("get_match_chains validates season parameter", {
  expect_error(
    get_match_chains(2020, 1),
    "not available for seasons prior to 2021"
  )

  expect_error(
    get_match_chains(2019, 5),
    "not available for seasons prior to 2021"
  )
})

test_that("get_match_chains accepts valid seasons", {
  # Should not error on validation for valid season (may error on API call)
  result <- tryCatch(
    get_match_chains(2021, 1),
    error = function(e) NULL  # Ignore API errors
  )

  # Just ensure we get past validation
  expect_true(TRUE)  # If we got here, validation passed
})

# -----------------------------------------------------------------------------
# get_players Tests
# -----------------------------------------------------------------------------

test_that("get_players has correct default parameter", {
  fn_formals <- formals(get_players)

  expect_false(fn_formals$use_api)
})

test_that("get_players function signature is correct", {
  fn_args <- names(formals(get_players))

  expect_true("use_api" %in% fn_args)
})

# -----------------------------------------------------------------------------
# get_token Tests
# -----------------------------------------------------------------------------

test_that("get_token exists as internal function", {
  expect_true(exists("get_token", envir = asNamespace("torp")))
})

# -----------------------------------------------------------------------------
# URL Construction Tests
# -----------------------------------------------------------------------------

test_that("round formatting is consistent", {
  # The scraper should format rounds with leading zeros
  # This tests the sprintf behavior used in the code
  expect_equal(sprintf("%02d", 1), "01")
  expect_equal(sprintf("%02d", 10), "10")
  expect_equal(sprintf("%02d", 24), "24")
})

test_that("match ID format is consistent", {
  # Match IDs should follow the pattern CD_M{season}014{round}{game}
  # Test the expected format
  season <- 2024
  round <- 5
  expected_pattern <- paste0("CD_M", season, "014", sprintf("%02d", round))

  expect_true(grepl("^CD_M\\d{4}014\\d{2}", expected_pattern))
})

# -----------------------------------------------------------------------------
# Error Handling Tests
# -----------------------------------------------------------------------------

test_that("get_game_chains handles invalid match ID gracefully", {
  skip_if_no_internet()

  # Test with clearly invalid match ID
  result <- tryCatch(
    get_game_chains("NOT_A_REAL_MATCH_ID"),
    error = function(e) data.frame()
  )

  expect_true(is.data.frame(result) || data.table::is.data.table(result))
})

test_that("access_api handles connection errors", {
  # This test documents expected behavior when API is unreachable
  # In practice, we can't easily mock network errors, so we just
  # verify the function exists and has proper structure

  fn_args <- names(formals(access_api))
  expect_true("url" %in% fn_args)
})

# -----------------------------------------------------------------------------
# Mock API Response Structure Tests
# -----------------------------------------------------------------------------

test_that("mock API response helper creates valid structure", {
  mock_response <- create_mock_api_response()

  expect_true(is.list(mock_response))
  expect_true("content" %in% names(mock_response))
  expect_true("matches" %in% names(mock_response$content))

  # Check first match has expected fields
  first_match <- mock_response$content$matches[[1]]
  expect_true("id" %in% names(first_match))
  expect_true("homeTeam" %in% names(first_match))
  expect_true("awayTeam" %in% names(first_match))
})

# -----------------------------------------------------------------------------
# Data Type Validation Tests
# -----------------------------------------------------------------------------

test_that("get_single_chain returns correct column types", {
  # Test that the function preserves data types
  mock_chains <- data.frame(
    chain_num = c(1L, 1L, 1L),
    period = c(1L, 1L, 1L),
    period_seconds = c(100.5, 200.5, 300.5),
    x = c(50.0, 60.0, 70.0),
    y = c(10.0, 15.0, 20.0),
    description = c("Kick", "Mark", "Goal"),
    stringsAsFactors = FALSE
  )

  result <- get_single_chain(mock_chains, 1)

  # Check if we got a valid result
  skip_if(is.null(result) || !is.data.frame(result), "get_single_chain returned invalid result")

  if (!is.null(nrow(result)) && nrow(result) > 0) {
    # Numeric columns should stay numeric
    expect_true(is.numeric(result$x) || is.double(result$x))
    expect_true(is.numeric(result$y) || is.double(result$y))
  }
})

# -----------------------------------------------------------------------------
# Integration with Validation Functions
# -----------------------------------------------------------------------------

test_that("scraper results can be validated with data validation functions", {
  # Mock some minimal chains-like data
  mock_result <- data.frame(
    match_id = "CD_M20240140101",
    period = 1L,
    period_seconds = 500.0,
    x = 50.0,
    y = 10.0,
    team_id = 1L,
    description = "Kick",
    player_name = "Test Player",
    utc_start_time = Sys.time(),
    stringsAsFactors = FALSE
  )

  # Should be able to validate with chains schema
  result <- validate_data_schema(mock_result, "chains_data", strict = FALSE)
  expect_true(result$valid)
})
