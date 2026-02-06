# -----------------------------------------------------------------------------
# player_game_ratings Tests
# -----------------------------------------------------------------------------

test_that("player_game_ratings function exists and is exported", {
  expect_true(exists("player_game_ratings"))
  expect_true("player_game_ratings" %in% getNamespaceExports("torp"))
})

test_that("player_game_ratings has correct function signature", {
  fn_args <- names(formals(player_game_ratings))
  expect_true("season_val" %in% fn_args)
  expect_true("round_num" %in% fn_args)
  expect_true("matchid" %in% fn_args)
  expect_true("team" %in% fn_args)
  expect_true("player_game_data" %in% fn_args)
})

test_that("player_game_ratings validates input", {
  expect_error(player_game_ratings(season_val = "invalid"), "season_val must be numeric")
  expect_error(player_game_ratings(round_num = "invalid"), "round_num must be numeric")
  expect_error(player_game_ratings(season_val = 1900), "1990 and 2030")
  expect_error(player_game_ratings(round_num = 50), "0 and 28")
})

# -----------------------------------------------------------------------------
# player_season_ratings Tests
# -----------------------------------------------------------------------------

test_that("player_season_ratings function exists and is exported", {
  expect_true(exists("player_season_ratings"))
  expect_true("player_season_ratings" %in% getNamespaceExports("torp"))
})

test_that("player_season_ratings validates input", {
  expect_error(player_season_ratings(season_val = "invalid"), "season_val must be numeric")
  expect_error(player_season_ratings(season_val = 1900), "1990 and 2030")
  expect_error(player_season_ratings(round_num = "invalid"), "round_num must be numeric")
  expect_error(player_season_ratings(round_num = 50), "0 and 28")
})

# -----------------------------------------------------------------------------
# get_season_data Tests
# -----------------------------------------------------------------------------

test_that("get_season_data uses valid round range (0:28)", {
  # Verify the function exists

  expect_true(exists("get_season_data", envir = asNamespace("torp")))

  # Patch player_game_ratings to capture the round_range argument
  captured_rounds <- NULL
  with_mocked_bindings(
    player_game_ratings = function(season_val, round_num, ...) {
      captured_rounds <<- round_num
      data.frame()  # Return empty df
    },
    {
      tryCatch(
        torp:::get_season_data(2023, NA),
        error = function(e) NULL
      )
    }
  )

  # Round range should be 0:28, not 0:99
  if (!is.null(captured_rounds)) {
    expect_true(all(captured_rounds <= 28),
                info = "get_season_data should not generate rounds > 28")
  }
})

# -----------------------------------------------------------------------------
# filter_game_data Tests
# -----------------------------------------------------------------------------

test_that("filter_game_data helper function works", {
  # Create test data
  test_df <- data.frame(
    season = c(2024, 2024, 2023, 2024),
    round = c(1, 2, 1, 1),
    match_id = c("M1", "M2", "M3", "M4"),
    tm = c("Adelaide Crows", "Brisbane Lions", "Carlton", "Adelaide Crows"),
    opp = c("Brisbane Lions", "Adelaide Crows", "Essendon", "Carlton"),
    tot_p_adj = c(100, 120, 80, 90),
    stringsAsFactors = FALSE
  )

  # Test filtering by match ID
  result1 <- torp:::filter_game_data(test_df, 2024, 1, "M1", NULL)
  expect_equal(nrow(result1), 1)
  expect_equal(result1$match_id, "M1")

  # Test filtering by team
  result2 <- torp:::filter_game_data(test_df, 2024, 1, NULL, "Adelaide Crows")
  expect_equal(nrow(result2), 2)
  expect_true(all(result2$tm == "Adelaide Crows" | result2$opp == "Adelaide Crows"))

  # Test filtering by season and round
  result3 <- torp:::filter_game_data(test_df, 2024, 1, NULL, NULL)
  expect_equal(nrow(result3), 2)
  expect_true(all(result3$season == 2024 & result3$round == 1))

  # Test error handling
  expect_error(torp:::filter_game_data(test_df, 2024, 1, "NONEXISTENT", NULL), "Match ID not found")
  expect_error(torp:::filter_game_data(test_df, 2024, 1, NULL, "NONEXISTENT"), "Team not found")
})

# -----------------------------------------------------------------------------
# resolve_player Tests
# -----------------------------------------------------------------------------

test_that("resolve_player exists as internal function", {
  expect_true(exists("resolve_player", envir = asNamespace("torp")))
})

test_that("resolve_player validates input", {
  expect_error(torp:::resolve_player(123), "non-empty character string")
  expect_error(torp:::resolve_player(""), "non-empty character string")
  expect_error(torp:::resolve_player(c("a", "b")), "non-empty character string")
})

test_that("resolve_player errors on non-existent player", {
  skip_if_no_internet()

  expect_error(
    torp:::resolve_player("Zzzyyyxxx Nonexistent Player 999"),
    "No player found"
  )
})

test_that("resolve_player returns correct structure for known player", {
  skip_if_no_internet()

  result <- tryCatch(
    torp:::resolve_player("Isaac Heeney"),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not resolve player (data unavailable)")

  expect_type(result, "list")
  expect_true("player_id" %in% names(result))
  expect_true("player_name" %in% names(result))
  expect_true("team" %in% names(result))
  expect_true("position" %in% names(result))
  expect_true(grepl("Heeney", result$player_name))
})

test_that("resolve_player works with partial name", {
  skip_if_no_internet()

  result <- tryCatch(
    torp:::resolve_player("Heeney"),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not resolve player (data unavailable)")
  expect_true(grepl("Heeney", result$player_name))
})

# -----------------------------------------------------------------------------
# player_profile Tests
# -----------------------------------------------------------------------------

test_that("player_profile function exists and is exported", {
  expect_true(exists("player_profile"))
  expect_true("player_profile" %in% getNamespaceExports("torp"))
})

test_that("player_profile has correct function signature", {
  fn_args <- names(formals(player_profile))
  expect_true("player_name" %in% fn_args)
  expect_true("seasons" %in% fn_args)
})

test_that("player_profile validates input", {
  expect_error(player_profile(123), "non-empty character string")
  expect_error(player_profile(""), "non-empty character string")
})

test_that("player_profile returns correct class", {
  skip_if_no_internet()

  result <- tryCatch(
    player_profile("Isaac Heeney", seasons = 2024),
    error = function(e) NULL
  )

  skip_if(is.null(result), "Could not load player profile (data unavailable)")

  expect_s3_class(result, "torp_player_profile")
  expect_true("player_info" %in% names(result))
  expect_true("yearly_stats" %in% names(result))
  expect_true("torp_season" %in% names(result))
  expect_true("current_torp" %in% names(result))

  # player_info should be a 1-row data.frame
  expect_equal(nrow(result$player_info), 1)
  expect_true("player_id" %in% names(result$player_info))
  expect_true("name" %in% names(result$player_info))
})

# -----------------------------------------------------------------------------
# print.torp_player_profile Tests
# -----------------------------------------------------------------------------

test_that("print.torp_player_profile method exists", {
  expect_true(exists("print.torp_player_profile", envir = asNamespace("torp")))
})

test_that("print.torp_player_profile produces output", {
  profile <- structure(
    list(
      player_info = data.frame(
        player_id = "CD_I123", name = "Test Player",
        team = "Test Team", position = "Midfielder",
        stringsAsFactors = FALSE
      ),
      yearly_stats = data.frame(season = 2024, games = 10),
      torp_season = data.frame(season = 2024, season_points = 100),
      current_torp = data.frame(torp = 5.5)
    ),
    class = "torp_player_profile"
  )

  output <- capture.output(print(profile))
  expect_true(any(grepl("Test Player", output)))
  expect_true(any(grepl("Yearly Stats", output)))
  expect_true(any(grepl("TORP Season", output)))
  expect_true(any(grepl("Current TORP", output)))
})
