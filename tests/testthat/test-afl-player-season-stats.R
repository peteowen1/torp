test_that("get_afl_player_season_stats has expected signature", {
  args <- formals(get_afl_player_season_stats)
  expect_true("season" %in% names(args))
  expect_true("comp" %in% names(args))
  expect_equal(args$comp, "AFLW")
})

test_that("get_afl_player_season_stats returns empty tibble when no provider id resolves", {
  local_mocked_bindings(
    .afl_all_comp_seasons = function(comp = "AFLM") tibble::tibble(),
    .package = "torp"
  )
  expect_warning(
    result <- get_afl_player_season_stats(2025, comp = "AFLW"),
    "could not resolve"
  )
  expect_equal(nrow(result), 0)
})

test_that("get_afl_player_season_stats returns empty tibble when the target year is absent", {
  local_mocked_bindings(
    .afl_all_comp_seasons = function(comp = "AFLM") {
      tibble::tibble(providerId = c("CD_S2024264", "CD_S2026264"))
    },
    .package = "torp"
  )
  expect_warning(
    result <- get_afl_player_season_stats(2025, comp = "AFLW"),
    "no comp-season provider id"
  )
  expect_equal(nrow(result), 0)
})

test_that("get_afl_player_season_stats flattens totals/averages and renames correctly", {
  # Mirrors the real flattened shape jsonlite::fromJSON(flatten=TRUE) produces
  # for statspro/playersStats/seasons -- players[].totals.* / .averages.* come
  # back as dot-prefixed top-level columns, not nested data.frames.
  mock_json <- list(
    totalResults = 2,
    players = data.frame(
      playerId = c("CD_I1001681", "CD_I9999999"),
      gamesPlayed = c(6, 3),
      "team.teamAbbr" = c("RICH", "STK"),
      "totals.spoils" = c(5, 0),
      "totals.intercepts" = c(18, 2),
      "averages.spoils" = c(0.83, 0),
      "averages.intercepts" = c(3.0, 0.67),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  )
  local_mocked_bindings(
    .afl_all_comp_seasons = function(comp = "AFLM") {
      tibble::tibble(providerId = "CD_S2025264")
    },
    access_api = function(url) mock_json,
    .package = "torp"
  )

  result <- get_afl_player_season_stats(2025, comp = "AFLW")

  expect_equal(nrow(result), 2)
  expect_true(all(c("player_id", "season", "comp", "team_abbr", "games_played",
                     "spoils", "intercepts", "spoils_avg", "intercepts_avg") %in% names(result)))
  expect_equal(result$spoils[result$player_id == "CD_I1001681"], 5)
  expect_equal(result$intercepts_avg[result$player_id == "CD_I1001681"], 3.0)
  expect_equal(result$season, c(2025, 2025))
  expect_equal(result$comp, c("AFLW", "AFLW"))
})

test_that("get_afl_player_season_stats pulls both 2022 AFLW comp seasons (Season 6 + 7)", {
  # 2022 AFLW is split into two comp seasons on the API side -- both must be
  # fetched, not just the first match. CD_S2101264's own first 4 digits
  # ("2101") are NOT the calendar year -- this is exactly the real shape that
  # broke a providerId-regex year extraction; `name` is the reliable source.
  calls <- character(0)
  local_mocked_bindings(
    .afl_all_comp_seasons = function(comp = "AFLM") {
      tibble::tibble(
        providerId = c("CD_S2022264", "CD_S2101264"),
        name = c("2022 NAB AFLW Season 6", "2022 NAB AFLW Season 7")
      )
    },
    access_api = function(url) {
      calls[[length(calls) + 1]] <<- url
      list(totalResults = 1, players = data.frame(
        playerId = "CD_I0000001", gamesPlayed = 1,
        "team.teamAbbr" = "RICH", "totals.spoils" = 1, "averages.spoils" = 1,
        check.names = FALSE, stringsAsFactors = FALSE
      ))
    },
    .package = "torp"
  )
  result <- get_afl_player_season_stats(2022, comp = "AFLW")
  expect_equal(nrow(result), 2)
  expect_true(any(grepl("CD_S2022264", calls)))
  expect_true(any(grepl("CD_S2101264", calls)))
})

test_that("load_aflw_season_stats has expected signature", {
  args <- formals(load_aflw_season_stats)
  expect_true("seasons" %in% names(args))
})

test_that("load_aflw_season_stats(TRUE) resolves to 2018-current, not AFL_MIN_SEASON-current", {
  # AFLW's own history starts 2018; validate_seasons(TRUE) alone would give
  # AFL_MIN_SEASON:current (2021:current) and silently drop 2018-2020.
  captured_seasons <- NULL
  local_mocked_bindings(
    generate_urls = function(data_type, file_prefix, seasons, rounds = NULL, prefer_aggregated = NULL) {
      captured_seasons <<- seasons
      character(0)
    },
    load_from_url = function(url, ...) tibble::tibble(),
    .package = "torp"
  )
  load_aflw_season_stats(seasons = TRUE)
  expect_true(min(captured_seasons) <= 2018)
})
