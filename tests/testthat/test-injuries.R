# --- load_preseason_injuries() ---

test_that("load_preseason_injuries returns empty df for missing season", {
  result <- load_preseason_injuries(1900)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("player", "team", "injury", "estimated_return", "player_norm") %in% names(result)))
})

test_that("load_preseason_injuries loads 2026 data", {
  result <- load_preseason_injuries(2026)
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(all(c("player", "team", "injury", "estimated_return", "player_norm") %in% names(result)))
  # player_norm should be lowercase with no extra whitespace

  expect_true(all(result$player_norm == tolower(result$player_norm)))
})


# --- match_injuries() ---

test_that("match_injuries handles empty injury list", {
  ratings <- data.frame(
    player_name = c("John Smith", "Jane Doe"),
    team = c("TeamA", "TeamB"),
    torp = c(5.0, 3.0),
    stringsAsFactors = FALSE
  )
  result <- match_injuries(ratings, data.frame())
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$injury)))
  expect_true(all(is.na(result$estimated_return)))
})

test_that("match_injuries handles NULL injury list", {
  ratings <- data.frame(
    player_name = c("John Smith"),
    team = c("TeamA"),
    torp = c(5.0),
    stringsAsFactors = FALSE
  )
  result <- match_injuries(ratings, NULL)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$injury))
})

test_that("match_injuries joins correctly via norm_name", {
  ratings <- data.frame(
    player_name = c("John Smith", "Jane Doe", "Bob Brown"),
    team = c("TeamA", "TeamB", "TeamC"),
    torp = c(5.0, 3.0, 2.0),
    stringsAsFactors = FALSE
  )
  injuries <- data.frame(
    player = "John Smith",
    injury = "Knee (ACL)",
    estimated_return = "2027",
    player_norm = "john smith",
    stringsAsFactors = FALSE
  )
  result <- match_injuries(ratings, injuries)
  expect_equal(nrow(result), 3)
  # John Smith should be injured
  john <- result[result$player_name == "John Smith", ]
  expect_equal(john$injury, "Knee (ACL)")
  expect_equal(john$estimated_return, "2027")
  # Others should be NA
  expect_true(is.na(result[result$player_name == "Jane Doe", "injury"]))
})

test_that("match_injuries errors without player_name column", {
  bad_df <- data.frame(name = "test", torp = 1.0)
  inj <- data.frame(
    player_norm = "test", injury = "Knee", estimated_return = "TBC",
    stringsAsFactors = FALSE
  )
  expect_error(match_injuries(bad_df, inj), "player_name")
})


# --- get_all_injuries() deduplication ---

test_that("get_all_injuries deduplicates with weekly taking precedence", {
  # Mock both sources — weekly scrape includes team column so dedup uses

  # (player_norm, team) composite key
  weekly_data <- data.frame(
    player = "John Smith",
    team = "TeamA",
    injury = "Hamstring",
    estimated_return = "Round 5",
    player_norm = "john smith",
    stringsAsFactors = FALSE
  )
  preseason_data <- data.frame(
    player = "John Smith",
    team = "TeamA",
    injury = "Knee (ACL)",
    estimated_return = "Season",
    player_norm = "john smith",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    scrape_injuries = function(timeout = 30) weekly_data,
    load_preseason_injuries = function(season) preseason_data
  )

  result <- get_all_injuries(2026, scrape = TRUE)

  # Should have only 1 row for John Smith at TeamA (weekly takes precedence)
  john_rows <- result[result$player_norm == "john smith", ]
  expect_equal(nrow(john_rows), 1)
  expect_equal(john_rows$source, "weekly")
  expect_equal(john_rows$injury, "Hamstring")
})

test_that("get_all_injuries with scrape=FALSE uses only preseason", {
  preseason_data <- data.frame(
    player = "Jane Doe",
    team = "TeamB",
    injury = "Foot",
    estimated_return = "TBC",
    player_norm = "jane doe",
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    load_preseason_injuries = function(season) preseason_data
  )

  result <- get_all_injuries(2026, scrape = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$source, "preseason")
})

test_that("get_all_injuries returns empty df when no injuries", {
  local_mocked_bindings(
    scrape_injuries = function(timeout = 30) {
      data.frame(player = character(), injury = character(),
                 estimated_return = character(), player_norm = character(),
                 stringsAsFactors = FALSE)
    },
    load_preseason_injuries = function(season) {
      data.frame(player = character(), team = character(),
                 injury = character(), estimated_return = character(),
                 player_norm = character(), stringsAsFactors = FALSE)
    }
  )

  result <- get_all_injuries(2026, scrape = TRUE)
  expect_equal(nrow(result), 0)
  expect_true(all(c("player", "injury", "estimated_return", "player_norm", "source") %in% names(result)))
})


# --- norm_name edge cases for injuries ---

test_that("norm_name handles common injury list name variations", {
  # Accented characters
  expect_equal(norm_name("José López"), "jose lopez")
  # Extra spacing
  expect_equal(norm_name("  John   Smith  "), "john smith")
  # Hyphens (common in AFL names)
  expect_equal(norm_name("Sam Powell-Pepper"), "sam powell pepper")
  # Apostrophes
  expect_equal(norm_name("Tom O'Brien"), "tom o brien")
})


# --- parse_return_round() ---

test_that("parse_return_round handles 'Round N' patterns", {
  expect_equal(parse_return_round("Round 14", 2026), 14)
  expect_equal(parse_return_round("Round 5", 2026), 5)
  expect_equal(parse_return_round("round 1", 2026), 1)
})

test_that("parse_return_round handles 'Round N-M' range (uses upper bound)", {
  expect_equal(parse_return_round("Round 10-12", 2026), 12)
  expect_equal(parse_return_round("Round 3-5", 2026), 5)
})

test_that("parse_return_round handles future year as Inf", {
  expect_equal(parse_return_round("2027", 2026), Inf)
  expect_equal(parse_return_round("2028", 2026), Inf)
})

test_that("parse_return_round handles current year as NA", {
  expect_true(is.na(parse_return_round("2026", 2026)))
})

test_that("parse_return_round handles season timeline phrases", {
  expect_equal(parse_return_round("Mid-season", 2026), SIM_INJURY_SEASON_MID)
  expect_equal(parse_return_round("Mid-to-late season", 2026), SIM_INJURY_SEASON_MID)
  expect_equal(parse_return_round("Late season", 2026), SIM_INJURY_SEASON_LATE)
  expect_equal(parse_return_round("Second half of 2026", 2026), SIM_INJURY_SECOND_HALF)
})

test_that("parse_return_round handles TBC/Indefinite with buffer", {
  expect_equal(parse_return_round("TBC", 2026, current_round = 5), 5 + SIM_INJURY_TBC_BUFFER)
  expect_equal(parse_return_round("Indefinite", 2026, current_round = 10), 10 + SIM_INJURY_TBC_BUFFER)
  expect_equal(parse_return_round("Test", 2026, current_round = 2), 2 + SIM_INJURY_TBC_BUFFER)
})

test_that("parse_return_round handles NA / empty / None", {
  expect_true(is.na(parse_return_round(NA, 2026)))
  expect_true(is.na(parse_return_round("", 2026)))
  expect_true(is.na(parse_return_round("None", 2026)))
})

test_that("parse_return_round is vectorized", {
  result <- parse_return_round(
    c("Round 5", "2027", "TBC", "Mid-season", NA),
    season = 2026, current_round = 3
  )
  expect_length(result, 5)
  expect_equal(result[1], 5)
  expect_equal(result[2], Inf)
  expect_equal(result[3], 3 + SIM_INJURY_TBC_BUFFER)
  expect_equal(result[4], SIM_INJURY_SEASON_MID)
  expect_true(is.na(result[5]))
})

test_that("parse_return_round handles N-M weeks range", {
  # "7-11 weeks" from round 2 -> round 2 + 11 = 13

  expect_equal(parse_return_round("7-11 weeks", 2026, current_round = 2), 13)
  # "1-2 weeks" from round 5 -> round 5 + 2 = 7
  expect_equal(parse_return_round("1-2 weeks", 2026, current_round = 5), 7)
  # "3-4 weeks" from round 1 -> round 1 + 4 = 5
  expect_equal(parse_return_round("3-4 weeks", 2026, current_round = 1), 5)
})

test_that("parse_return_round handles single N weeks", {
  expect_equal(parse_return_round("4 weeks", 2026, current_round = 3), 7)
  expect_equal(parse_return_round("1 week", 2026, current_round = 10), 11)
})

test_that("parse_return_round handles 'Season' as Inf", {
  expect_equal(parse_return_round("Season", 2026, current_round = 4), Inf)
})

test_that("parse_return_round handles N-plus weeks", {
  # "10-plus weeks" -> 10 * 1.5 = 15 weeks -> current_round + 15

  expect_equal(parse_return_round("10-plus weeks", 2026, current_round = 4), 19)
  expect_equal(parse_return_round("6-plus weeks", 2026, current_round = 4), 13)
})

test_that("parse_return_round handles month-based returns", {
  # "2 months" -> 2 * 4 = 8 weeks -> current_round + 8

  expect_equal(parse_return_round("2 months", 2026, current_round = 4), 12)
  expect_equal(parse_return_round("5 months", 2026, current_round = 4), 24)
  # "3-4 months" -> 4 * 4 = 16 weeks -> current_round + 16

  expect_equal(parse_return_round("3-4 months", 2026, current_round = 4), 20)
})

test_that("parse_return_round handles Assess and Individualised program", {
  tbc_val <- 4 + SIM_INJURY_TBC_BUFFER
  expect_equal(parse_return_round("Assess", 2026, current_round = 4), tbc_val)
  expect_equal(
    parse_return_round("Individualised program", 2026, current_round = 4),
    tbc_val
  )
})


# --- build_injury_schedule() ---

test_that("build_injury_schedule returns empty dt for empty input", {
  result <- build_injury_schedule(NULL, data.table::data.table())
  expect_equal(nrow(result), 0)
  expect_true(all(c("team", "torp_boost", "return_round") %in% names(result)))
})

test_that("build_injury_schedule excludes Inf return_round players", {
  injuries <- data.frame(
    player = "John Smith",
    player_norm = "john smith",
    return_round = Inf,
    stringsAsFactors = FALSE
  )
  ratings <- data.table::data.table(
    player_name = "John Smith",
    team = "Sydney",
    torp = 10.0,
    pred_tog = 0.8
  )
  result <- build_injury_schedule(injuries, ratings)
  expect_equal(nrow(result), 0)
})

test_that("build_injury_schedule computes correct boosts", {
  injuries <- data.frame(
    player = c("Player A", "Player B"),
    player_norm = c("player a", "player b"),
    return_round = c(5, 10),
    stringsAsFactors = FALSE
  )
  ratings <- data.table::data.table(
    player_name = c("Player A", "Player B", "Player C"),
    team = c("Sydney", "Sydney", "Sydney"),
    torp = c(10.0, 5.0, 3.0),
    pred_tog = c(0.8, 0.7, 0.6)
  )
  result <- build_injury_schedule(injuries, ratings)

  expect_true(nrow(result) > 0)
  expect_true(all(c("team", "torp_boost", "return_round") %in% names(result)))
  # Both returning players should generate positive boosts
  expect_true(all(result$torp_boost > 0))
  # Should have entries for rounds 5 and 10
  expect_true(5 %in% result$return_round)
  expect_true(10 %in% result$return_round)
})

test_that("build_injury_schedule aggregates same-round returns", {
  injuries <- data.frame(
    player = c("Player A", "Player B"),
    player_norm = c("player a", "player b"),
    return_round = c(5, 5),
    stringsAsFactors = FALSE
  )
  ratings <- data.table::data.table(
    player_name = c("Player A", "Player B", "Player C"),
    team = c("Sydney", "Sydney", "Sydney"),
    torp = c(10.0, 5.0, 3.0),
    pred_tog = c(0.8, 0.7, 0.6)
  )
  result <- build_injury_schedule(injuries, ratings)
  # Both return round 5, should be aggregated into one row per team
  # build_injury_schedule normalizes team names via torp_replace_teams
  sydney_r5 <- result[team == "Sydney Swans" & return_round == 5]
  expect_equal(nrow(sydney_r5), 1)
})
