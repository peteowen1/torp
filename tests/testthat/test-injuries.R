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
