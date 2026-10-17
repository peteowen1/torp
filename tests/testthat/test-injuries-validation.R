# Listing-accuracy validation helpers (R/injuries_validation.R)

# Rounds 1-3 kick off on 2025-03-20/27/04-03 (all in the past).
.iv_fixtures <- function() {
  data.frame(
    round_number = 1:3,
    utc_start_time = c("2025-03-20T09:00", "2025-03-27T09:00", "2025-04-03T09:00")
  )
}

.iv_lineup <- function(rows) {
  rows$lineup_position <- "FB"
  rows
}

.iv_inj <- function(player, team, scraped_at, est) {
  data.frame(
    player = player, team = team, injury = "Hamstring",
    estimated_return = est, source = "weekly",
    scraped_at = as.POSIXct(scraped_at, tz = "UTC"),
    player_norm = norm_name(player),
    stringsAsFactors = FALSE
  )
}

.iv_mock <- function(inj, teams, env = parent.frame()) {
  testthat::local_mocked_bindings(
    load_injury_data = function(...) inj,
    load_fixtures = function(...) .iv_fixtures(),
    load_teams = function(...) teams,
    .env = env
  )
}

test_that("latest listing supersedes an earlier TBC listing", {
  inj <- rbind(
    .iv_inj("Joe Bloggs", "Carlton Blues", "2025-03-22 00:00:00", "TBC"),
    .iv_inj("Joe Bloggs", "Carlton Blues", "2025-03-25 00:00:00", "1-2 weeks")
  )
  teams <- .iv_lineup(data.frame(
    round_number = 3L, given_name = "Joe", surname = "Bloggs",
    team_name = "Carlton Blues"))
  .iv_mock(inj, teams)

  # Round 2: latest pre-kickoff listing is "1-2 weeks", not TBC.
  res <- suppressMessages(tbc_played_rate(2025))
  expect_equal(nrow(res), 0L)
  acc <- suppressMessages(injury_return_accuracy(2025))
  expect_equal(acc$estimated_return[acc$round == 2], "1-2 weeks")
})

test_that("same-name players on different clubs are not conflated", {
  inj <- rbind(
    .iv_inj("Sam Smith", "Carlton Blues", "2025-03-22 00:00:00", "TBC"),
    .iv_inj("Sam Smith", "Geelong Cats", "2025-03-22 00:00:00", "TBC")
  )
  teams <- .iv_lineup(data.frame(
    round_number = 2L, given_name = "Sam", surname = "Smith",
    team_name = "Geelong Cats"))
  .iv_mock(inj, teams)

  res <- suppressMessages(tbc_played_rate(2025, round = 2))
  expect_equal(nrow(res), 2L)
  expect_true(res$played[res$team == "Geelong Cats"])
  expect_false(res$played[res$team == "Carlton Blues"])
})

test_that("tbc_return_survival keeps a second episode and censored newest scrape", {
  inj <- rbind(
    .iv_inj("Joe Bloggs", "Carlton Blues", "2025-03-18 00:00:00", "TBC"),
    .iv_inj("Joe Bloggs", "Carlton Blues", "2025-03-30 00:00:00", "TBC"),
    .iv_inj("Ann Other", "Geelong Cats", "2025-04-10 00:00:00", "TBC")
  )
  teams <- .iv_lineup(data.frame(
    round_number = c(1L, 2L), given_name = "Joe", surname = "Bloggs",
    team_name = "Carlton Blues"))
  .iv_mock(inj, teams)

  res <- suppressMessages(tbc_return_survival(2025))
  joe <- res[res$player == "Joe Bloggs", ]
  expect_equal(nrow(joe), 2L)
  expect_equal(joe$first_tbc_round, c(1L, 3L))
  expect_equal(joe$censored, c(FALSE, TRUE))
  ann <- res[res$player == "Ann Other", ]
  expect_equal(nrow(ann), 1L)
  expect_equal(ann$first_tbc_round, 4L)
  expect_true(ann$censored)
  expect_gte(ann$rounds_out, 0L)
})

test_that(".scrape_round_for returns next upcoming round or NA", {
  rs <- data.frame(round_number = 1:2,
                   round_start = as.POSIXct(c("2025-03-20", "2025-03-27"), tz = "UTC"))
  ts <- as.POSIXct(c("2025-03-01", "2025-03-21", "2025-04-01"), tz = "UTC")
  expect_equal(.scrape_round_for(ts, rs), c(1L, 2L, NA_integer_))
})
