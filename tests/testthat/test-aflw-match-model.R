# docs/plans/AFLW-MIGRATION-PLAN.md Phase 3 build-order item 3 -- AFLW match
# prediction v1. Network-free: every function under test here operates
# purely on passed-in data.frames, no loaders involved (mirrors
# test-team-elo.R's pattern).

.synthetic_stat_ratings <- function() {
  data.table::data.table(
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = c("A", "B", "C", "D"),
    season = c(2024L, 2024L, 2024L, 2024L),
    round = c(1L, 1L, 1L, 1L),
    pos_group = c("MID", "MID", "FWD", "DEF"),
    goals_rating = c(1, -1, 2, 0),
    kicks_rating = c(0.5, 0.5, -0.5, 0)
  )
}

.synthetic_coef_df <- function() {
  data.frame(stat_name = c("goals", "kicks"), beta = c(2, 1), sd = c(1, 1))
}

.synthetic_teams <- function() {
  data.table::data.table(
    player_id    = c("p1", "p2", "p3", "p4", "p5"),
    team_name    = c("Home Team", "Home Team", "Away Team", "Away Team", "Away Team"),
    match_id     = c("m1", "m1", "m1", "m1", "m1"),
    season       = c(2024L, 2024L, 2024L, 2024L, 2024L),
    round_number = c(1L, 1L, 1L, 1L, 1L)
  )
}

test_that(".aflw_team_psr_by_round sums per-player PSR to team totals, unrated players contribute 0", {
  team_psr <- as.data.frame(torp:::.aflw_team_psr_by_round(.synthetic_stat_ratings(), .synthetic_coef_df(), .synthetic_teams()))

  expect_true(all(c("match_id", "team_name", "team_psr", "n_rated_players", "n_players") %in% names(team_psr)))
  expect_equal(nrow(team_psr), 2)

  home_row <- team_psr[team_psr$team_name == "Home Team", ]
  away_row <- team_psr[team_psr$team_name == "Away Team", ]

  # p5 is listed for "Away Team" but has no stat_ratings row at all (debut) --
  # must contribute 0, not be dropped or produce NA. p3 and p4 DO have
  # stat_ratings rows (p4's psr happens to compute to exactly 0 -- a real
  # rating of zero, not a missing-data fallback), so n_rated_players is 2, not 3.
  expect_equal(away_row$n_players, 3)
  expect_equal(away_row$n_rated_players, 2)
  expect_false(is.na(away_row$team_psr))
  expect_equal(home_row$n_players, 2)
  expect_equal(home_row$n_rated_players, 2)

  # The regression this test exists to catch: n_rated_players must be able to
  # differ from n_players (counted before the NA->0 fill, not after).
  expect_true(away_row$n_rated_players < away_row$n_players)
})

test_that("join_aflw_features_to_matches computes leak-safe aflw_elo_diff with HGA applied", {
  matches <- data.frame(
    match_id = "m1", date = as.Date("2024-01-01"), season = 2024L,
    home_team = "Home Team", away_team = "Away Team", home_margin = 10,
    stringsAsFactors = FALSE
  )
  elo_result <- list(
    by_match = data.table::data.table(match_id = "m1", team_name = c("Home Team", "Away Team"), elo_pre = c(1550, 1500)),
    current  = data.table::data.table(team_name = c("Home Team", "Away Team"), elo_current = c(1550, 1500))
  )
  team_psr <- data.table::data.table(
    match_id = "m1", team_name = c("Home Team", "Away Team"),
    team_psr = c(5, 2), n_rated_players = c(2, 2), n_players = c(2, 2)
  )

  out <- torp:::join_aflw_features_to_matches(matches, elo_result, team_psr, hga = 20)

  # elo_diff = (1550 + 20) - 1500 = 70, using historical elo_pre (leak-safe)
  expect_equal(out$aflw_elo_diff, 70)
  expect_equal(out$aflw_psr_diff, 3)  # 5 - 2
  expect_true(out$aflw_psr_matched)
})

test_that("join_aflw_features_to_matches: a team whose roster wasn't found at all gives NA psr_diff, NOT a false 0", {
  matches <- data.frame(
    match_id = "m1", date = as.Date("2024-01-01"), season = 2024L,
    home_team = "Fremantle Dockers", away_team = "Away Team", home_margin = 10,
    stringsAsFactors = FALSE
  )
  elo_result <- list(
    by_match = data.table::data.table(match_id = character(0), team_name = character(0), elo_pre = numeric(0)),
    current  = data.table::data.table(team_name = c("Fremantle Dockers", "Away Team"), elo_current = c(1500, 1500))
  )
  # Roster only carries "Walyalup" (heritage name) for the home side --
  # simulates the exact AFL_TEAM_ALIASES gap that produced +-400-point
  # aflw_psr_diff outliers before this was traced and fixed.
  team_psr <- data.table::data.table(
    match_id = "m1", team_name = c("Walyalup", "Away Team"),
    team_psr = c(40, 2), n_rated_players = c(20, 2), n_players = c(20, 2)
  )

  out <- suppressMessages(torp:::join_aflw_features_to_matches(matches, elo_result, team_psr))

  expect_true(is.na(out$aflw_psr_diff))
  expect_false(out$aflw_psr_matched)
  # This is the regression case: aflw_psr_diff must NOT silently read as
  # 0 - 2 = -2 (which is what a naive coalesce(x, 0) would produce)
  expect_false(isTRUE(out$aflw_psr_diff == -2))
})

test_that("join_aflw_features_to_matches: a roster that IS found but has zero rated players also gives NA, not a false 0-0 'even match'", {
  # Distinct failure mode from the heritage-name test above: the join
  # itself succeeds (team_psr is non-NA, team_psr = 0 because every listed
  # player was unrated -- e.g. that round's stat-ratings batch hasn't run
  # yet), which a presence-only check ("is team_psr NA?") cannot see.
  matches <- data.frame(
    match_id = "m1", date = as.Date("2024-01-01"), season = 2024L,
    home_team = "Home Team", away_team = "Away Team", home_margin = 10,
    stringsAsFactors = FALSE
  )
  elo_result <- list(
    by_match = data.table::data.table(match_id = character(0), team_name = character(0), elo_pre = numeric(0)),
    current  = data.table::data.table(team_name = c("Home Team", "Away Team"), elo_current = c(1500, 1500))
  )
  team_psr <- data.table::data.table(
    match_id = "m1", team_name = c("Home Team", "Away Team"),
    team_psr = c(0, 5), n_rated_players = c(0, 22), n_players = c(22, 22)
  )

  out <- suppressMessages(torp:::join_aflw_features_to_matches(matches, elo_result, team_psr))

  expect_true(is.na(out$aflw_psr_diff))
  expect_false(out$aflw_psr_matched)
})

test_that("join_aflw_features_to_matches: no Elo history at all falls back to neutral 1500, elo_diff = hga", {
  matches <- data.frame(
    match_id = "brand_new", date = as.Date("2024-01-01"), season = 2024L,
    home_team = "New Team", away_team = "Other New Team", home_margin = NA,
    stringsAsFactors = FALSE
  )
  elo_result <- list(
    by_match = data.table::data.table(match_id = character(0), team_name = character(0), elo_pre = numeric(0)),
    current  = data.table::data.table(team_name = character(0), elo_current = numeric(0))
  )
  team_psr <- data.table::data.table(
    match_id = character(0), team_name = character(0),
    team_psr = numeric(0), n_rated_players = integer(0), n_players = integer(0)
  )

  out <- suppressMessages(torp:::join_aflw_features_to_matches(matches, elo_result, team_psr, hga = 20))
  expect_equal(out$aflw_elo_diff, 20)  # (1500+20) - 1500
  expect_true(is.na(out$aflw_psr_diff))
})

test_that("fit_aflw_match_model drops psr-unmatched rows explicitly and produces finite predictions", {
  set.seed(1)
  n <- 40
  train_df <- data.frame(
    aflw_elo_diff = stats::rnorm(n, 0, 100),
    aflw_psr_diff = stats::rnorm(n, 0, 20),
    aflw_psr_matched = c(rep(TRUE, n - 3), rep(FALSE, 3)),
    home_margin = numeric(n)
  )
  train_df$home_margin <- 0.2 * train_df$aflw_elo_diff + 0.5 * train_df$aflw_psr_diff + stats::rnorm(n, 0, 10)
  # Force the "unmatched" rows to NA psr_diff, matching real join output
  train_df$aflw_psr_diff[!train_df$aflw_psr_matched] <- NA

  expect_message(
    model <- torp:::fit_aflw_match_model(train_df),
    "dropping 3 training row"
  )

  expect_s3_class(model$win_model, "glm")
  expect_s3_class(model$margin_model, "lm")

  newdata <- data.frame(aflw_elo_diff = c(-50, 0, 50), aflw_psr_diff = c(-10, 0, 10))
  win_p <- torp:::predict_aflw_win_prob(model, newdata)
  margin_p <- torp:::predict_aflw_margin(model, newdata)

  expect_true(all(is.finite(win_p)))
  expect_true(all(win_p >= 0 & win_p <= 1))
  expect_true(all(is.finite(margin_p)))
  # Higher elo_diff/psr_diff should predict a higher home win prob and margin
  # given the positive-coefficient synthetic data generating process
  expect_true(win_p[3] > win_p[1])
  expect_true(margin_p[3] > margin_p[1])
})

test_that("fit_aflw_match_model works fine with no aflw_psr_matched column at all (backwards-compatible)", {
  set.seed(2)
  train_df <- data.frame(
    aflw_elo_diff = stats::rnorm(20, 0, 100),
    aflw_psr_diff = stats::rnorm(20, 0, 20),
    home_margin = stats::rnorm(20, 0, 30)
  )
  expect_no_error(model <- torp:::fit_aflw_match_model(train_df))
  expect_s3_class(model$win_model, "glm")
})

test_that("fit_aflw_match_model drops NA aflw_psr_diff rows and REPORTS a count, even with no aflw_psr_matched column (the silent na.omit hole this function exists to close)", {
  set.seed(3)
  train_df <- data.frame(
    aflw_elo_diff = stats::rnorm(20, 0, 100),
    aflw_psr_diff = stats::rnorm(20, 0, 20),
    home_margin = stats::rnorm(20, 0, 30)
  )
  train_df$aflw_psr_diff[c(2, 7, 15)] <- NA_real_

  expect_message(
    model <- torp:::fit_aflw_match_model(train_df),
    "dropping 3 training row"
  )
  # the fitted model's underlying data must not include the 3 NA rows
  expect_equal(nrow(stats::model.frame(model$win_model)), 17)
})

test_that("predict_aflw_win_prob / predict_aflw_margin warn how many rows will predict NA, and predict NA in place (not a silently shortened vector)", {
  set.seed(4)
  fit_df <- data.frame(
    aflw_elo_diff = stats::rnorm(20, 0, 100),
    aflw_psr_diff = stats::rnorm(20, 0, 20),
    home_margin = stats::rnorm(20, 0, 30)
  )
  model <- torp:::fit_aflw_match_model(fit_df)

  newdata <- data.frame(aflw_elo_diff = c(10, 20, 30), aflw_psr_diff = c(1, NA_real_, 3))

  expect_warning(preds_wp <- torp:::predict_aflw_win_prob(model, newdata), "1 of 3")
  expect_length(preds_wp, 3)  # NA in place, not a shortened vector
  expect_true(is.na(preds_wp[2]))
  expect_false(anyNA(preds_wp[-2]))

  expect_warning(preds_margin <- torp:::predict_aflw_margin(model, newdata), "1 of 3")
  expect_length(preds_margin, 3)
  expect_true(is.na(preds_margin[2]))
  expect_false(anyNA(preds_margin[-2]))
})
