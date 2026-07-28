# torpverse/docs/plans/FABLE-MATCH-FEATURES-PLAN.md WS1/WS1b -- xScore team
# power rating, the match model's team-strength feature (replaced the win-based
# Elo 2026-07-28). Network-free: build_xscore_rating() operates purely on a
# synthetic matches data.frame, no loaders involved.

.synthetic_xmatches <- function() {
  data.frame(
    match_id     = c("m1", "m2", "m3", "m4"),
    date         = as.Date(c("2024-01-01", "2024-01-08", "2024-01-15", "2025-01-01")),
    season       = c(2024L, 2024L, 2024L, 2025L),
    round        = c(1L, 2L, 3L, 1L),
    home_team    = c("A", "B", "A", "A"),
    away_team    = c("B", "A", "C", "B"),
    home_xmargin = c(20, -10, 5, 0),
    stringsAsFactors = FALSE
  )
}

test_that("build_xscore_rating: rating_pre for match N reflects only strictly-prior matches", {
  m <- .synthetic_xmatches()
  res <- build_xscore_rating(m, k = 0.1, hga = 0, carryover = 1)
  bm <- res$by_match

  # m1 is the first match either team appears in -- both start at the
  # league-average 0 (NOT 1500; this rating lives in points space)
  expect_equal(bm$rating_pre[bm$match_id == "m1" & bm$team_name == "A"], 0)
  expect_equal(bm$rating_pre[bm$match_id == "m1" & bm$team_name == "B"], 0)

  # A out-xscored B by 20 in m1, so A rises and B falls going into m2
  expect_gt(bm$rating_pre[bm$match_id == "m2" & bm$team_name == "A"], 0)
  expect_lt(bm$rating_pre[bm$match_id == "m2" & bm$team_name == "B"], 0)

  # C has never played before m3 -- untouched by matches it wasn't in
  expect_equal(bm$rating_pre[bm$match_id == "m3" & bm$team_name == "C"], 0)
})

test_that("build_xscore_rating: updates are zero-sum, so ratings stay centred on 0", {
  m <- .synthetic_xmatches()
  res <- build_xscore_rating(m, k = 0.1, hga = 0, carryover = 1)
  # Every update moves the two sides equally and oppositely from a common
  # origin, so the league total must remain 0 -- this is what makes the
  # "neutral fallback = 0" contract meaningful.
  expect_equal(sum(res$current$rating_current), 0, tolerance = 1e-10)
})

test_that("build_xscore_rating: a team's rating is its own history, NOT half the match difference", {
  # Regression test for a real bug caught in development: reconstructing a
  # team's rating as rating_diff/2 is wrong whenever both teams are on the same
  # side of average (home +30 / away +10 gives a difference of 20, not 10 each).
  m <- data.frame(
    match_id     = c("m1", "m2", "m3"),
    date         = as.Date(c("2024-01-01", "2024-01-08", "2024-01-15")),
    season       = rep(2024L, 3), round = 1:3,
    home_team    = c("A", "A", "A"),
    away_team    = c("B", "C", "B"),
    home_xmargin = c(60, 60, 60),   # A dominates everyone
    stringsAsFactors = FALSE
  )
  res <- build_xscore_rating(m, k = 0.2, hga = 0, carryover = 1)
  bm <- res$by_match
  a_pre <- bm$rating_pre[bm$match_id == "m3" & bm$team_name == "A"]
  b_pre <- bm$rating_pre[bm$match_id == "m3" & bm$team_name == "B"]
  # A is well above average and B below it; the difference is not 2x either one
  expect_gt(a_pre, 0)
  expect_lt(b_pre, 0)
  expect_false(isTRUE(all.equal(a_pre, (a_pre - b_pre) / 2)))
})

test_that("build_xscore_rating: carryover < 1 regresses ratings toward 0 at a season boundary", {
  m <- .synthetic_xmatches()
  full <- build_xscore_rating(m, k = 0.1, hga = 0, carryover = 1)
  half <- build_xscore_rating(m, k = 0.1, hga = 0, carryover = 0.5)

  a_full <- full$by_match$rating_pre[full$by_match$match_id == "m4" & full$by_match$team_name == "A"]
  a_half <- half$by_match$rating_pre[half$by_match$match_id == "m4" & half$by_match$team_name == "A"]
  expect_lt(abs(a_half), abs(a_full))
})

test_that("build_xscore_rating: hga is credited to the home team, not the rating", {
  # With hga = h and a home xmargin of exactly h, the prediction is perfect for
  # two equal teams, so neither rating should move.
  m <- data.frame(
    match_id = "m1", date = as.Date("2024-01-01"), season = 2024L, round = 1L,
    home_team = "A", away_team = "B", home_xmargin = 8,
    stringsAsFactors = FALSE
  )
  res <- build_xscore_rating(m, k = 0.5, hga = 8, carryover = 1)
  expect_equal(res$current$rating_current, c(0, 0))
})

test_that("join_xrating_diff_to_team_mdl_df: played matches use history; unplayed fall back to current rating", {
  m <- .synthetic_xmatches()
  res <- build_xscore_rating(m, k = 0.1, hga = 0, carryover = 1)

  team_mdl_df <- data.frame(
    match_id    = c("m1", "m1", "future", "future"),
    team_name.x = c("A", "B", "A", "B"),
    team_name.y = c("B", "A", "B", "A"),
    stringsAsFactors = FALSE
  )
  out <- join_xrating_diff_to_team_mdl_df(team_mdl_df, res)

  # m1: both teams started at 0, so the diff is 0 both ways
  expect_equal(out$xelo_diff[out$match_id == "m1"], c(0, 0))

  # "future" has no historical rating -- must use each team's CURRENT rating,
  # not silently collapse to 0 (that would blank the feature for exactly the
  # rows predictions are needed on)
  a_cur <- res$current$rating_current[res$current$team_name == "A"]
  b_cur <- res$current$rating_current[res$current$team_name == "B"]
  fut_a_home <- out$xelo_diff[out$match_id == "future" & out$team_name.x == "A"]
  expect_equal(fut_a_home, a_cur - b_cur)
  expect_false(isTRUE(all.equal(fut_a_home, 0)))
})

test_that("join_xrating_diff_to_team_mdl_df: a team with no history anywhere gets the neutral 0", {
  m <- .synthetic_xmatches()
  res <- build_xscore_rating(m, k = 0.1, hga = 0, carryover = 1)
  team_mdl_df <- data.frame(
    match_id    = "x1",
    team_name.x = "NEVER_PLAYED",
    team_name.y = "ALSO_NEVER",
    stringsAsFactors = FALSE
  )
  out <- join_xrating_diff_to_team_mdl_df(team_mdl_df, res)
  expect_equal(out$xelo_diff, 0)
})

test_that("join_xrating_diff_to_team_mdl_df: xelo_diff is antisymmetric between the two rows of a match", {
  m <- .synthetic_xmatches()
  res <- build_xscore_rating(m, k = 0.1, hga = 0, carryover = 1)
  team_mdl_df <- data.frame(
    match_id    = c("m3", "m3"),
    team_name.x = c("A", "C"),
    team_name.y = c("C", "A"),
    stringsAsFactors = FALSE
  )
  out <- join_xrating_diff_to_team_mdl_df(team_mdl_df, res)
  expect_equal(out$xelo_diff[1], -out$xelo_diff[2])
})

test_that(".xscore_matches_from_team_mdl_df: falls back to actual margin when xScore is missing", {
  team_mdl_df <- data.frame(
    match_id        = c("m1", "m1"),
    team_type       = c("home", "away"),
    win             = c(1, 0),
    utc_start_time  = rep("2024-01-01T00:00:00", 2),
    season.x        = rep(2024L, 2),
    round_number.x  = rep(1L, 2),
    team_name.x     = c("A", "B"),
    team_name.y     = c("B", "A"),
    xscore_diff     = c(NA_real_, NA_real_),
    score_diff      = c(25, -25),
    stringsAsFactors = FALSE
  )
  # 1 of 1 missing is 100%, above the 10% threshold, so this takes the loud
  # branch -- a systematic xG-join failure must not degrade to a plain points
  # rating silently.
  expect_warning(.xscore_matches_from_team_mdl_df(team_mdl_df), "no xScore margin")
  out <- suppressWarnings(.xscore_matches_from_team_mdl_df(team_mdl_df))
  expect_equal(out$home_xmargin, 25)
})

test_that(".xscore_matches_from_team_mdl_df: an isolated xScore gap informs rather than warns", {
  mk <- function(id, xs, sd) data.frame(
    match_id = rep(id, 2), team_type = c("home", "away"), win = c(1, 0),
    utc_start_time = rep("2024-01-01T00:00:00", 2), season.x = rep(2024L, 2),
    round_number.x = rep(1L, 2), team_name.x = c("A", "B"),
    team_name.y = c("B", "A"), xscore_diff = c(xs, -xs),
    score_diff = c(sd, -sd), stringsAsFactors = FALSE
  )
  df <- do.call(rbind, c(list(mk("m0", NA_real_, 25)),
                          lapply(1:19, function(i) mk(paste0("m", i), 10, 10))))
  expect_no_warning(out <- .xscore_matches_from_team_mdl_df(df))
  expect_equal(out$home_xmargin[out$match_id == "m0"], 25)
})
