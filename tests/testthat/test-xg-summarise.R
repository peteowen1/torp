# Tests for .summarise_match_xg() extracted helper in R/xg.R

test_that(".summarise_match_xg computes correct match-level xG stats", {
  pbp <- data.frame(
    match_id = rep("M1", 10),
    period = rep(1:2, each = 5),
    team = c(rep("Home FC", 5), rep("Away FC", 5)),
    home_team_name = rep("Home FC", 10),
    away_team_name = rep("Away FC", 10),
    points_shot = c(6, 1, 0, 6, 0, 1, 6, 0, 0, 0),
    xscore = c(4.5, 0.8, 0.3, 5.2, 0.1, 0.9, 4.0, 0.4, 0.2, 0.1),
    shot_row = c(1, 1, 0, 1, 0, 1, 1, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  result <- torp:::.summarise_match_xg(pbp, quarter = 1:2)

  expect_equal(nrow(result), 1)
  expect_equal(result$home_team, "Home FC")
  expect_equal(result$away_team, "Away FC")
  # Home: 6 + 1 + 0 + 6 + 0 = 13

  expect_equal(result$home_shots_score, 13)
  # Away: 1 + 6 + 0 + 0 + 0 = 7
  expect_equal(result$away_shots_score, 7)
  expect_equal(result$score_diff, 6)
})

test_that(".summarise_match_xg filters by quarter", {
  pbp <- data.frame(
    match_id = rep("M1", 6),
    period = c(1, 1, 2, 2, 3, 3),
    team = rep(c("Home", "Away"), 3),
    home_team_name = rep("Home", 6),
    away_team_name = rep("Away", 6),
    points_shot = c(6, 1, 6, 6, 6, 6),
    xscore = c(4, 1, 5, 4, 5, 5),
    shot_row = rep(1, 6),
    stringsAsFactors = FALSE
  )

  # Only Q1
  result <- torp:::.summarise_match_xg(pbp, quarter = 1)
  expect_equal(result$home_shots_score, 6)
  expect_equal(result$away_shots_score, 1)

  # Q1 + Q2
  result2 <- torp:::.summarise_match_xg(pbp, quarter = 1:2)
  expect_equal(result2$home_shots_score, 12)
})

test_that(".summarise_match_xg handles multiple matches", {
  pbp <- data.frame(
    match_id = c(rep("M1", 4), rep("M2", 4)),
    period = rep(1, 8),
    team = rep(c("A", "B"), 4),
    home_team_name = rep(c("A", "A", "C", "C"), each = 1, length.out = 8),
    away_team_name = rep(c("B", "B", "D", "D"), each = 1, length.out = 8),
    points_shot = rep(6, 8),
    xscore = rep(4.5, 8),
    shot_row = rep(1, 8),
    stringsAsFactors = FALSE
  )

  result <- torp:::.summarise_match_xg(pbp, quarter = 1:4)
  expect_equal(nrow(result), 2)
})

test_that(".summarise_match_xg warns on zero total xscore", {
  pbp <- data.frame(
    match_id = rep("M1", 4),
    period = rep(1, 4),
    team = c("A", "A", "B", "B"),
    home_team_name = rep("A", 4),
    away_team_name = rep("B", 4),
    points_shot = rep(0, 4),
    xscore = rep(0, 4),
    shot_row = rep(1, 4),
    stringsAsFactors = FALSE
  )

  expect_warning(
    torp:::.summarise_match_xg(pbp, quarter = 1:4),
    "zero total xscore"
  )
})

test_that(".summarise_match_xg only counts shot_row == 1 for xscore", {
  pbp <- data.frame(
    match_id = rep("M1", 4),
    period = rep(1, 4),
    team = rep("Home", 4),
    home_team_name = rep("Home", 4),
    away_team_name = rep("Away", 4),
    points_shot = c(6, 0, 6, 0),
    xscore = c(5.0, 3.0, 4.0, 2.0),
    shot_row = c(1, 0, 1, 0),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(torp:::.summarise_match_xg(pbp, quarter = 1:4))
  # xscore should only count where shot_row == 1: 5.0*1 + 3.0*0 + 4.0*1 + 2.0*0 = 9.0
  expect_equal(result$home_xscore, 9.0)
})
