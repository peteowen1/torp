test_that("plot_shot_map returns ggplot with mock data", {
  mock <- data.frame(
    match_id = rep("CD_M20240140101", 30),
    season = 2024L,
    round_number = 1L,
    home_team_name = "Adelaide Crows",
    away_team_name = "Brisbane Lions",
    team = sample(c("Adelaide Crows", "Brisbane Lions"), 30, replace = TRUE),
    goal_x = runif(30, 5, 80),
    y = runif(30, -35, 35),
    shot_row = rep(1L, 30),
    goal_prob = runif(30, 0.1, 0.8),
    behind_prob = runif(30, 0.05, 0.3),
    clanger_prob = runif(30, 0.05, 0.2),
    xscore = runif(30, 0.5, 5),
    points_shot = sample(c(0, 1, 6), 30, replace = TRUE, prob = c(0.3, 0.3, 0.4)),
    player_name = paste0("Player_", sample(1:20, 30, replace = TRUE)),
    stringsAsFactors = FALSE
  )

  p <- plot_shot_map(match_id = "CD_M20240140101", data = mock)
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 1)
})

test_that("plot_shot_map filters by team", {
  mock <- data.frame(
    match_id = rep("CD_M20240140101", 30),
    team = rep(c("Adelaide Crows", "Brisbane Lions"), each = 15),
    goal_x = runif(30, 5, 80),
    y = runif(30, -35, 35),
    shot_row = rep(1L, 30),
    goal_prob = runif(30, 0.1, 0.8),
    points_shot = sample(c(0, 1, 6), 30, replace = TRUE),
    stringsAsFactors = FALSE
  )

  p <- plot_shot_map(match_id = "CD_M20240140101", team = "Adelaide Crows", data = mock)
  expect_s3_class(p, "ggplot")
})

test_that("plot_shot_map errors with no shots", {
  mock <- data.frame(
    match_id = "CD_M20240140101",
    goal_x = 50, y = 10, shot_row = 0L,
    stringsAsFactors = FALSE
  )
  expect_error(plot_shot_map(match_id = "CD_M20240140101", data = mock))
})

test_that("plot_shot_map works without xG columns", {
  mock <- data.frame(
    match_id = rep("CD_M20240140101", 10),
    goal_x = runif(10, 5, 80),
    y = runif(10, -35, 35),
    shot_row = rep(1L, 10),
    points_shot = sample(c(0, 1, 6), 10, replace = TRUE),
    team = rep("Adelaide Crows", 10),
    stringsAsFactors = FALSE
  )

  p <- plot_shot_map(match_id = "CD_M20240140101", data = mock)
  expect_s3_class(p, "ggplot")
})
