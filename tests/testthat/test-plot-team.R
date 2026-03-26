test_that("plot_team_ratings returns ggplot with mock data", {
  mock_ratings <- data.frame(
    team = torp::AFL_TEAMS$name,
    team_epr = rnorm(18, 0, 3),
    team_recv = rnorm(18, 0, 1),
    team_disp = rnorm(18, 0, 1),
    team_spoil = rnorm(18, 0, 0.5),
    team_hitout = rnorm(18, 0, 0.3),
    stringsAsFactors = FALSE
  )
  p <- plot_team_ratings(mock_ratings, metric = "epr")
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 1)
})

test_that("plot_team_ratings works with different metrics", {
  mock_ratings <- data.frame(
    team = torp::AFL_TEAMS$name,
    team_epr = rnorm(18), team_recv = rnorm(18),
    team_disp = rnorm(18), team_spoil = rnorm(18),
    team_hitout = rnorm(18),
    stringsAsFactors = FALSE
  )
  p <- plot_team_ratings(mock_ratings, metric = "recv")
  expect_s3_class(p, "ggplot")
})

test_that("plot_team_ratings errors on missing metric column", {
  mock_ratings <- data.frame(team = "Adelaide Crows", x = 1, stringsAsFactors = FALSE)
  expect_error(plot_team_ratings(mock_ratings, metric = "epr"))
})
