test_that("plot_team_ratings returns ggplot with mock data", {
  mock_ratings <- data.frame(
    team = torp::AFL_TEAMS$name,
    torp = rnorm(18, 0, 5),
    epr = rnorm(18, 0, 3),
    recv_epr = rnorm(18, 0, 1),
    disp_epr = rnorm(18, 0, 1),
    spoil_epr = rnorm(18, 0, 0.5),
    hitout_epr = rnorm(18, 0, 0.3),
    stringsAsFactors = FALSE
  )
  p <- plot_team_ratings(mock_ratings, metric = "torp")
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 1)
})

test_that("plot_team_ratings works with different metrics", {
  mock_ratings <- data.frame(
    team = torp::AFL_TEAMS$name,
    torp = rnorm(18), epr = rnorm(18),
    recv_epr = rnorm(18), disp_epr = rnorm(18),
    spoil_epr = rnorm(18), hitout_epr = rnorm(18),
    stringsAsFactors = FALSE
  )
  p <- plot_team_ratings(mock_ratings, metric = "epr")
  expect_s3_class(p, "ggplot")
})

test_that("plot_team_ratings errors on missing metric column", {
  mock_ratings <- data.frame(team = "Adelaide Crows", x = 1, stringsAsFactors = FALSE)
  expect_error(plot_team_ratings(mock_ratings, metric = "torp"))
})
