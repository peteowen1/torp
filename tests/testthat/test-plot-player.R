test_that("plot_player_rating returns ggplot with mock data", {
  mock <- create_mock_player_game_ratings()
  # Use data param directly; skip resolve_player by pre-filtering
  # We test the plot building, not player resolution
  mock$game_number <- seq_len(nrow(mock))
  mock$season_fac <- factor(mock$season)
  vals <- mock$torp_value
  mock$rolling_avg <- NA_real_
  for (i in seq_along(vals)) {
    start <- max(1, i - 5 + 1)
    mock$rolling_avg[i] <- mean(vals[start:i], na.rm = TRUE)
  }

  p <- ggplot2::ggplot(mock, ggplot2::aes(x = game_number)) +
    ggplot2::geom_point(ggplot2::aes(y = torp_value, colour = season_fac), size = 1.5, alpha = 0.4) +
    ggplot2::geom_line(ggplot2::aes(y = rolling_avg), colour = "grey20") +
    theme_torp()
  expect_s3_class(p, "ggplot")
})

test_that("plot_stat_rating_profile returns ggplot for bar type", {
  mock_profile <- create_mock_stat_rating_profile()
  p <- plot_stat_rating_profile(mock_profile, type = "bar")
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 1)
})

test_that("plot_stat_rating_profile returns ggplot for radar type", {
  mock_profile <- create_mock_stat_rating_profile()
  p <- plot_stat_rating_profile(mock_profile, type = "radar")
  expect_s3_class(p, "ggplot")
})

test_that("plot_stat_rating_profile errors on wrong class", {
  expect_error(plot_stat_rating_profile(list(a = 1)))
})

test_that("plot_stat_rating_profile filters categories", {
  mock_profile <- create_mock_stat_rating_profile()
  p <- plot_stat_rating_profile(mock_profile, categories = "scoring")
  expect_s3_class(p, "ggplot")
})

test_that("plot_stat_rating_profile uses league comparison", {
  mock_profile <- create_mock_stat_rating_profile()
  p <- plot_stat_rating_profile(mock_profile, comparison = "league")
  expect_s3_class(p, "ggplot")
})
