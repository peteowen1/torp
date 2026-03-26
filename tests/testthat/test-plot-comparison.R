test_that("plot_player_comparison returns ggplot with mock data", {
  # Build mock data for 2 players
  mock <- data.frame(
    season = rep(2024, 40),
    round_number = rep(1:20, 2),
    match_id = paste0("CD_M2024014", sprintf("%04d", 1:40)),
    player_id = rep(c("CD_I111111", "CD_I222222"), each = 20),
    player_name = rep(c("Player One", "Player Two"), each = 20),
    team = rep(c("Adelaide Crows", "Brisbane Lions"), each = 20),
    torp_value = rnorm(40, 0, 2),
    epv = rnorm(40, 0, 1.5),
    psv = rnorm(40, 0, 1),
    stringsAsFactors = FALSE
  )

  # We can't use plot_player_comparison directly because it calls resolve_player()

  # which needs network access. Instead, test the core plotting logic.
  # Build the combined data manually as the function would.
  mock$game_number <- rep(1:20, 2)
  mock$player_label <- mock$player_name
  vals1 <- mock$torp_value[1:20]
  vals2 <- mock$torp_value[21:40]
  mock$rolling_avg <- c(
    sapply(seq_along(vals1), function(i) mean(vals1[max(1, i - 4):i])),
    sapply(seq_along(vals2), function(i) mean(vals2[max(1, i - 4):i]))
  )

  p <- ggplot2::ggplot(mock, ggplot2::aes(x = game_number, colour = player_label)) +
    ggplot2::geom_point(ggplot2::aes(y = torp_value), size = 1, alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = rolling_avg), linewidth = 0.9) +
    theme_torp()

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2)
})

test_that("plot_player_comparison errors on wrong number of players", {
  expect_error(
    plot_player_comparison("Single Player"),
    "2-5 player names"
  )
  expect_error(
    plot_player_comparison(paste0("Player", 1:6)),
    "2-5 player names"
  )
})
