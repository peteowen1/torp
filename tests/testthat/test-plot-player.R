test_that("plot_player_rating returns ggplot with mocked resolve_player", {
  mock <- create_mock_player_game_ratings()

  local_mocked_bindings(
    resolve_player = function(...) list(
      player_id = "CD_I123456",
      player_name = "Test Player",
      team = "Adelaide Crows",
      position = "Midfielder"
    )
  )

  p <- plot_player_rating("Test Player", data = mock)
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2) # points + rolling line
  expect_equal(p$labels$y, "TORP Value")
})

test_that("plot_player_rating uses correct y-label per metric", {
  mock <- create_mock_player_game_ratings()

  local_mocked_bindings(
    resolve_player = function(...) list(
      player_id = "CD_I123456", player_name = "Test Player",
      team = "Adelaide Crows", position = "Midfielder"
    )
  )

  p <- plot_player_rating("Test Player", metric = "epv", data = mock)
  expect_equal(p$labels$y, "EPV")
})

test_that("plot_player_rating errors on missing metric column", {
  mock <- create_mock_player_game_ratings()
  mock$nonexistent <- NULL

  local_mocked_bindings(
    resolve_player = function(...) list(
      player_id = "CD_I123456", player_name = "Test Player",
      team = "Adelaide Crows", position = "Midfielder"
    )
  )

  expect_error(plot_player_rating("Test Player", metric = "torp_value", data = mock[, c("season", "round_number", "player_id", "player_name", "team")]))
})

test_that("plot_player_rating errors on no game ratings", {
  mock <- create_mock_player_game_ratings()
  mock <- mock[0, ] # empty

  local_mocked_bindings(
    resolve_player = function(...) list(
      player_id = "CD_I123456", player_name = "Test Player",
      team = "Adelaide Crows", position = "Midfielder"
    )
  )

  expect_error(plot_player_rating("Test Player", data = mock), "No game ratings")
})

test_that("plot_stat_rating_profile returns ggplot for bar type", {
  mock_profile <- create_mock_stat_rating_profile()
  p <- plot_stat_rating_profile(mock_profile)
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 1)
  expect_equal(p$labels$y, "Percentile")
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
