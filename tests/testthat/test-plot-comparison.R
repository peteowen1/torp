test_that("plot_player_comparison returns ggplot with mocked resolve_player", {
  mock <- rbind(
    create_mock_player_game_ratings(),
    transform(create_mock_player_game_ratings(),
      player_id = "CD_I654321", player_name = "Player Two", team = "Brisbane Lions"
    )
  )

  call_count <- 0L
  local_mocked_bindings(
    resolve_player = function(name, ...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        list(player_id = "CD_I123456", player_name = "Test Player",
             team = "Adelaide Crows", position = "Midfielder")
      } else {
        list(player_id = "CD_I654321", player_name = "Player Two",
             team = "Brisbane Lions", position = "Forward")
      }
    }
  )

  p <- plot_player_comparison(c("Test Player", "Player Two"), data = mock)
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2) # points + rolling line
  expect_equal(p$labels$y, "TORP Value")
})

test_that("plot_player_comparison errors on wrong number of players", {
  expect_error(plot_player_comparison("Single Player"), "2-5 player names")
  expect_error(plot_player_comparison(paste0("Player", 1:6)), "2-5 player names")
})

test_that("plot_player_comparison handles resolve_player failure gracefully", {
  mock <- create_mock_player_game_ratings()

  call_count <- 0L
  local_mocked_bindings(
    resolve_player = function(name, ...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        list(player_id = "CD_I123456", player_name = "Test Player",
             team = "Adelaide Crows", position = "Midfielder")
      } else {
        stop("Player not found")
      }
    }
  )

  # Second player fails but first succeeds — should still abort because
  # we end up with only 1 player (which is still plotted, not an error for the plot itself)
  # Actually the function will proceed with 1 player — which is fine
  expect_warning(
    p <- plot_player_comparison(c("Test Player", "Bad Name"), data = mock),
    "Could not resolve"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_player_comparison works without points", {
  mock <- rbind(
    create_mock_player_game_ratings(),
    transform(create_mock_player_game_ratings(),
      player_id = "CD_I654321", player_name = "Player Two", team = "Brisbane Lions"
    )
  )

  call_count <- 0L
  local_mocked_bindings(
    resolve_player = function(name, ...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        list(player_id = "CD_I123456", player_name = "Test Player",
             team = "Adelaide Crows", position = "Midfielder")
      } else {
        list(player_id = "CD_I654321", player_name = "Player Two",
             team = "Brisbane Lions", position = "Forward")
      }
    }
  )

  p <- plot_player_comparison(c("Test Player", "Player Two"),
                               show_points = FALSE, data = mock)
  expect_s3_class(p, "ggplot")
})
