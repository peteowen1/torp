# Tests for helper_functions.R
# Tests for clean_pbp and related internal functions

test_that("clean_pbp internal helper functions exist", {
  # Test that key internal functions exist

  expect_true(exists("add_torp_ids", envir = asNamespace("torp")))
  expect_true(exists("add_basic_variables", envir = asNamespace("torp")))
  expect_true(exists("add_chain_variables", envir = asNamespace("torp")))
  expect_true(exists("add_quarter_variables", envir = asNamespace("torp")))
  expect_true(exists("add_game_variables", envir = asNamespace("torp")))
  expect_true(exists("add_score_variables", envir = asNamespace("torp")))
})

test_that("determine_play_type returns correct values", {
  determine_play_type <- torp:::determine_play_type

  # Test specific descriptions
  expect_equal(as.character(determine_play_type("Handball")), "Handball")
  expect_equal(as.character(determine_play_type("Kick")), "Kick")
  expect_equal(as.character(determine_play_type("Ground Kick")), "Ground Kick")
  expect_equal(as.character(determine_play_type("Mark")), "Reception")
  expect_equal(as.character(determine_play_type("Free For")), "Reception")

  # Test vector input
  descriptions <- c("Kick", "Handball", "Mark")
  results <- determine_play_type(descriptions)
  expect_equal(as.character(results), c("Kick", "Handball", "Reception"))
})

test_that("determine_phase_of_play returns correct phases", {
  determine_phase_of_play <- torp:::determine_phase_of_play

  # Test throw-in scenarios
  result <- determine_phase_of_play("Centre Bounce", "Kick", 1)
  expect_equal(as.character(result), "Hard Ball")

  # Test set shot detection
  result <- determine_phase_of_play("Free For", "Kick", 0)
  expect_equal(as.character(result), "Set Shot")

  result <- determine_phase_of_play("Contested Mark", "Kick", 0)
  expect_equal(as.character(result), "Set Shot")

  # Test loose ball
  result <- determine_phase_of_play("Loose Ball Get", "Kick", 0)
  expect_equal(as.character(result), "Loose Ball")

  # Test handball received
  result <- determine_phase_of_play("Handball Received", "Kick", 0)
  expect_equal(as.character(result), "Handball Received")
})

test_that("calculate_label_wp returns correct win probabilities", {
  calculate_label_wp <- torp:::calculate_label_wp

  # Home team winning
  expect_equal(calculate_label_wp(100, 80, 1), 1)
  expect_equal(calculate_label_wp(100, 80, 0), 0)

  # Away team winning
  expect_equal(calculate_label_wp(80, 100, 1), 0)
  expect_equal(calculate_label_wp(80, 100, 0), 1)

  # Draw
  expect_equal(calculate_label_wp(100, 100, 1), 0.5)
  expect_equal(calculate_label_wp(100, 100, 0), 0.5)

  # Vector input
  home_scores <- c(100, 80, 90)
  away_scores <- c(80, 100, 90)
  home_flags <- c(1, 1, 1)
  results <- calculate_label_wp(home_scores, away_scores, home_flags)
  expect_equal(results, c(1, 0, 0.5))
})

test_that("calculate_points_row returns correct values", {
  calculate_points_row <- torp:::calculate_points_row

  # Goal scoring
  result <- calculate_points_row("goal", "Goal", 1, 0)
  expect_equal(result, 6)

  # Behind scoring
  result <- calculate_points_row("behind", "Behind", 1, 0)
  expect_equal(result, 1)

  # Rushed behind
  result <- calculate_points_row("rushed", "Spoil", 1, 0)
  expect_equal(result, 1)

  # Non-scoring play
  result <- calculate_points_row("turnover", "Kick", 0, 0)
  expect_true(is.na(result))
})

test_that("clean_pbp function exists and is exported",
{
  expect_true(exists("clean_pbp"))
  expect_true("clean_pbp" %in% getNamespaceExports("torp"))
})
