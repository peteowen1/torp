# Tests for R/player_attribution.R

test_that("calculate_player_attribution returns expected structure", {
  features <- data.frame(
    home_epr = 5.0, away_epr = 3.0, neutral_feature = 10
  )
  predict_fn <- function(x) x$home_epr - x$away_epr
  rating_cols <- c("home_epr", "away_epr")

  result <- calculate_player_attribution(features, predict_fn, rating_cols)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("feature", "full_pred", "ablated_pred",
                     "contribution", "pct_contribution") %in% names(result)))
  expect_equal(nrow(result), 2)
})

test_that("calculate_player_attribution computes correct contributions", {
  features <- data.frame(a = 10, b = 5, c = 0)
  predict_fn <- function(x) x$a + x$b + x$c
  rating_cols <- c("a", "b", "c")

  result <- calculate_player_attribution(features, predict_fn, rating_cols)

  # Zeroing 'a' reduces prediction from 15 to 5 → contribution = 10
  a_row <- result[result$feature == "a", ]
  expect_equal(a_row$full_pred, 15)
  expect_equal(a_row$ablated_pred, 5)
  expect_equal(a_row$contribution, 10)

  # Zeroing 'b' reduces prediction from 15 to 10 → contribution = 5
  b_row <- result[result$feature == "b", ]
  expect_equal(b_row$contribution, 5)

  # Zeroing 'c' (already 0) → no change
  c_row <- result[result$feature == "c", ]
  expect_equal(c_row$contribution, 0)
})

test_that("calculate_player_attribution handles negative contributions", {
  features <- data.frame(good = 10, bad = -5)
  predict_fn <- function(x) x$good + x$bad
  rating_cols <- c("good", "bad")

  result <- calculate_player_attribution(features, predict_fn, rating_cols)

  # Zeroing 'bad' increases prediction: contribution is negative
  bad_row <- result[result$feature == "bad", ]
  expect_equal(bad_row$contribution, -5)
})

test_that("calculate_player_attribution rejects multi-row input", {
  features <- data.frame(a = c(1, 2), b = c(3, 4))
  predict_fn <- function(x) sum(x$a)

  expect_error(
    calculate_player_attribution(features, predict_fn, c("a")),
    "exactly 1 row"
  )
})

test_that("calculate_player_attribution ignores missing rating columns", {
  features <- data.frame(a = 10, b = 5)
  predict_fn <- function(x) x$a + x$b
  # 'c' doesn't exist in features
  rating_cols <- c("a", "c")

  result <- calculate_player_attribution(features, predict_fn, rating_cols)

  # Only 'a' should be ablated (c doesn't exist)
  expect_equal(nrow(result), 1)
  expect_equal(result$feature, "a")
})

test_that("calculate_player_attribution percentage sums to ~100 when all positive", {
  features <- data.frame(a = 3, b = 2, c = 1)
  predict_fn <- function(x) x$a + x$b + x$c
  rating_cols <- c("a", "b", "c")

  result <- calculate_player_attribution(features, predict_fn, rating_cols)

  expect_equal(sum(abs(result$pct_contribution)), 100, tolerance = 0.1)
})

test_that("calculate_player_attribution orders by absolute contribution", {
  features <- data.frame(a = 1, b = 10, c = 5)
  predict_fn <- function(x) x$a + x$b + x$c
  rating_cols <- c("a", "b", "c")

  result <- calculate_player_attribution(features, predict_fn, rating_cols)

  # Should be ordered by |contribution| descending
  expect_equal(result$feature[1], "b")
  expect_equal(result$feature[3], "a")
})

test_that("batch_player_attribution aggregates across matches", {
  make_features <- function(a, b) data.frame(a = a, b = b)
  predict_fn <- function(x) x$a + x$b
  rating_cols <- c("a", "b")

  match_list <- list(
    make_features(10, 5),
    make_features(8, 7),
    make_features(6, 9)
  )

  result <- batch_player_attribution(match_list, predict_fn, rating_cols)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("mean_contribution" %in% names(result))
})
