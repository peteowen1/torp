# Tests for R/plot_utils.R

test_that("theme_torp returns a ggplot2 theme", {
  skip_if_not_installed("ggplot2")
  thm <- theme_torp()
  expect_s3_class(thm, "theme")
})

test_that("team_color_scale returns a ggplot2 scale", {
  skip_if_not_installed("ggplot2")
  sc <- team_color_scale()
  expect_s3_class(sc, "Scale")
})

test_that("team_fill_scale returns a ggplot2 scale", {
  skip_if_not_installed("ggplot2")
  sc <- team_fill_scale()
  expect_s3_class(sc, "Scale")
})

test_that("team_color_lookup returns correct colour for known team", {
  col <- torp:::team_color_lookup("Carlton")
  expect_true(is.character(col))
  expect_true(grepl("^#", col))
})

test_that("team_color_lookup returns default for unknown team", {
  col <- torp:::team_color_lookup("Nonexistent FC", default = "#123456")
  expect_equal(col, "#123456")
})

test_that("team_color_lookup returns default for NA team", {
  col <- torp:::team_color_lookup(NA_character_, default = "#000000")
  expect_equal(col, "#000000")
})

test_that(".rolling_mean computes correct trailing average", {
  x <- c(10, 20, 30, 40, 50)

  # Window of 1: just the value itself
  expect_equal(torp:::.rolling_mean(x, 1), c(10, 20, 30, 40, 50))

  # Window of 3
  result <- torp:::.rolling_mean(x, 3)
  expect_equal(result[1], 10)               # only 1 value

  expect_equal(result[2], mean(c(10, 20)))   # 2 values
  expect_equal(result[3], mean(c(10, 20, 30)))
  expect_equal(result[4], mean(c(20, 30, 40)))
  expect_equal(result[5], mean(c(30, 40, 50)))
})

test_that(".rolling_mean handles NA values", {
  x <- c(10, NA, 30)
  result <- torp:::.rolling_mean(x, 3)
  expect_equal(result[3], mean(c(10, NA, 30), na.rm = TRUE))
})

test_that(".rolling_mean handles length-1 input", {
  expect_equal(torp:::.rolling_mean(42, 5), 42)
})

test_that("quarter_breaks returns correct positions", {
  qb <- torp:::quarter_breaks()
  expect_equal(length(qb), 5)
  expect_equal(qb[["Q1"]], 0)
  expect_equal(qb[["End"]], 8000)
})
