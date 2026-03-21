# Tests for calculate_psr()

test_that("calculate_psr returns expected structure", {
  skills <- data.table::data.table(
    player_id = c("P1", "P2", "P3"),
    player_name = c("Alice", "Bob", "Carol"),
    season = 2025L,
    round = 1L,
    pos_group = c("mid", "fwd", "def"),
    kicks_rating = c(10, 5, 8),
    handballs_rating = c(6, 3, 7)
  )

  coef_df <- data.frame(
    stat_name = c("kicks", "handballs"),
    beta = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )

  result <- calculate_psr(skills, coef_df)

  expect_s3_class(result, "data.table")
  expect_true("psr" %in% names(result))
  expect_true("psr_raw" %in% names(result))
  expect_true("player_id" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("calculate_psr applies coefficients correctly", {
  skills <- data.table::data.table(
    player_id = c("P1", "P2"),
    player_name = c("Alice", "Bob"),
    season = 2025L,
    round = 1L,
    kicks_rating = c(10, 0),
    handballs_rating = c(0, 5)
  )

  coef_df <- data.frame(
    stat_name = c("kicks", "handballs"),
    beta = c(2, 3),
    stringsAsFactors = FALSE
  )

  result <- calculate_psr(skills, coef_df, center = FALSE)

  # P1: 10*2 + 0*3 = 20
  expect_equal(result[player_id == "P1"]$psr, 20)
  # P2: 0*2 + 5*3 = 15
  expect_equal(result[player_id == "P2"]$psr, 15)
})

test_that("calculate_psr centers by default", {
  skills <- data.table::data.table(
    player_id = c("P1", "P2"),
    player_name = c("Alice", "Bob"),
    season = 2025L,
    round = 1L,
    kicks_rating = c(10, 0)
  )

  coef_df <- data.frame(
    stat_name = "kicks",
    beta = 1,
    stringsAsFactors = FALSE
  )

  result <- calculate_psr(skills, coef_df, center = TRUE)

  # Raw: P1=10, P2=0; mean=5; centered: P1=5, P2=-5
  expect_equal(result[player_id == "P1"]$psr, 5)
  expect_equal(result[player_id == "P2"]$psr, -5)
  # psr_raw should be uncentered
  expect_equal(result[player_id == "P1"]$psr_raw, 10)
})

test_that("calculate_psr handles SD normalization", {
  skills <- data.table::data.table(
    player_id = "P1",
    player_name = "Alice",
    season = 2025L,
    round = 1L,
    kicks_rating = 10
  )

  coef_df <- data.frame(
    stat_name = "kicks",
    beta = 2,
    sd = 5,
    stringsAsFactors = FALSE
  )

  result <- calculate_psr(skills, coef_df, center = FALSE)

  # 10/5 * 2 = 4

  expect_equal(result$psr, 4)
})

test_that("calculate_psr handles missing stat rating columns gracefully", {
  skills <- data.table::data.table(
    player_id = "P1",
    player_name = "Alice",
    season = 2025L,
    round = 1L,
    kicks_rating = 10
  )

  coef_df <- data.frame(
    stat_name = c("kicks", "nonexistent"),
    beta = c(1, 1),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- calculate_psr(skills, coef_df, center = FALSE),
    "not found"
  )
  # Should still compute for available columns
  expect_equal(result$psr, 10)
})

test_that("calculate_psr errors on missing required coef_df columns", {
  skills <- data.table::data.table(player_id = "P1", kicks_rating = 10)
  bad_coef <- data.frame(stat = "kicks", value = 1)

  expect_error(calculate_psr(skills, bad_coef), "stat_name")
})

test_that("calculate_psr handles all-zero coefficients", {
  skills <- data.table::data.table(
    player_id = "P1",
    player_name = "Alice",
    season = 2025L,
    round = 1L,
    kicks_rating = 10
  )

  coef_df <- data.frame(stat_name = "kicks", beta = 0)

  expect_warning(
    result <- calculate_psr(skills, coef_df),
    "zero"
  )
  expect_equal(result$psr, 0)
})

test_that("calculate_psr treats NA stat ratings as zero", {
  skills <- data.table::data.table(
    player_id = c("P1", "P2"),
    player_name = c("Alice", "Bob"),
    season = 2025L,
    round = 1L,
    kicks_rating = c(10, NA_real_)
  )

  coef_df <- data.frame(stat_name = "kicks", beta = 1)

  result <- calculate_psr(skills, coef_df, center = FALSE)

  expect_equal(result[player_id == "P1"]$psr, 10)
  expect_equal(result[player_id == "P2"]$psr, 0)
})
