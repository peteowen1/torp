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


# ==========================================================================
# Tests for calculate_psv()
# ==========================================================================

test_that("calculate_psv returns expected structure", {
  stats <- data.table::data.table(
    player_id = c("P1", "P2", "P3"),
    player_name = c("Alice", "Bob", "Carol"),
    season = 2025L,
    round = 1L,
    match_id = c("M1", "M2", "M3"),
    team = c("A", "B", "C"),
    tog = c(0.9, 0.8, 0.75),
    kicks = c(12, 8, 10),
    handballs = c(6, 10, 5)
  )

  coef_df <- data.frame(
    stat_name = c("kicks", "handballs"),
    beta = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df)

  expect_s3_class(result, "data.table")
  expect_true(all(c("psv", "psv_raw") %in% names(result)))
  expect_true("player_id" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("calculate_psv applies TOG adjustment to rate stats", {
  stats <- data.table::data.table(
    player_id = c("P1", "P2"),
    player_name = c("Alice", "Bob"),
    season = 2025L, round = 1L,
    match_id = c("M1", "M2"),
    tog = c(1.0, 0.5),
    kicks = c(10, 10)
  )

  coef_df <- data.frame(stat_name = "kicks", beta = 1, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, center = FALSE)

  # P1: 10/1.0 * 1 = 10; P2: 10/0.5 * 1 = 20
  expect_equal(result[player_id == "P1"]$psv, 10)
  expect_equal(result[player_id == "P2"]$psv, 20)
})

test_that("calculate_psv does NOT TOG-adjust efficiency stats", {
  stats <- data.table::data.table(
    player_id = "P1",
    player_name = "Alice",
    season = 2025L, round = 1L,
    match_id = "M1",
    tog = 0.5,
    disposal_efficiency = 70  # percentage scale
  )

  coef_df <- data.frame(stat_name = "disposal_efficiency", beta = 1, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, center = FALSE)

  # disposal_efficiency is an efficiency stat — should be converted to 0-1 (0.7)

  # but NOT divided by TOG
  expect_equal(result$psv, 0.7)
})

test_that("calculate_psv handles SD normalization", {
  stats <- data.table::data.table(
    player_id = "P1",
    player_name = "Alice",
    season = 2025L, round = 1L,
    match_id = "M1",
    tog = 1.0,
    kicks = 10
  )

  coef_df <- data.frame(stat_name = "kicks", beta = 2, sd = 5, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, center = FALSE)

  # 10/1.0 (tog adj) / 5 (sd) * 2 (beta) = 4
  expect_equal(result$psv, 4)
})

test_that("calculate_psv centers within round by default", {
  stats <- data.table::data.table(
    player_id = c("P1", "P2"),
    player_name = c("Alice", "Bob"),
    season = 2025L, round = 1L,
    match_id = c("M1", "M2"),
    tog = c(1.0, 1.0),
    kicks = c(10, 0)
  )

  coef_df <- data.frame(stat_name = "kicks", beta = 1, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, center = TRUE)

  # Raw: P1=10, P2=0; mean=5; centered: P1=5, P2=-5
  expect_equal(result[player_id == "P1"]$psv, 5)
  expect_equal(result[player_id == "P2"]$psv, -5)
})

test_that("calculate_psv_components ensures osv + dsv = psv", {
  stats <- data.table::data.table(
    player_id = c("P1", "P2", "P3"),
    player_name = c("Alice", "Bob", "Carol"),
    season = 2025L, round = 1L,
    match_id = c("M1", "M2", "M3"),
    tog = c(0.9, 0.8, 1.0),
    kicks = c(12, 8, 10),
    tackles = c(5, 3, 7)
  )

  margin_coef <- data.frame(stat_name = c("kicks", "tackles"), beta = c(0.5, 0.3))
  off_coef <- data.frame(stat_name = c("kicks", "tackles"), beta = c(0.8, 0.1))
  def_coef <- data.frame(stat_name = c("kicks", "tackles"), beta = c(0.1, 0.7))

  result <- calculate_psv_components(stats, margin_coef, off_coef, def_coef,
                                      center = FALSE)

  expect_true(all(c("psv", "osv", "dsv") %in% names(result)))
  expect_equal(result$osv + result$dsv, result$psv, tolerance = 1e-10)
})

test_that("calculate_psv derives contested_poss_rate from components", {
  stats <- data.table::data.table(
    player_id = "P1",
    player_name = "Alice",
    season = 2025L, round = 1L,
    match_id = "M1",
    tog = 1.0,
    contested_possessions = 8,
    uncontested_possessions = 12,
    contested_poss_rate = 0  # zero = not directly available
  )

  coef_df <- data.frame(stat_name = "contested_poss_rate", beta = 1, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, center = FALSE)

  # Should derive: 8 / (8+12) = 0.4
  expect_equal(result$psv, 0.4)
})
