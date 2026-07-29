# Tests for calculate_psr()

# Since 2026-07-29 calculate_psr() centres on the LISTED taxonomy and aborts
# rather than silently falling back (see PSR_CENTRE_ON_LISTED). These fixtures
# use synthetic player_ids that match no real roster, so supply the key here.
# Mapping each distinct pos_group to a distinct listed label preserves the exact
# partition these tests were written against, so every assertion below still
# measures what it originally measured.
.listed_for <- function(skills) {
  lab <- c("MIDFIELDER", "MEDIUM_FORWARD", "MEDIUM_DEFENDER",
           "KEY_FORWARD", "KEY_DEFENDER", "RUCK")
  pos <- if (is.null(skills$pos_group)) {
    # No pos_group: the old code centred these fixtures as ONE global group, so
    # give every player the same listed label to preserve that partition.
    rep(lab[1], length(skills$player_id))
  } else {
    pg <- as.character(skills$pos_group)
    lab[match(pg, unique(pg))]
  }
  data.frame(player_id = skills$player_id, position = pos,
             stringsAsFactors = FALSE)
}

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

  result <- calculate_psr(skills, coef_df, listed_pos = .listed_for(skills))

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
  expect_equal(result[player_id == "P1"]$psr, 20 * PSV_POINTS_SCALE)
  # P2: 0*2 + 5*3 = 15
  expect_equal(result[player_id == "P2"]$psr, 15 * PSV_POINTS_SCALE)
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

  result <- calculate_psr(skills, coef_df, center = TRUE, listed_pos = .listed_for(skills))

  # Raw: P1=10, P2=0; mean=5; centered: P1=5, P2=-5
  expect_equal(result[player_id == "P1"]$psr, 5 * PSV_POINTS_SCALE)
  expect_equal(result[player_id == "P2"]$psr, -5 * PSV_POINTS_SCALE)
  # psr_raw should be uncentered
  expect_equal(result[player_id == "P1"]$psr_raw, 10 * PSV_POINTS_SCALE)
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

  expect_equal(result$psr, 4 * PSV_POINTS_SCALE)
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
  expect_equal(result$psr, 10 * PSV_POINTS_SCALE)
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
    result <- calculate_psr(skills, coef_df, listed_pos = .listed_for(skills)),
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

  expect_equal(result[player_id == "P1"]$psr, 10 * PSV_POINTS_SCALE)
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
  expect_true(all(c("psv", "psv_p80", "psv_raw") %in% names(result)))
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

  # psv_p80 is the per-full-game rate: P1: 10/1.0 = 10; P2: 10/0.5 = 20
  expect_equal(result[player_id == "P1"]$psv_p80, 10 * PSV_POINTS_SCALE)
  expect_equal(result[player_id == "P2"]$psv_p80, 20 * PSV_POINTS_SCALE)
  # psv is the per-game value (psv_p80 * tog): P1: 10*1.0 = 10; P2: 20*0.5 = 10
  expect_equal(result[player_id == "P1"]$psv, 10 * PSV_POINTS_SCALE)
  expect_equal(result[player_id == "P2"]$psv, 10 * PSV_POINTS_SCALE)
})

test_that("calculate_psv excludes efficiency stats and bounces", {
  stats <- data.table::data.table(
    player_id = "P1",
    player_name = "Alice",
    season = 2025L, round = 1L,
    match_id = "M1",
    tog = 1.0,
    kicks = 10,
    disposal_efficiency = 70,
    bounces = 5
  )

  coef_df <- data.frame(
    stat_name = c("kicks", "disposal_efficiency", "bounces"),
    beta = c(1, 1, -1),
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, center = FALSE)

  # Only kicks should contribute (efficiency + bounces excluded)
  expect_equal(result$psv, 10 * PSV_POINTS_SCALE)
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
  expect_equal(result$psv, 4 * PSV_POINTS_SCALE)
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
  expect_equal(result[player_id == "P1"]$psv, 5 * PSV_POINTS_SCALE)
  expect_equal(result[player_id == "P2"]$psv, -5 * PSV_POINTS_SCALE)
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

test_that("calculate_psv: psv is per-game (psv_p80 * tog) for low- and full-TOG players (issue #80)", {
  # One low-TOG player and one full-TOG player with identical full-game
  # production. psv_p80 (the centered per-80 rate) should be equal for both,
  # while psv (the per-game value) must be scaled down by the low-TOG player's
  # actual time on ground.
  stats <- data.table::data.table(
    player_id = c("LOW", "FULL"),
    player_name = c("LowTog", "FullTog"),
    season = 2025L, round = 1L,
    match_id = c("M1", "M1"),
    lineup_position = c("MID", "MID"),
    tog = c(0.10, 1.00),
    kicks = c(1, 10) # same per-80 rate: 1/0.1 = 10, 10/1.0 = 10
  )

  coef_df <- data.frame(stat_name = "kicks", beta = 1, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, center = TRUE)

  low <- result[player_id == "LOW"]
  full <- result[player_id == "FULL"]

  # Identical per-80 rate -> identical centered psv_p80 (both zero after
  # centering within the single MID position group).
  expect_equal(low$psv_p80, full$psv_p80)

  # Per-game psv == psv_p80 * tog for every player.
  expect_equal(result$psv, result$psv_p80 * result$tog, tolerance = 1e-10)

  # The low-TOG per-game value is 10x smaller than its per-80 extrapolation
  # would be (0.10 tog), which is the core of the bug being fixed.
  expect_equal(low$psv, low$psv_p80 * 0.10, tolerance = 1e-10)
})

test_that("calculate_psv_components emits *_p80 columns and osv/dsv per-game (issue #80)", {
  stats <- data.table::data.table(
    player_id = c("P1", "P2"),
    player_name = c("Alice", "Bob"),
    season = 2025L, round = 1L,
    match_id = c("M1", "M1"),
    lineup_position = c("MID", "FWD"),
    tog = c(0.50, 1.00),
    kicks = c(6, 10),
    tackles = c(2, 4)
  )

  margin_coef <- data.frame(stat_name = c("kicks", "tackles"), beta = c(0.5, 0.3))
  off_coef <- data.frame(stat_name = c("kicks", "tackles"), beta = c(0.8, 0.1))
  def_coef <- data.frame(stat_name = c("kicks", "tackles"), beta = c(0.1, 0.7))

  result <- calculate_psv_components(stats, margin_coef, off_coef, def_coef,
                                      center = TRUE)

  expect_true(all(c("psv_p80", "osv_p80", "dsv_p80",
                    "psv", "osv", "dsv") %in% names(result)))
  # Decomposition holds on both scales.
  expect_equal(result$osv_p80 + result$dsv_p80, result$psv_p80, tolerance = 1e-10)
  expect_equal(result$osv + result$dsv, result$psv, tolerance = 1e-10)
  # Per-game columns are *_p80 * tog.
  expect_equal(result$osv, result$osv_p80 * result$tog, tolerance = 1e-10)
  expect_equal(result$dsv, result$dsv_p80 * result$tog, tolerance = 1e-10)
})

test_that("calculate_psv excludes efficiency stats, bounces, and availability stats", {
  stats <- data.table::data.table(
    player_id = "P1",
    player_name = "Alice",
    season = 2025L, round = 1L,
    match_id = "M1",
    tog = 1.0,
    kicks = 10,
    disposal_efficiency = 70,
    goal_accuracy = 50,
    bounces = 5,
    cond_tog = 0.8
  )

  # Coefficients for all stats including excluded ones
  coef_df <- data.frame(
    stat_name = c("kicks", "disposal_efficiency", "goal_accuracy", "bounces", "cond_tog"),
    beta = c(1, 1, 1, -1, 1),
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, center = FALSE)

  # Only kicks (10/1.0 = 10) should contribute — all others excluded
  expect_equal(result$psv, 10 * PSV_POINTS_SCALE)
})
