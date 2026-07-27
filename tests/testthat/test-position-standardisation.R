# Tests for the EPV position-variance standardisation and the corrected
# lineup-position taxonomy (FABLE-DEFENDER-VALUE-PLAN §7.15, §7.18).

test_that(".wtd_sd matches a hand-computed weighted SD", {
  x <- c(1, 2, 3, 4)
  w <- c(1, 1, 1, 1)
  expect_equal(.wtd_sd(x, w), sqrt(mean((x - mean(x))^2)))
  # unequal weights: the heavy observation dominates the mean
  expect_lt(.wtd_sd(c(0, 10), c(99, 1)), .wtd_sd(c(0, 10), c(1, 1)))
  expect_true(is.na(.wtd_sd(c(NA_real_, NA_real_), c(1, 1))))
})

test_that(".position_adjust centres when standardise is FALSE", {
  p80 <- c(1, 2, 3, 6)
  tog <- rep(1, 4)
  out <- .position_adjust(p80, tog, pooled_sd = 5, standardise = FALSE)
  expect_equal(out, (p80 - mean(p80)) * tog)
  # centring alone leaves the group's weighted mean at zero
  expect_equal(sum(out), 0, tolerance = 1e-10)
})

test_that(".position_adjust rescales to the pooled SD when standardise is TRUE", {
  p80 <- c(1, 2, 3, 6)
  tog <- rep(1, 4)
  pooled <- 5
  out <- .position_adjust(p80, tog, pooled_sd = pooled, standardise = TRUE)
  # still centred
  expect_equal(sum(out), 0, tolerance = 1e-10)
  # and now carries the POOLED spread rather than its own
  expect_equal(.wtd_sd(out / tog, tog), pooled, tolerance = 1e-8)
})

test_that(".position_adjust falls back to centring on a degenerate SD", {
  # This is the hitout failure mode: a position with no within-group variance
  # would otherwise divide by ~zero and amplify without bound.
  p80 <- rep(2, 5)
  tog <- rep(1, 5)
  out <- .position_adjust(p80, tog, pooled_sd = 1.24, standardise = TRUE)
  expect_true(all(is.finite(out)))
  expect_equal(out, rep(0, 5))
})

test_that("hitout is excluded from standardisation", {
  # The exclusion is the guard against the ruck blow-up, so assert it directly
  # rather than trusting the constant to stay put.
  expect_false("hitout" %in% EPV_STANDARDISE_CHANNELS)
  expect_setequal(EPV_STANDARDISE_CHANNELS, c("recv", "disp", "spoil"))
})

test_that("the corrected lineup map fixes the three audited assignments", {
  m <- LINEUP_POSITION_GROUP_MAP
  # centre half forward is a key post, the forward pockets are not
  expect_equal(unname(m[["CHF"]]), "KEY_FORWARD")
  expect_equal(unname(m[["FPL"]]), "MEDIUM_FORWARD")
  expect_equal(unname(m[["FPR"]]), "MEDIUM_FORWARD")
  # and the two central defensive posts are grouped
  expect_equal(unname(m[["FB"]]), "KEY_DEFENDER")
  expect_equal(unname(m[["CHB"]]), "KEY_DEFENDER")
})

test_that("the lineup map is symmetric between the two ends", {
  m <- LINEUP_POSITION_GROUP_MAP
  onfield <- m[!is.na(m)]
  expect_equal(sum(onfield == "KEY_DEFENDER"), sum(onfield == "KEY_FORWARD"))
  expect_equal(sum(onfield == "MEDIUM_DEFENDER"), sum(onfield == "MEDIUM_FORWARD"))
  # bench codes must stay NA so they fall through to modal resolution
  expect_true(all(is.na(m[c("INT", "SUB", "EMERG")])))
})

test_that(".add_lineup_pos_group maps weekly roles and leaves bench rows NA", {
  dt <- data.table::data.table(
    player_id = c("a", "b", "c", "d"),
    lineup_position = c("CHF", "FPL", "INT", "CHB")
  )
  out <- .add_lineup_pos_group(data.table::copy(dt))
  expect_equal(out$lineup_pos_group,
               c("KEY_FORWARD", "MEDIUM_FORWARD", NA_character_, "KEY_DEFENDER"))
  # absent source column is a no-op, not an error
  bare <- data.table::data.table(player_id = "a")
  expect_identical(.add_lineup_pos_group(bare), bare)
})

test_that("calculate_psr prefers the weekly lineup group over pos_group", {
  # Two players with identical raw profiles but different weekly roles must be
  # centred against different groups; that is the whole point of §7.15.
  skills <- data.frame(
    player_id = c("a", "b", "c", "d"),
    season = 2025L, round = 1L,
    pos_group = "MIDFIELDER",                       # season label: all the same
    lineup_pos_group = c("KEY_DEFENDER", "KEY_DEFENDER",
                         "KEY_FORWARD", "KEY_FORWARD"),
    wt_80s = 1,
    disposals_rating = c(1, 3, 10, 12)
  )
  coefs <- data.frame(stat_name = "disposals", beta = 1)
  out <- calculate_psr(skills, coefs, center = TRUE)
  # centred within the WEEKLY group, so each pair sums to zero
  expect_equal(sum(out$psr[1:2]), 0, tolerance = 1e-10)
  expect_equal(sum(out$psr[3:4]), 0, tolerance = 1e-10)
  # had it centred on pos_group, the forwards would both be strongly positive
  expect_lt(out$psr[3], out$psr[4])
})
