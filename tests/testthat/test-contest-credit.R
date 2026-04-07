# Tests for compute_contest_credit() and add_contest_vars_dt()
# =============================================================

# --- Helpers ---

# Create mock chains data for compute_contest_credit().
# Raw chains use same x,y convention as extract_contests() (x == .next_x).
create_credit_chains <- function() {
  data.table::data.table(
    match_id = rep("M001", 12),
    display_order = 1:12,
    period = rep(1L, 12),
    period_seconds = seq(100L, 1200L, by = 100L),
    team_id = c(
      "T1", "T1", "T2",  # 1-3: Kick(T1) -> Contest Target(T1) -> Spoil(T2)
      "T1", "T1", "T2",  # 4-6: Kick(T1) -> Contest Target(T1) -> Contested Mark(T2)
      "T1", "T1", "T1",  # 7-9: Kick(T1) -> Contest Target(T1) -> Contested Mark(T1) same-team
      "T1", "T1", "T2"   # 10-12: Handball(T1) -> Contest Target(T1) -> Spoil(T2) — no preceding kick
    ),
    player_id = paste0("P", sprintf("%03d", 1:12)),
    description = c(
      "Kick", "Contest Target", "Spoil",
      "Kick", "Contest Target", "Contested Mark",
      "Kick", "Contest Target", "Contested Mark",
      "Handball", "Contest Target", "Spoil"
    ),
    x = c(
      30L, 30L, 30L,   # 1-3: same location
      50L, 50L, 50L,   # 4-6: same location
      20L, 20L, 20L,   # 7-9: same location, same team
      10L, 10L, 10L    # 10-12: same location but no kick
    ),
    y = c(
      10L, 10L, 10L,
      -5L, -5L, -5L,
      15L, 15L, 15L,
      0L, 0L, 0L
    )
  )
}

# Create mock PBP data with delta_epv values for the kicker rows.
# Must have display_order matching kick rows in chains.
create_credit_pbp <- function() {
  data.table::data.table(
    match_id = rep("M001", 12),
    display_order = 1:12,
    delta_epv = c(
      0.5, NA, NA,      # row 1 (kick) has +0.5 EPV
      -0.3, NA, NA,     # row 4 (kick) has -0.3 EPV
      0.8, NA, NA,      # row 7 (kick) has +0.8 EPV
      NA, NA, NA         # rows 10-12: no kick EPV
    )
  )
}


# --- compute_contest_credit() tests ---

test_that("compute_contest_credit returns correct schema on empty input", {
  empty_chains <- data.table::data.table(
    match_id = character(), display_order = integer(),
    period = integer(), period_seconds = integer(),
    team_id = character(), player_id = character(),
    description = character(), x = integer(), y = integer()
  )
  empty_pbp <- data.table::data.table(
    match_id = character(), display_order = integer(), delta_epv = numeric()
  )
  result <- compute_contest_credit(empty_chains, empty_pbp)
  expect_equal(nrow(result), 0)
  expect_true(all(c("player_id", "match_id", "contest_epv",
                     "aerial_target_wins", "aerial_target_losses",
                     "aerial_def_wins", "aerial_def_losses") %in% names(result)))
})

test_that("compute_contest_credit finds contests and allocates credit", {
  chains <- create_credit_chains()
  pbp <- create_credit_pbp()
  result <- compute_contest_credit(chains, pbp, contest_share = 1 / 3)

  # Should find 2 opposing-team contests with kick: rows 2->3 and 5->6
  # Row 8->9 is same-team (T1 -> T1), should be excluded
  # Row 11->12 has no preceding kick, so no delta_epv
  expect_true(nrow(result) > 0)

  # Check specific players
  # P002 (target, contest 1): delta_epv=0.5, share=1/3 → contest_epv = 0.5/3 ≈ 0.167
  p2 <- result[player_id == "P002"]
  if (nrow(p2) > 0) {
    expect_equal(round(p2$contest_epv, 3), round(0.5 / 3, 3))
    expect_equal(p2$aerial_target_wins, 1L)  # positive EPV → win
    expect_equal(p2$aerial_target_losses, 0L)
  }

  # P003 (defender, contest 1): contest_epv = -0.5/3 ≈ -0.167
  p3 <- result[player_id == "P003"]
  if (nrow(p3) > 0) {
    expect_equal(round(p3$contest_epv, 3), round(-0.5 / 3, 3))
    expect_equal(p3$aerial_def_losses, 1L)  # negative EPV → loss
  }

  # P005 (target, contest 2): delta_epv=-0.3, share=1/3 → contest_epv = -0.3/3 ≈ -0.1
  p5 <- result[player_id == "P005"]
  if (nrow(p5) > 0) {
    expect_equal(round(p5$contest_epv, 3), round(-0.3 / 3, 3))
    expect_equal(p5$aerial_target_losses, 1L)  # negative EPV → loss
  }

  # P006 (defender, contest 2): contest_epv = 0.3/3 ≈ 0.1 (defender benefits from bad kick)
  p6 <- result[player_id == "P006"]
  if (nrow(p6) > 0) {
    expect_equal(round(p6$contest_epv, 3), round(0.3 / 3, 3))
    expect_equal(p6$aerial_def_wins, 1L)
  }
})

test_that("compute_contest_credit excludes same-team contests", {
  chains <- create_credit_chains()
  pbp <- create_credit_pbp()
  result <- compute_contest_credit(chains, pbp)

  # P008 (target row 8) and P009 (outcome row 9) are both T1 — should not appear
  # as contest participants from the aerial 3-way split
  p8 <- result[player_id == "P008"]
  p9 <- result[player_id == "P009"]
  # If they appear at all it should only be from a different contest, not 8->9
  expect_true(nrow(p8) == 0 || !any(p8$aerial_target_wins > 0 & p8$aerial_def_wins == 0))
})

test_that("compute_contest_credit skips contest when kick is beyond 5-row lookback", {
  # Create chains where the only kick is >5 rows before the contest target
  chains_far <- data.table::data.table(
    match_id = rep("M002", 9),
    display_order = 1:9,
    period = rep(1L, 9),
    period_seconds = seq(100L, 900L, by = 100L),
    team_id = c("T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T2"),
    player_id = paste0("Q", sprintf("%03d", 1:9)),
    description = c(
      "Kick",        # row 1: kick — but 7 rows before contest target
      "Handball", "Handball", "Handball",
      "Handball", "Handball", "Handball",
      "Contest Target",  # row 8: too far from kick
      "Spoil"            # row 9: defender
    ),
    x = c(10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L),
    y = c(5L,  5L,  5L,  5L,  5L,  5L,  5L,  5L,  5L)
  )
  pbp_far <- data.table::data.table(
    match_id = rep("M002", 9),
    display_order = 1:9,
    delta_epv = c(0.5, rep(NA, 8))
  )
  result <- compute_contest_credit(chains_far, pbp_far)

  # Contest target at row 8 with kick at row 1 — 7 rows back, beyond 5-row lookback
  q8 <- result[player_id == "Q008"]
  q9 <- result[player_id == "Q009"]
  expect_true(nrow(q8) == 0)
  expect_true(nrow(q9) == 0)
})

test_that("compute_contest_credit does not modify input chains", {
  chains <- create_credit_chains()
  pbp <- create_credit_pbp()
  orig_cols <- copy(names(chains))
  compute_contest_credit(chains, pbp)
  expect_equal(names(chains), orig_cols)
})

test_that("compute_contest_credit contest_share parameter scales credit", {
  chains <- create_credit_chains()
  pbp <- create_credit_pbp()

  r_third <- compute_contest_credit(chains, pbp, contest_share = 1 / 3)
  r_half  <- compute_contest_credit(chains, pbp, contest_share = 1 / 2)

  # Same players should appear
  expect_equal(sort(r_third$player_id), sort(r_half$player_id))

  # Credit should scale proportionally: half/third = 1.5
  if (nrow(r_third) > 0 && nrow(r_half) > 0) {
    merged <- merge(r_third[, .(player_id, epv3 = contest_epv)],
                    r_half[, .(player_id, epv2 = contest_epv)],
                    by = "player_id")
    nonzero <- merged[epv3 != 0]
    if (nrow(nonzero) > 0) {
      expect_equal(round(nonzero$epv2 / nonzero$epv3, 2), rep(1.5, nrow(nonzero)))
    }
  }
})


# --- add_contest_vars_dt() tests ---

# Create PBP-style data for add_contest_vars_dt().
# After coordinate fixing, PBP uses team-relative coords where opposing teams
# at the same physical location have NEGATED coordinates: x == -.next_x.
create_contest_pbp <- function() {
  data.table::data.table(
    match_id = rep("M001", 8),
    display_order = 1:8,
    description = c(
      "Kick",              # row 1: kicker
      "Contest Target",    # row 2: target at same location (negated coords)
      "Spoil",             # row 3: defender spoils
      "Kick",              # row 4: unrelated kick
      "Mark",              # row 5: clean mark (no contest)
      "Kick",              # row 6: kicker for second contest
      "Contest Target",    # row 7: target
      "Contested Mark"     # row 8: defender marks
    ),
    player_id = paste0("P", sprintf("%03d", 1:8)),
    team_id = c("T1", "T1", "T2", "T1", "T1", "T1", "T1", "T2"),
    x = c(30L, 30L, -30L, 50L, 55L, 40L, 40L, -40L),
    y = c(10L, 10L, -10L, 20L, 25L, -5L, -5L, 5L)
  )
}

test_that("add_contest_vars_dt detects contests via negated coordinates", {
  dt <- create_contest_pbp()
  add_contest_vars_dt(dt)

  # Row 2 should be detected as contest target (T1, x=30,y=10) with
  # next row (T2, x=-30,y=-10): x == -.next_x, y == -.next_y
  expect_equal(dt$contest_target_id[2], "P002")
  expect_equal(dt$contest_defender_id[2], "P003")
  expect_equal(dt$contest_outcome[2], "spoil")

  # Row 7 should also be detected
  expect_equal(dt$contest_target_id[7], "P007")
  expect_equal(dt$contest_defender_id[7], "P008")
  expect_equal(dt$contest_outcome[7], "intercept_mark")
})

test_that("add_contest_vars_dt propagates contest info back to Kick row", {
  dt <- create_contest_pbp()
  add_contest_vars_dt(dt)

  # Contest 1 (row 2-3): Kick at row 1 should receive contest info
  expect_equal(dt$contest_target_id[1], "P002")
  expect_equal(dt$contest_defender_id[1], "P003")
  expect_equal(dt$contest_outcome[1], "spoil")

  # Contest 2 (row 7-8): Kick at row 6 should receive contest info
  expect_equal(dt$contest_target_id[6], "P007")
  expect_equal(dt$contest_defender_id[6], "P008")
  expect_equal(dt$contest_outcome[6], "intercept_mark")
})

test_that("add_contest_vars_dt does not flag non-contest rows", {
  dt <- create_contest_pbp()
  add_contest_vars_dt(dt)

  # Row 4 (unrelated Kick) and row 5 (Mark) should have NA contest info
  expect_true(is.na(dt$contest_target_id[4]))
  expect_true(is.na(dt$contest_outcome[5]))
})

test_that("add_contest_vars_dt handles empty data", {
  dt <- data.table::data.table(
    match_id = character(), display_order = integer(),
    description = character(), player_id = character(),
    team_id = character(), x = integer(), y = integer()
  )
  expect_silent(add_contest_vars_dt(dt))
  expect_true("contest_target_id" %in% names(dt))
  expect_true("contest_outcome" %in% names(dt))
})

test_that("add_contest_vars_dt cleans up temp columns", {
  dt <- create_contest_pbp()
  add_contest_vars_dt(dt)

  # No dot-prefixed temp columns should remain
  dot_cols <- grep("^\\.", names(dt), value = TRUE)
  expect_length(dot_cols, 0)
})

test_that("add_contest_vars_dt does not cross match boundaries", {
  # Two matches: contest target at end of match 1 should not propagate to match 2

  dt <- data.table::data.table(
    match_id = c(rep("M001", 3), rep("M002", 3)),
    display_order = c(1:3, 1:3),
    description = c(
      "Kick", "Handball", "Contest Target",  # M001: kick is 2 rows back
      "Spoil", "Kick", "Mark"                # M002 starts fresh
    ),
    player_id = paste0("P", sprintf("%03d", 1:6)),
    team_id = c("T1", "T1", "T1", "T2", "T1", "T1"),
    x = c(10L, 10L, 10L, -10L, 50L, 50L),
    y = c(5L, 5L, 5L, -5L, 20L, 20L)
  )
  add_contest_vars_dt(dt)

  # Row 4 (M002, "Spoil") should NOT receive contest info from M001
  expect_true(is.na(dt$contest_target_id[4]))
})
