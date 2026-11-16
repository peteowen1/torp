# Tests for add_contest_vars_dt().
#
# The compute_contest_credit() tests that shared this file were removed with
# the v2 EPV engine on 2026-11-16.

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
