# A match that ends on a scoring shot never gets its terminating Goal/Behind row
# from the feed. `end_of_chain` therefore stays 0 across that closing chain and
# its points are never booked, leaving the running score on the last row 1 or 6
# short of the official result -- 254 of 1,274 matches over 2021-2026 before the
# fix, 0 regressions and 1,263 of 1,274 exact after it.
#
# These tests drive `add_quarter_vars_dt()` directly on minimal frames, because
# the defect lives in one branch and a full clean_pbp fixture would test the
# feed rather than the rule.

.mk <- function(final_state, end_of_chain, description = "Kick",
                team_id = "T1", n_prior = 2L, prior_booked = TRUE) {
  # n_prior rows of an earlier chain that scored (booked at its own end_of_chain),
  # then a closing chain whose last row carries `final_state`.
  data.table::data.table(
    match_id = "M1",
    display_order = seq_len(n_prior + 2L),
    period = 4L,
    description = c(rep("Kick", n_prior - 1L), if (prior_booked) "Goal" else "Kick",
                    "Uncontested Mark", description),
    final_state = c(rep("goal", n_prior), final_state, final_state),
    end_of_chain = c(rep(0L, n_prior - 1L), if (prior_booked) 1L else 0L, 0L, end_of_chain),
    team_id = team_id,
    opp_id = "T2",
    home_team_id = "T1",
    x = 0, y = 0, goal_x = 0, throw_in = 0L,
    stringsAsFactors = FALSE
  )
}

test_that("a closing chain with no Goal row gets its score booked on the last row", {
  for (case in list(list("goal", 6L), list("behind", 1L), list("rushed", 1L))) {
    dt <- .mk(case[[1]], end_of_chain = 0L)
    torp:::add_quarter_vars_dt(dt)
    expect_equal(dt$points_row[nrow(dt)], case[[2]],
                 info = paste("final_state", case[[1]]))
    expect_equal(dt$points_team_id[nrow(dt)], "T1")
  }
})

test_that("a rushedOpp closing chain is credited to the opposition", {
  dt <- .mk("rushedOpp", end_of_chain = 0L)
  torp:::add_quarter_vars_dt(dt)
  expect_equal(dt$points_row[nrow(dt)], 1L)
  expect_equal(dt$points_team_id[nrow(dt)], "T2")
})

test_that("a chain that already booked its score is left alone", {
  # the closing chain terminates properly: end_of_chain 1 on the last row, so the
  # existing rule books it and the fix must not add a second score. This is the
  # regression the first draft of the rule had -- it fired on five matches whose
  # score was already correct.
  dt <- .mk("goal", end_of_chain = 1L, description = "Goal")
  torp:::add_quarter_vars_dt(dt)
  expect_equal(sum(dt$points_row, na.rm = TRUE), 6L + 6L)  # prior chain + this one
  expect_equal(dt$points_row[nrow(dt)], 6L)
})

test_that("a closing chain that did not score books nothing", {
  dt <- .mk("turnover", end_of_chain = 0L)
  torp:::add_quarter_vars_dt(dt)
  expect_true(is.na(dt$points_row[nrow(dt)]))
})

test_that("the fix does not touch end_of_chain or scoring_team_id", {
  # deliberately narrow: both feed EPV features, and the defect is an unbooked
  # score rather than a mis-drawn chain. If a later change starts stamping them,
  # this fails and the blast radius gets re-argued rather than widening quietly.
  dt <- .mk("goal", end_of_chain = 0L)
  torp:::add_quarter_vars_dt(dt)
  expect_equal(dt$end_of_chain[nrow(dt)], 0L)
  expect_true(is.na(dt$scoring_team_id[nrow(dt)]))
})

test_that("an orphan kick-in booked earlier does not suppress the fix", {
  # The guard is chain-scoped, not match-scoped. The orphan kick-in fix above
  # can book a point inside the trailing material WITHOUT closing a chain, and
  # a guard that looked only at the last `end_of_chain` would then treat the
  # rest of the match as already booked and never fire again. A review found
  # this; it does not occur in 2021-2026, so no measurement would have caught
  # it. Shape: chain closes and books (row 2), an orphan kick-in books row 3,
  # then a separate closing chain scores with no terminating row.
  dt <- data.table::data.table(
    match_id = "M1",
    display_order = 1:7,
    period = 4L,
    description = c("Kick", "Goal", "Long Kick", "Kickin", "Handball",
                    "Uncontested Mark", "Kick"),
    final_state = c("goal", "goal", "behind", "behind", "goal", "goal", "goal"),
    end_of_chain = c(0L, 1L, 0L, 0L, 0L, 0L, 0L),
    team_id = "T1", opp_id = "T2", home_team_id = "T1",
    x = 0, y = 0, goal_x = 0, throw_in = 0L,
    stringsAsFactors = FALSE
  )
  torp:::add_quarter_vars_dt(dt)
  expect_equal(dt$points_row[3], 1L)   # the orphan kick-in fix still books this
  expect_equal(dt$points_row[7], 6L)   # and the closing chain is booked too
  expect_equal(dt$points_team_id[7], "T1")
})

test_that("running the pass twice books nothing extra", {
  # Idempotence is the other half of the guard: on a second pass the last row
  # is itself booked, so the boundary equals the last row and the fix declines.
  dt <- .mk("goal", end_of_chain = 0L)
  torp:::add_quarter_vars_dt(dt)
  first <- sum(dt$points_row, na.rm = TRUE)
  torp:::add_quarter_vars_dt(dt)
  expect_equal(sum(dt$points_row, na.rm = TRUE), first)
})

test_that("booking the last row fills pos_points across the closing quarter", {
  # Not a one-row edit: pos_points is a next-observation-carried-backward fill
  # within the quarter, so every previously unbooked row of that quarter picks
  # up the score, and pos_is_goal with it. That is the right value, but it is a
  # change to released columns and is asserted here so it stays deliberate.
  dt <- .mk("goal", end_of_chain = 0L, n_prior = 2L, prior_booked = FALSE)
  torp:::add_quarter_vars_dt(dt)
  expect_true(all(dt$pos_points == 6L))
  # the EP training label must NOT move: scoring_team_id is left alone, so
  # pos_points_team_id stays NA
  expect_true(all(is.na(dt$pos_points_team_id)))
})
