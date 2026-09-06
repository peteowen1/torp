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
