test_that(".dirichlet_split preserves the total in every sim", {
  set.seed(2)
  totals <- stats::rpois(20000, 20)
  m <- .dirichlet_split(totals, c(1, 1, 1, 1), length(totals))
  expect_equal(unname(rowSums(m)), totals)
  expect_true(all(m >= 0L))
})

test_that("cumulative scores in sim events match a naive loop", {
  set.seed(11)
  r <- .sim_scoring_sequence(estimate = 5, pred_total = 160, home_conv = 0.5,
                             away_conv = 0.5, n_sims = 200)
  ev <- data.table::copy(r$events)
  ref <- ev[, .recompute_cumulative(team, score_value), by = sim_id]
  expect_identical(ev$cum_home, ref$cum_home)
  expect_identical(ev$cum_away, ref$cum_away)
})
