# The two shared arithmetic rules behind every rolling opponent profile.
#
# These were extracted from three copies of the shrinkage line and four of the
# decay line. The extraction's whole claim is that published numbers are
# BIT-IDENTICAL, not merely close -- so these tests use identical(), not
# expect_equal()'s tolerance. A helper that agrees to 1e-10 with the expression
# it replaced would still have changed every rating in the last decimal place,
# and "close enough" is not what was promised.

test_that(".decay_weight() is identical to the expression it replaced", {
  set.seed(1)
  days <- c(0, 1, 7, 365, 1000, stats::runif(200, 0, 3000))
  for (lambda in c(0, 0.003, 0.05, 1)) {
    expect_identical(.decay_weight(days, lambda), exp(-lambda * days))
  }
})

test_that(".shrink_to_league() is identical to the expression it replaced", {
  set.seed(2)
  n <- 500
  wt_sum <- stats::runif(n, 0, 50)
  wt_mean <- stats::rnorm(n, 20, 10)
  league <- stats::rnorm(n, 20, 5)
  for (prior_games in c(0, 5, 10000)) {
    expect_identical(
      .shrink_to_league(wt_sum, wt_mean, league, prior_games),
      (wt_sum * wt_mean + prior_games * league) / (wt_sum + prior_games)
    )
  }
})

test_that("prior_games = 0 returns the unshrunk weighted mean exactly", {
  # Not approximately: (w*m + 0*L)/(w + 0) is w*m/w. Any caller passing 0 must
  # get its own mean back untouched.
  expect_identical(.shrink_to_league(4, 7.5, 99, 0), 7.5)
  expect_identical(.shrink_to_league(1e-9, -3.25, 1000, 0), -3.25)
})

test_that("shrinkage moves monotonically toward the league average", {
  # The direction the whole adjustment depends on. A team above the league mean
  # must be pulled DOWN and never past it, and vice versa.
  above <- .shrink_to_league(wt_sum = 3, wt_mean = 40, league_avg = 25, prior_games = 5)
  below <- .shrink_to_league(wt_sum = 3, wt_mean = 10, league_avg = 25, prior_games = 5)
  expect_lt(above, 40); expect_gt(above, 25)
  expect_gt(below, 10); expect_lt(below, 25)

  # More prior = closer to league, strictly.
  seq_priors <- c(0, 1, 5, 50, 5000)
  vals <- vapply(seq_priors, function(p) .shrink_to_league(3, 40, 25, p), numeric(1))
  expect_true(all(diff(vals) < 0))
  expect_lt(abs(vals[length(vals)] - 25), 0.05)
})

test_that("weight, not game count, is what buys confidence", {
  # Two teams with the same mean but different total weight: the lighter one
  # is pulled harder toward the league. This is the property the docstring
  # claims and the reason shrinkage keys on weight.
  heavy <- .shrink_to_league(wt_sum = 20, wt_mean = 40, league_avg = 25, prior_games = 5)
  light <- .shrink_to_league(wt_sum = 1, wt_mean = 40, league_avg = 25, prior_games = 5)
  expect_gt(heavy, light)
})

test_that("zero total weight yields the league average, not NaN", {
  # A team whose prior games all decayed to nothing has no evidence. The
  # callers guard this themselves before calling, but the rule should still be
  # well defined rather than 0/0.
  expect_identical(.shrink_to_league(0, 40, 25, 5), 25)
})

test_that("lambda_decay = 0 makes every weight exactly 1", {
  # The property the characterisation tests in
  # test-opponent-adjustment-profiles.R rely on to keep their expected values
  # hand-computable. If this ever stopped holding, those expectations would
  # silently become wrong rather than failing.
  expect_identical(.decay_weight(c(0, 5, 500, 1e6), 0), rep(1, 4))
})
