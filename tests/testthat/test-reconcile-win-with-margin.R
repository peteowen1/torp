test_that("a win probability that contradicts the margin is derived from the margin; others are untouched", {
  set.seed(1)
  n <- 200
  done <- data.frame(pred_score_diff = rnorm(n, 0, 20), score_diff = NA_real_, pred_win = NA_real_)
  done$score_diff <- done$pred_score_diff + rnorm(n, 0, 30)
  done$pred_win <- stats::plogis(done$pred_score_diff / 15)       # agrees everywhere
  # 2026 R29's shape: margin -10.4, win model 0.540
  bad <- data.frame(pred_score_diff = c(-10.4, 10.4), score_diff = NA_real_, pred_win = c(0.540, 0.460))
  df <- rbind(done, bad)
  out <- torp:::.reconcile_win_with_margin(df)
  expect_equal(sum(out$pred_win_from_margin), 2L)
  expect_identical(out$pred_win[1:n], df$pred_win[1:n])         # nothing else moves
  expect_lt(out$pred_win[n + 1], 0.5)                           # now agrees with -10.4
  expect_gt(out$pred_win[n + 2], 0.5)
  expect_equal(out$pred_win[n + 1] + out$pred_win[n + 2], 1, tolerance = 1e-12)
})

test_that("with too few completed matches to size the spread it leaves the rows for the validation", {
  df <- data.frame(pred_score_diff = c(-10.4, 10.4), score_diff = NA_real_, pred_win = c(0.54, 0.46))
  expect_warning(out <- torp:::.reconcile_win_with_margin(df), "Cannot size")
  expect_identical(out$pred_win, df$pred_win)
})
