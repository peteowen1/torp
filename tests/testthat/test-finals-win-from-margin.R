test_that("finals win probability is increasing in pred_margin and below 0.5 when it is negative (torp#247)", {
  # The issue's rows: -6.6 -> 0.496, +18.4 -> 0.659, +12.1 -> 0.658; plus an
  # H&A row (+21.3 -> 0.731) that must be left alone.
  preds <- data.frame(
    season = 2026L,
    round = c(26L, 27L, 27L, 20L),
    pred_margin = c(-6.6, 18.4, 12.1, 21.3),
    pred_win = c(0.496, 0.659, 0.658, 0.731)
  )
  out <- torp:::.finals_win_from_margin(preds, sigma = 33)
  fin <- out[out$round > 24, ]
  fin <- fin[order(fin$pred_margin), ]
  expect_true(all(diff(fin$pred_win) > 0))
  expect_lt(fin$pred_win[fin$pred_margin < 0], 0.5)
  expect_gt(out$pred_win[out$pred_margin == 18.4], out$pred_win[out$pred_margin == 12.1])
  expect_identical(out$pred_win[out$round == 20], 0.731)
})

test_that("finals win probability follows the margin across a sweep, with a season missing from the round map", {
  m <- seq(-40, 40, by = 2.5)
  preds <- data.frame(season = 2099L, round = 26L, pred_margin = m, pred_win = 0.6)
  out <- torp:::.finals_win_from_margin(preds, sigma = 30)
  expect_true(all(diff(out$pred_win) > 0))
  expect_true(all(out$pred_win[m < 0] < 0.5))
  expect_true(all(out$pred_win[m > 0] > 0.5))
  expect_equal(out$pred_win[m == 0], 0.5)
})

test_that("without a usable sigma the finals rows are left as they were", {
  preds <- data.frame(season = 2026L, round = 26L, pred_margin = -6.6, pred_win = 0.496)
  expect_warning(out <- torp:::.finals_win_from_margin(preds, sigma = NA_real_), "Cannot size")
  expect_identical(out$pred_win, 0.496)
})

test_that(".margin_residual_sd sizes the spread from completed rows only", {
  set.seed(3)
  df <- data.frame(pred_score_diff = rnorm(300, 0, 20))
  df$score_diff <- df$pred_score_diff + rnorm(300, 0, 30)
  df$score_diff[1:50] <- NA
  expect_equal(torp:::.margin_residual_sd(df),
               stats::sd(df$score_diff[51:300] - df$pred_score_diff[51:300]))
  expect_true(is.na(torp:::.margin_residual_sd(df[1:120, ])))   # 70 completed < 100
})
