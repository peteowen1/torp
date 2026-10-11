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

test_that("without a usable sigma the finals rows use the documented fallback, never the win model's value (torp#252)", {
  preds <- data.frame(season = 2026L, round = 26L, pred_margin = -6.6, pred_win = 0.496)
  out <- suppressMessages(torp:::.finals_win_from_margin(preds, sigma = NA_real_))
  expect_equal(out$pred_win, stats::pnorm(-6.6 / FINALS_MARGIN_SIGMA_FALLBACK))
  expect_lt(out$pred_win, 0.5)
})

test_that("the default sigma is the held-out spread, sized only when there are finals", {
  calls <- 0L
  local_mocked_bindings(.heldout_margin_sigma = function(...) { calls <<- calls + 1L; 40 })
  ha <- data.frame(season = 2026L, round = 10L, pred_margin = 12, pred_win = 0.7)
  expect_identical(suppressMessages(torp:::.finals_win_from_margin(ha))$pred_win, 0.7)
  expect_identical(calls, 0L)
  fin <- data.frame(season = 2026L, round = 26L, pred_margin = 12, pred_win = 0.7)
  out <- suppressMessages(torp:::.finals_win_from_margin(fin))
  expect_equal(out$pred_win, stats::pnorm(12 / 40))
  expect_identical(calls, 1L)
})

test_that("seasons before the round map are skipped, not guessed (2020 had 17 H&A rounds)", {
  preds <- data.frame(season = c(2020L, 2026L), round = c(20L, 26L),
                      pred_margin = c(10, 10), pred_win = c(0.55, 0.55))
  expect_warning(out <- suppressMessages(torp:::.finals_win_from_margin(preds, sigma = 33)),
                 "2020")
  expect_identical(out$pred_win[1], 0.55)
  expect_equal(out$pred_win[2], stats::pnorm(10 / 33))
})

test_that(".heldout_margin_sigma sizes the spread from completed locked predictions only", {
  set.seed(4)
  p <- data.frame(pred_margin = rnorm(150, 0, 20))
  p$margin <- p$pred_margin + rnorm(150, 0, 32)
  p$margin[1:20] <- NA                                  # not played yet
  expect_equal(torp:::.heldout_margin_sigma(p),
               stats::sd(p$margin[21:150] - p$pred_margin[21:150]))
  expect_true(is.na(torp:::.heldout_margin_sigma(p[1:110, ])))   # 90 completed < 100
  expect_true(is.na(torp:::.heldout_margin_sigma(data.frame(x = 1))))
})

test_that(".heldout_margin_sigma returns NA, not an error, when the load fails", {
  local_mocked_bindings(load_predictions = function(...) stop("offline"))
  expect_true(is.na(suppressMessages(torp:::.heldout_margin_sigma())))
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
