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
  local_mocked_bindings(load_predictions = function(...) stop("offline"),
                        .load_honest_backfill_predictions = function(...) stop("offline"))
  expect_true(is.na(suppressWarnings(suppressMessages(torp:::.heldout_margin_sigma()))))
})

test_that(".heldout_margin_sigma uses live-locked rows plus the honest backfill, not the hindsight rows", {
  set.seed(5)
  honest <- data.frame(pred_margin = rnorm(120, 0, 20))
  honest$margin <- honest$pred_margin + rnorm(120, 0, 33)
  locked <- data.frame(pred_margin = rnorm(60, 0, 20))
  locked$margin <- locked$pred_margin + rnorm(60, 0, 30)
  locked$margin[1:5] <- NA                              # not played yet
  asked <- NULL
  local_mocked_bindings(
    .load_honest_backfill_predictions = function(...) honest,
    get_afl_season = function(...) 2027L,
    load_predictions = function(seasons, ...) { asked <<- seasons; locked }
  )
  both <- rbind(locked, honest)
  expect_equal(torp:::.heldout_margin_sigma(), torp:::.residual_sd(both$margin, both$pred_margin))
  expect_identical(asked, LIVE_LOCK_FIRST_SEASON:2027L)   # no pre-live-lock season requested
})

test_that(".heldout_margin_sigma warns and falls back to every predictions row when the backfill cannot be read", {
  set.seed(6)
  p <- data.frame(pred_margin = rnorm(150, 0, 20))
  p$margin <- p$pred_margin + rnorm(150, 0, 32)
  asked <- NULL
  local_mocked_bindings(
    .load_honest_backfill_predictions = function(...) stop("404"),
    load_predictions = function(seasons, ...) { asked <<- seasons; p }
  )
  expect_warning(s <- torp:::.heldout_margin_sigma(), "Honest backfill predictions could not be read")
  expect_true(isTRUE(asked))
  expect_equal(s, stats::sd(p$margin - p$pred_margin))
})

test_that(".load_honest_backfill_predictions reads only pre-live-lock backfill files, and errors without them", {
  urls <- NULL
  local_mocked_bindings(
    get_release_assets = function(...) c("predictions_2024.parquet", "backfill_predictions_2023.parquet",
                                         "backfill_predictions_2022.parquet", "backfill_predictions_2022.csv",
                                         paste0("backfill_predictions_", LIVE_LOCK_FIRST_SEASON, ".parquet")),
    load_from_url = function(url, ...) { urls <<- url; data.frame(margin = 1, pred_margin = 0) }
  )
  torp:::.load_honest_backfill_predictions()
  expect_identical(basename(urls), c("backfill_predictions_2022.parquet", "backfill_predictions_2023.parquet"))
  expect_true(all(grepl("/predictions/backfill_predictions_", urls, fixed = TRUE)))

  local_mocked_bindings(get_release_assets = function(...) "predictions_2024.parquet")
  expect_error(torp:::.load_honest_backfill_predictions(), "no")
  local_mocked_bindings(get_release_assets = function(...) NULL)
  expect_error(torp:::.load_honest_backfill_predictions(), "listing")
  local_mocked_bindings(get_release_assets = function(...) "backfill_predictions_2022.parquet",
                        load_from_url = function(...) data.frame())
  expect_error(torp:::.load_honest_backfill_predictions(), "empty")
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
