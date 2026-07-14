# torpverse/docs/plans/FABLE-MATCH-MAE-PLAN.md WS1 "V1a" -- match margin
# recalibration sidecar. Network-free.

test_that("apply_match_margin_calibration: NULL calib is identity", {
  pred <- c(-10, 0, 25.5)
  expect_identical(apply_match_margin_calibration(pred, NULL), pred)
})

test_that("apply_match_margin_calibration: scales by b", {
  pred <- c(-10, 0, 25.5)
  calib <- list(b = 0.8)
  expect_equal(apply_match_margin_calibration(pred, calib), 0.8 * pred)
})

test_that("apply_match_margin_calibration: non-finite or missing b falls back to identity", {
  pred <- c(-10, 0, 25.5)
  expect_identical(apply_match_margin_calibration(pred, list(b = NA_real_)), pred)
  expect_identical(apply_match_margin_calibration(pred, list(b = Inf)), pred)
  expect_identical(apply_match_margin_calibration(pred, list()), pred)
})

test_that("fit_match_margin_calibration: fewer than 2 completed seasons -> identity fallback, no training attempted", {
  team_mdl_df <- data.frame(
    season.x = c(2026L, 2026L),
    win = c(1, 0)
  )
  res <- fit_match_margin_calibration(team_mdl_df)
  expect_equal(res$b, 1)
  expect_equal(res$n_oos, 0L)
  expect_true(is.na(res$holdout_season))
})

test_that("load_match_margin_calibration: torpmodels error degrades to NULL, not an error", {
  testthat::local_mocked_bindings(
    load_torp_model = function(...) stop("simulated 404"),
    .package = "torpmodels"
  )
  if (exists("match_margin_calibration", envir = torp:::.torp_model_cache)) {
    rm("match_margin_calibration", envir = torp:::.torp_model_cache)
  }
  expect_warning(res <- load_match_margin_calibration(), "unavailable")
  expect_null(res)
})
