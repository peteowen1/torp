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
  expect_true(res$cold_start)
  expect_true(is.na(res$holdout_season))
})

# ---- n_oos MATCH count regression (BUG 1 + BUG 2) -------------------------
# team_mdl_df is long -- 2 rows per match (home + away). n_oos must mean
# MATCH count everywhere it's compared against MATCH_RECAL_MIN_N (the
# cold-start guard) and in the n_oos >= 2 threshold that gates slope_raw --
# not the raw OOS row count. .train_match_gams/.train_match_xgb are mocked
# out (fast, deterministic, no real GAM/XGBoost training).

.match_calib_mock_df <- function(n_train_matches, n_holdout_matches,
                                  train_season = 2024L, holdout_season = 2025L) {
  mk <- function(n, season) {
    data.frame(
      season.x   = rep(season, n * 2),
      win        = rep(c(1, 0), n),
      score_diff = rep(c(10, -10), n)
    )
  }
  rbind(mk(n_train_matches, train_season), mk(n_holdout_matches, holdout_season))
}

# Deterministic mock training: pred_margin = score_diff / slope, so the
# fitted OOS slope (margin ~ pred_margin, through actual score_diff) comes
# out to exactly `slope` with zero residual -- no real model fit needed.
.match_calib_mock_train <- function(slope = 1) {
  list(
    gams = function(team_mdl_df, train_filter = NULL, ...) {
      team_mdl_df$gam_pred_score_diff <- team_mdl_df$score_diff / slope
      list(models = list(), data = team_mdl_df)
    },
    xgb = function(team_mdl_df, train_filter = NULL, ...) {
      team_mdl_df$xgb_pred_score_diff <- team_mdl_df$gam_pred_score_diff
      list(models = list(), data = team_mdl_df)
    }
  )
}

test_that("fit_match_margin_calibration: 20 holdout matches (40 OOS rows) below MATCH_RECAL_MIN_N=30 must cold-start on the MATCH count", {
  mocks <- .match_calib_mock_train(slope = 1)
  testthat::local_mocked_bindings(
    .train_match_gams = mocks$gams,
    .train_match_xgb = mocks$xgb,
    .package = "torp"
  )
  df <- .match_calib_mock_df(n_train_matches = 50, n_holdout_matches = 20)

  expect_warning(res <- fit_match_margin_calibration(df), "identity fallback")

  # 40 OOS rows but only 20 matches -- the OLD (buggy) row-count comparison
  # (40 >= 30) would NOT have cold-started here.
  expect_equal(res$n_oos, 20)
  expect_true(res$cold_start)
  expect_equal(res$b, 1)

  # slope_raw is still computable (>= 2 matches) and, on this perfect-fit
  # mock, lands exactly at 1 -- well inside MATCH_MARGIN_SLOPE_GATE. This is
  # BUG 2's concrete failure mode: a coincidentally in-range raw slope
  # alongside a cold-start identity fit. match_model.R's upload gate
  # (R/match_model.R ~L939-947) must refuse to upload despite slope_ok,
  # because cold_start is TRUE -- replicate that exact gate expression here.
  expect_equal(res$slope_raw, 1, tolerance = 1e-8)
  gate <- MATCH_MARGIN_SLOPE_GATE
  slope_ok <- is.na(res$slope_raw) || (res$slope_raw >= gate[1] && res$slope_raw <= gate[2])
  expect_true(slope_ok)
  should_upload <- !isTRUE(res$cold_start) && slope_ok
  expect_false(should_upload)
})

test_that("fit_match_margin_calibration: n_oos >= 2 threshold for slope_raw is also a MATCH count (1 match/2 rows -> NA slope, not a fitted one)", {
  mocks <- .match_calib_mock_train(slope = 1)
  testthat::local_mocked_bindings(
    .train_match_gams = mocks$gams,
    .train_match_xgb = mocks$xgb,
    .package = "torp"
  )
  df <- .match_calib_mock_df(n_train_matches = 50, n_holdout_matches = 1)

  expect_warning(res <- fit_match_margin_calibration(df), "identity fallback")

  expect_equal(res$n_oos, 1)
  expect_true(res$cold_start)
  expect_equal(res$b, 1)
  # 2 OOS rows but only 1 match -- the OLD (buggy) row-count comparison
  # (2 >= 2) would have attempted the lm() fit here.
  expect_true(is.na(res$slope_raw))
})

test_that("fit_match_margin_calibration: exactly MATCH_RECAL_MIN_N holdout matches is not a cold start and fits a real (non-identity) slope", {
  mocks <- .match_calib_mock_train(slope = 0.9)
  testthat::local_mocked_bindings(
    .train_match_gams = mocks$gams,
    .train_match_xgb = mocks$xgb,
    .package = "torp"
  )
  df <- .match_calib_mock_df(n_train_matches = 50, n_holdout_matches = MATCH_RECAL_MIN_N)

  res <- fit_match_margin_calibration(df)

  expect_equal(res$n_oos, MATCH_RECAL_MIN_N)
  expect_false(res$cold_start)
  expect_equal(res$b, 0.9, tolerance = 1e-8)
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
