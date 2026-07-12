# FABLE-RECAL-PLAN.md Step 3 -- WP recalibration applied in get_wp_preds()
# (D4). Network-free: both "wp" and "wp_calibration" are mocked via
# load_model_with_fallback(), never hitting torpmodels/the network. The
# "wp" fixture is a tiny xgboost booster built the same way
# torpmodels/tests/testthat/test-model_meta.R builds its fixture, trained
# directly on the exact WP_MODEL_FEATURES column set/order so predict()
# never hits a feature-name mismatch.

.wp_calib_test_mock_df <- function(n = 12) {
  feats <- torp:::WP_MODEL_FEATURES
  set.seed(2026)
  feat_df <- as.data.frame(
    matrix(stats::rnorm(n * length(feats), sd = 1.5), nrow = n,
           dimnames = list(NULL, feats))
  )
  # Binary/indicator columns should look binary, not gaussian
  bin_cols <- c("shot_row", "home", "play_type_handball", "play_type_kick",
               "play_type_reception", "phase_of_play_handball_received",
               "phase_of_play_hard_ball", "phase_of_play_loose_ball",
               "phase_of_play_set_shot")
  for (col in bin_cols) feat_df[[col]] <- rep_len(c(0, 1), n)

  # Extra columns add_wp_vars() needs for its own (unrelated) wpa recurrence
  feat_df$match_id <- "CD_M2024014001"
  feat_df$period <- rep(4L, n)
  feat_df$period_seconds <- seq(100, by = 100, length.out = n)
  feat_df$team_id_mdl <- rep_len(c(1L, 2L), n)
  feat_df
}

.wp_calib_test_fixture_model <- function() {
  testthat::skip_if_not_installed("xgboost")
  feats <- torp:::WP_MODEL_FEATURES
  set.seed(2026)
  train_df <- .wp_calib_test_mock_df(n = 150)[, feats]
  X <- stats::model.matrix(~ . + 0, data = train_df)
  y <- stats::rbinom(150, 1, 0.5)
  xgboost::xgb.train(
    params = list(objective = "binary:logistic", eval_metric = "logloss"),
    data = xgboost::xgb.DMatrix(X, label = y),
    nrounds = 3, verbose = 0
  )
}

.wp_calib_raw_preds <- function(fixture, df) {
  model_data <- df |> torp:::select_wp_model_vars()
  model_matrix <- stats::model.matrix(~ . + 0, data = model_data)
  raw <- stats::predict(fixture, model_matrix)
  if (is.matrix(raw)) raw <- as.vector(raw)
  # get_wp_preds() round-trips through data.frame(wp = preds_raw) and back
  # out via $wp, which drops any names predict() attached -- unname here so
  # comparisons below aren't tripped up by that, not by an actual value diff.
  unname(raw)
}

test_that("(a) identity behaviour when calibration absent -- byte-identical to the raw predict path", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df()

  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(NULL)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  raw <- .wp_calib_raw_preds(fixture, df)
  got <- torp:::get_wp_preds(df)

  expect_identical(got$wp, raw)
})

test_that("(b) with fixture list(a = 0, b = 1.3), output equals plogis(1.3 * qlogis(raw))", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df()

  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(list(a = 0, b = 1.3))
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  raw <- .wp_calib_raw_preds(fixture, df)
  expected <- stats::plogis(1.3 * stats::qlogis(raw))
  got <- torp:::get_wp_preds(df)

  expect_equal(got$wp, expected)
  # and it actually did something (not silently falling back to identity)
  expect_false(isTRUE(all.equal(got$wp, raw)))
})

test_that("(c) calibration preserves the rank order of predictions (monotone transform)", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df(n = 25)

  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(list(a = -0.2, b = 1.4))
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  raw <- .wp_calib_raw_preds(fixture, df)
  got <- torp:::get_wp_preds(df)

  expect_identical(order(raw), order(got$wp))
  # ties in raw must stay tied after calibration too
  expect_identical(rank(raw, ties.method = "min"), rank(got$wp, ties.method = "min"))
})

test_that("(d) WPA regression: wpa is still the calibrated wp lead-difference (no add_wp_vars() code change)", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df(n = 12)

  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(list(a = 0.1, b = 1.2))
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  result <- add_wp_vars(df)

  expect_true(all(result$wp >= 0.001 & result$wp <= 0.999))

  ordered <- result[order(result$period, result$period_seconds), ]
  wp_next <- c(ordered$wp[-1], ordered$wp[nrow(ordered)])
  team_next <- c(ordered$team_id_mdl[-1], ordered$team_id_mdl[nrow(ordered)])
  expected_wpa <- round(ifelse(team_next == ordered$team_id_mdl,
                               wp_next - ordered$wp,
                               (1 - wp_next) - ordered$wp), 5)

  expect_equal(ordered$wpa, expected_wpa)
})

test_that("load_model_with_fallback('wp_calibration') never aborts on a load failure -- warns once, returns NULL", {
  clear_model_cache()
  testthat::local_mocked_bindings(
    load_torp_model = function(model_name, ...) {
      if (identical(model_name, "wp_calibration")) stop("404 Not Found")
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torpmodels"
  )

  expect_warning(
    result1 <- torp:::load_model_with_fallback("wp_calibration"),
    "wp_calibration unavailable"
  )
  expect_null(result1)

  # second call must hit the cached NULL, not warn/retry the network again
  expect_no_warning(result2 <- torp:::load_model_with_fallback("wp_calibration"))
  expect_null(result2)

  clear_model_cache()
})
