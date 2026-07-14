# torpverse/docs/plans/FABLE-RECAL-PLAN.md Step 3 -- WP recalibration applied in get_wp_preds()
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

test_that("(e) interaction form: serve-time application matches the fit-side formula on both cell sides", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df(n = 12)
  # Explicit cell mix: rows 1-4 in cell (Q4, close), 5-8 out by period,
  # 9-12 out by margin. points_diff is a WP feature, so raw preds shift
  # too -- expected values are computed from the same df's raw preds.
  df$period <- rep(c(4L, 4L, 1L, 3L, 4L, 4L), 2)
  df$points_diff <- rep(c(3, -12, 5, 0, 13, -40), 2)
  in_cell <- df$period == 4 & abs(df$points_diff) <= 12

  calib <- list(a = 0.1, b = 1.2, a_q4c = 0.3, b_q4c = 0.25,
                form = "q4close_interaction",
                cell = list(period = 4, margin_abs_max = 12))
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  raw <- .wp_calib_raw_preds(fixture, df)
  I <- as.numeric(in_cell)
  expected <- stats::plogis((0.1 + 0.3 * I) + (1.2 + 0.25 * I) * stats::qlogis(raw))
  got <- torp:::get_wp_preds(df)

  expect_equal(got$wp, expected)
  # the two arms genuinely differ (interaction did something in-cell only)
  global_only <- stats::plogis(0.1 + 1.2 * stats::qlogis(raw))
  expect_equal(got$wp[!in_cell], global_only[!in_cell])
  expect_false(isTRUE(all.equal(got$wp[in_cell], global_only[in_cell])))
})

test_that("(f) NA period/points_diff rows get the global arm; a df without those columns is all-global", {
  fixture <- .wp_calib_test_fixture_model()
  calib <- list(a = 0.1, b = 1.2, a_q4c = 0.3, b_q4c = 0.25,
                form = "q4close_interaction",
                cell = list(period = 4, margin_abs_max = 12))
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  # NA rows -> global arm. period is NOT a WP feature (points_diff is), so
  # NA period never reaches the model matrix; NA points_diff would, so we
  # only inject NA period here and keep points_diff in-cell-valued.
  df <- .wp_calib_test_mock_df(n = 8)
  df$points_diff <- rep(2, 8)
  df$period <- c(4L, NA, 4L, NA, 4L, NA, 4L, NA)
  raw <- .wp_calib_raw_preds(fixture, df)
  got <- torp:::get_wp_preds(df)
  global_only <- stats::plogis(0.1 + 1.2 * stats::qlogis(raw))
  in_only <- stats::plogis((0.1 + 0.3) + (1.2 + 0.25) * stats::qlogis(raw))
  expect_equal(got$wp[is.na(df$period)], global_only[is.na(df$period)])
  expect_equal(got$wp[!is.na(df$period)], in_only[!is.na(df$period)])

  # df missing the period column entirely -> every row global
  df2 <- .wp_calib_test_mock_df(n = 6)
  df2$period <- NULL
  raw2 <- .wp_calib_raw_preds(fixture, df2)
  got2 <- torp:::get_wp_preds(df2)
  expect_equal(got2$wp, stats::plogis(0.1 + 1.2 * stats::qlogis(raw2)))
})

test_that("(g) explicit-zero interaction fields and absent cell spec behave as global with default cell", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df(n = 10)

  # a_q4c = b_q4c = 0 (the global form as shipped by the trainer) must be
  # numerically identical to the plain 2-param application
  calib_zero <- list(a = -0.05, b = 1.15, a_q4c = 0, b_q4c = 0, form = "global",
                     cell = list(period = 4, margin_abs_max = 12))
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib_zero)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )
  raw <- .wp_calib_raw_preds(fixture, df)
  got <- torp:::get_wp_preds(df)
  expect_equal(got$wp, stats::plogis(-0.05 + 1.15 * stats::qlogis(raw)))

  # interaction fields present but NO cell spec -> defaults (4, 12) used
  calib_nocell <- list(a = 0, b = 1, a_q4c = 0.2, b_q4c = 0.1, form = "q4close_interaction")
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib_nocell)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )
  df$period <- rep(c(4L, 2L), 5)
  df$points_diff <- rep(1, 10)
  raw <- .wp_calib_raw_preds(fixture, df)
  I <- as.numeric(df$period == 4)
  got <- torp:::get_wp_preds(df)
  expect_equal(got$wp, stats::plogis(0.2 * I + (1 + 0.1 * I) * stats::qlogis(raw)))
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
