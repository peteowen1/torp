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

test_that("(d2) §7 acceptance (b): WPA telescopes to (final - opening) calibrated WP on a mock match under the leverage form", {
  # Simplification, documented: real matches switch team_id_mdl on turnover,
  # and add_wp_vars()'s per-row wpa is defined in the ACTING team's own
  # shifting perspective (verified in test (d) above) plus a same-team-only
  # boundary rule for the last row (wpa = 0 there, no code models the final
  # event -> actual W/L transition -- that lives in wp_credit.R, out of
  # scope for this applier). Holding team_id_mdl constant for every row
  # removes the perspective-flip and boundary-transition complications and
  # isolates exactly what this change can affect: does the NEW leverage
  # form's calibrated wp sequence still telescope cleanly (no double-count/
  # sign error introduced by the w-dependent term)? With one team
  # throughout, wpa[i] = wp[i+1] - wp[i] for all but the last row (0 there
  # by construction), so sum(wpa) collapses to wp[n] - wp[1] exactly --
  # "WPA sums to (final - opening) WP" in its simplest, provable form.
  fixture <- .wp_calib_test_fixture_model()
  n <- 15
  df <- .wp_calib_test_mock_df(n = n)
  df$team_id_mdl <- rep(1L, n)   # single team throughout -- no perspective flips
  df$points_diff <- seq(-20, 20, length.out = n)
  df$est_match_remaining <- seq(2000, 100, length.out = n)

  calib <- list(a = 0.05, b = 1.1, c = 0.6, form = "leverage_interaction_v1")
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  result <- add_wp_vars(df)
  ordered <- result[order(result$period, result$period_seconds), ]

  expect_true(all(ordered$team_id_mdl == 1L))
  expect_equal(ordered$wpa[n], 0)   # boundary row: no next event
  expect_equal(sum(ordered$wpa), ordered$wp[n] - ordered$wp[1], tolerance = 1e-8)
})

test_that("(e) leverage_interaction_v1 form: serve-time application matches the fit-side formula", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df(n = 12)
  # Explicit spread of (est_match_remaining, points_diff) so w covers a real
  # range, not just 0/1 -- points_diff is a WP feature (raw preds shift
  # too), est_match_remaining is also a WP feature.
  df$points_diff <- rep(c(3, -12, 5, 0, 13, -40), 2)
  df$est_match_remaining <- rep(c(300, 900, 1800, 0, 600, 4000), 2)
  minutes_remaining <- df$est_match_remaining / 60
  w <- pmax(0, 1 - minutes_remaining / 20) * pmax(0, 1 - abs(df$points_diff) / 18)

  calib <- list(a = 0.1, b = 1.0, c = 0.5, form = "leverage_interaction_v1",
                ramp_mins = 20, margin_cap = 18)
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  raw <- .wp_calib_raw_preds(fixture, df)
  expected <- stats::plogis(0.1 + (1.0 + 0.5 * w) * stats::qlogis(raw))
  got <- torp:::get_wp_preds(df)

  expect_equal(got$wp, expected)
  # w genuinely varies across rows (not silently collapsing to the global arm)
  global_only <- stats::plogis(0.1 + 1.0 * stats::qlogis(raw))
  expect_false(isTRUE(all.equal(got$wp, global_only)))
})

test_that("(f) w spans its full [0, 1] range correctly at the ramp/margin boundaries", {
  # Unlike the retired q4close_interaction form (gated on `period`, which is
  # NOT a WP model feature -- so injecting NA there was safe and left raw
  # preds/row-count untouched), leverage_interaction_v1's `w` is built from
  # points_diff AND est_match_remaining, which ARE both WP_MODEL_FEATURES.
  # An NA in either would already be dropped by model.matrix()'s na.omit
  # default (or abort entirely if the column is missing) before reaching
  # calibration -- so the "NA/missing -> w = 0" defensive branches in
  # get_wp_preds() are unreachable via its normal (df -> preds) contract;
  # kept as belt-and-braces, not exercised here. Instead: verify w's
  # boundary behaviour (0 at/beyond the ramp or margin edge, positive just
  # inside it) end to end.
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df(n = 6)
  df$est_match_remaining <- c(1200, 1199, 0, 300, 300, 300)   # min -> at/just-inside ramp edge
  df$points_diff <- c(5, 5, 5, 18, 17.9, 0)                   # at/just-inside margin edge

  calib <- list(a = 0, b = 1.0, c = 0.6, form = "leverage_interaction_v1")
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  raw <- .wp_calib_raw_preds(fixture, df)
  minutes_remaining <- df$est_match_remaining / 60
  w <- pmax(0, 1 - minutes_remaining / 20) * pmax(0, 1 - abs(df$points_diff) / 18)
  expect_equal(w[c(1, 4)], c(0, 0))          # exactly at the ramp/margin edge -> 0
  expect_true(all(w[c(2, 3, 5, 6)] > 0))     # just inside either edge -> positive

  got <- torp:::get_wp_preds(df)
  expect_equal(got$wp, stats::plogis(0 + (1.0 + 0.6 * w) * stats::qlogis(raw)))
})

test_that("(g) missing form or \"global_v1\" is the legacy arm; leverage form defaults ramp_mins/margin_cap when absent", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df(n = 10)

  # missing $form -> legacy 2-param
  calib_missing <- list(a = -0.05, b = 1.15)
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib_missing)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )
  raw <- .wp_calib_raw_preds(fixture, df)
  got <- torp:::get_wp_preds(df)
  expect_equal(got$wp, stats::plogis(-0.05 + 1.15 * stats::qlogis(raw)))

  # form = "global_v1" -> numerically identical legacy arm
  calib_v1 <- list(a = -0.05, b = 1.15, form = "global_v1")
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib_v1)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )
  got_v1 <- torp:::get_wp_preds(df)
  expect_equal(got_v1$wp, got$wp)

  # leverage form with NO ramp_mins/margin_cap fields -> defaults (20, 18)
  calib_nodefaults <- list(a = 0, b = 1, c = 0.3, form = "leverage_interaction_v1")
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(calib_nodefaults)
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )
  df$points_diff <- rep(1, 10)
  df$est_match_remaining <- rep(300, 10)
  raw <- .wp_calib_raw_preds(fixture, df)
  w <- pmax(0, 1 - (300 / 60) / 20) * pmax(0, 1 - 1 / 18)
  got <- torp:::get_wp_preds(df)
  expect_equal(got$wp, stats::plogis(0 + (1 + 0.3 * w) * stats::qlogis(raw)))
})

test_that("(h) sidecar list missing $a/$b does not crash get_wp_preds() -- identity fallback (recognized form, incomplete fields)", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df()

  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(list(form = "global_v1"))
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )

  raw <- .wp_calib_raw_preds(fixture, df)
  got <- expect_no_error(torp:::get_wp_preds(df))
  expect_identical(got$wp, raw)

  # same for the leverage form
  testthat::local_mocked_bindings(
    load_model_with_fallback = function(model_name) {
      if (model_name == "wp") return(fixture)
      if (model_name == "wp_calibration") return(list(form = "leverage_interaction_v1"))
      stop("unexpected model_name in mock: ", model_name)
    },
    .package = "torp"
  )
  got2 <- expect_no_error(torp:::get_wp_preds(df))
  expect_identical(got2$wp, raw)
})

test_that("(i) an unrecognized sidecar $form aborts loudly -- never silently serves uncalibrated/misapplied WP", {
  fixture <- .wp_calib_test_fixture_model()
  df <- .wp_calib_test_mock_df()

  for (bad_form in c("global", "q4close_interaction", "some_future_form")) {
    testthat::local_mocked_bindings(
      load_model_with_fallback = function(model_name) {
        if (model_name == "wp") return(fixture)
        if (model_name == "wp_calibration") return(list(a = 0, b = 1.2, form = bad_form))
        stop("unexpected model_name in mock: ", model_name)
      },
      .package = "torp"
    )
    expect_error(torp:::get_wp_preds(df), "unrecognized form")
  }
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
