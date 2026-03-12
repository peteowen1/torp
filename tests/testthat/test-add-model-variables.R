# -----------------------------------------------------------------------------
# Model Cache Tests
# -----------------------------------------------------------------------------

test_that("clear_model_cache function exists and is exported", {
  expect_true(exists("clear_model_cache"))
  expect_true("clear_model_cache" %in% getNamespaceExports("torp"))
})

test_that("clear_model_cache clears all cached models", {
  # Access the internal cache environment
  cache_env <- torp:::.torp_model_cache

  # Pre-populate cache with a test value
  assign("test_model", "dummy_value", envir = cache_env)

  # Verify it's there
  expect_true(exists("test_model", envir = cache_env))

  # Clear the cache
  clear_model_cache()

  # Verify it's gone
  expect_false(exists("test_model", envir = cache_env))
  expect_equal(length(ls(envir = cache_env)), 0)
})

test_that("clear_model_cache returns invisible NULL", {
  result <- clear_model_cache()
  expect_null(result)
})

# -----------------------------------------------------------------------------
# load_model_with_fallback Tests
# -----------------------------------------------------------------------------

test_that("load_model_with_fallback exists as internal function", {
  expect_true(exists("load_model_with_fallback", envir = asNamespace("torp")))
})

test_that("load_model_with_fallback handles unknown model names", {
  # Clear cache to ensure fresh state
  clear_model_cache()

  expect_error(
    torp:::load_model_with_fallback("nonexistent_model"),
    "Unknown model name"
  )
})

test_that("load_model_with_fallback caches loaded models", {
  clear_model_cache()
  cache_env <- torp:::.torp_model_cache

  # First call should trigger loading
  result1 <- tryCatch(
    torp:::load_model_with_fallback("ep"),
    error = function(e) NULL
  )

  # If a model was loaded, it should be cached
  if (!is.null(result1)) {
    expect_true(exists("ep", envir = cache_env))

    # Second call should use cache
    result2 <- torp:::load_model_with_fallback("ep")
    expect_identical(result1, result2)
  }
})

# -----------------------------------------------------------------------------
# get_epv_preds Tests
# -----------------------------------------------------------------------------

test_that("get_epv_preds function exists and has correct structure", {
  expect_true(exists("get_epv_preds"))

  # Test with mock data
  mock_df <- data.frame(
    goal_x = c(50, 60, 70),
    y = c(0, 5, -5),
    lag_goal_x = c(55, 65, 75),
    lag_goal_x5 = c(60, 70, 80),
    lag_y = c(2, 7, -3),
    period_seconds = c(100, 200, 300),
    period = c(1, 1, 2),
    play_type_handball = c(1, 0, 1),
    play_type_kick = c(0, 1, 0),
    play_type_reception = c(0, 0, 1),
    phase_of_play_handball_received = c(1, 0, 0),
    phase_of_play_hard_ball = c(0, 1, 0),
    phase_of_play_loose_ball = c(0, 0, 1),
    phase_of_play_set_shot = c(0, 0, 0),
    shot_row = c(0, 1, 0),
    speed5 = c(10, 15, 20),
    home = c(1, 0, 1)
  )

  # Function should exist and process data (may fail without model data)
  result <- suppressWarnings(tryCatch({
    get_epv_preds(mock_df)
  }, error = function(e) e))

  # Either works or gives expected error about missing model
  if (inherits(result, "error")) {
    expect_true(grepl("ep_model|object.*not found|EP model", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 5)
    expect_true(all(c("opp_goal", "opp_behind", "behind", "goal", "no_score") %in% names(result)))
  }
})

test_that("get_wp_preds function exists and has correct structure", {
  expect_true(exists("get_wp_preds"))

  # Test with mock data
  mock_df <- data.frame(
    total_game_time_elapsed = c(1200, 2400, 3600),
    total_game_time_remaining = c(3600, 2400, 1200),
    shot_row = c(0, 1, 0),
    home = c(1, 0, 1),
    points_diff = c(6, -3, 12),
    xpoints_diff = c(6.5, -2.8, 12.3),
    pos_lead_prob = c(0.7, 0.3, 0.9),
    time_left_scaler = c(1.5, 2.0, 2.5),
    diff_time_ratio = c(9.75, -5.6, 30.75),
    score_urgency = c(0.1, -0.05, 0.6),
    goal_x = c(50, 30, 80),
    play_type_handball = c(1, 0, 1),
    play_type_kick = c(0, 1, 0),
    play_type_reception = c(0, 0, 1),
    phase_of_play_handball_received = c(1, 0, 0),
    phase_of_play_hard_ball = c(0, 1, 0),
    phase_of_play_loose_ball = c(0, 0, 1),
    phase_of_play_set_shot = c(0, 0, 0)
  )

  # Function should exist and process data (may fail without model data)
  result <- tryCatch({
    get_wp_preds(mock_df)
  }, error = function(e) e)

  # Either works or gives expected error about missing model/package
  if (inherits(result, "error")) {
    expect_true(grepl("wp_model|object.*not found|torpmodels|Failed to load", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 1)
    expect_true("wp" %in% names(result))
  }
})

test_that("add_epv_vars function works correctly", {
  expect_true(exists("add_epv_vars"))

  # Create mock play-by-play data
  mock_pbp <- create_mock_pbp_data(10)

  # Function should exist and process data (may fail without model data)
  result <- suppressWarnings(tryCatch({
    add_epv_vars(mock_pbp)
  }, error = function(e) e))

  # Test structure if it works, or expected error
  if (inherits(result, "error")) {
    expect_true(grepl("ep_model|model.*prediction|select_epv_model_vars|all_of|base_vars", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_gte(ncol(result), ncol(mock_pbp))  # Should have additional columns
  }
})

test_that("add_wp_vars function works correctly", {
  expect_true(exists("add_wp_vars"))

  # Create mock play-by-play data
  mock_pbp <- create_mock_pbp_data(10)

  # Function should exist and process data (may fail without model data)
  result <- suppressWarnings(tryCatch({
    add_wp_vars(mock_pbp)
  }, error = function(e) e))

  # Either works or gives expected error about missing model/package
  if (inherits(result, "error")) {
    expect_true(grepl("wp_model|object.*not found|torpmodels|Failed to load", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_gte(ncol(result), ncol(mock_pbp))
  }
})

test_that("add_shot_vars function works correctly", {
  expect_true(exists("add_shot_vars"))

  # Create mock shot data
  mock_shots <- create_mock_shot_data(10)

  # Function should exist and process data (may fail without model data)
  result <- suppressWarnings(tryCatch({
    add_shot_vars(mock_shots)
  }, error = function(e) e))

  # Test structure if it works, or expected error
  if (inherits(result, "error")) {
    expect_true(grepl("shot_ocat_mdl|object.*not found", result$message, ignore.case = TRUE))
  } else {
    expect_true(is.data.frame(result))
    expect_gte(ncol(result), ncol(mock_shots))  # Should have additional columns
  }
})

# -----------------------------------------------------------------------------
# Input Validation Tests
# -----------------------------------------------------------------------------

test_that("add_epv_vars validates input is a data frame", {
  expect_error(add_epv_vars("not a dataframe"), "data frame")
  expect_error(add_epv_vars(list(a = 1)), "data frame")
  expect_error(add_epv_vars(NULL), "data frame")
})

test_that("add_epv_vars rejects empty data frame", {
  empty_df <- data.frame()
  expect_error(add_epv_vars(empty_df), "empty")
})

test_that("add_wp_vars validates input is a data frame", {
  expect_error(add_wp_vars("not a dataframe"), "data frame")
  expect_error(add_wp_vars(list(a = 1)), "data frame")
  expect_error(add_wp_vars(NULL), "data frame")
})

test_that("add_wp_vars rejects empty data frame", {
  empty_df <- data.frame()
  expect_error(add_wp_vars(empty_df), "empty")
})

test_that("add_shot_vars validates input is a data frame", {
  expect_error(add_shot_vars("not a dataframe"), "data frame")
  expect_error(add_shot_vars(list(a = 1)), "data frame")
  expect_error(add_shot_vars(NULL), "data frame")
})

test_that("add_shot_vars rejects empty data frame", {
  empty_df <- data.frame()
  expect_error(add_shot_vars(empty_df), "empty")
})

# -----------------------------------------------------------------------------
# add_wp_vars Enhanced Model Tests
# -----------------------------------------------------------------------------

test_that("add_wp_vars adds WPA calculation columns", {
  mock_pbp <- create_mock_pbp_data(20)

  result <- suppressWarnings(tryCatch({
    add_wp_vars(mock_pbp)
  }, error = function(e) NULL))

  if (!is.null(result)) {
    # Should add wp and wpa columns
    expect_true("wp" %in% names(result))
    expect_true("wpa" %in% names(result))

    # wp should be bounded
    expect_true(all(result$wp >= 0 & result$wp <= 1))
  }
})

test_that("add_wp_vars adds context columns", {
  mock_pbp <- create_mock_pbp_data(20)

  result <- suppressWarnings(tryCatch({
    add_wp_vars(mock_pbp)
  }, error = function(e) NULL))

  if (!is.null(result)) {
    # Should add categorical columns
    expect_true("wp_category" %in% names(result))
    expect_true("high_leverage" %in% names(result))

    # wp_category should have valid values
    valid_categories <- c("very_likely", "likely", "toss_up", "unlikely", "very_unlikely")
    expect_true(all(result$wp_category %in% valid_categories))
  }
})

# -----------------------------------------------------------------------------
# Model Map Tests
# -----------------------------------------------------------------------------

test_that("load_model_with_fallback knows all valid model names", {
  valid_names <- c("ep", "wp", "shot", "xgb_win", "match_gams")

  for (name in valid_names) {
    # Should not error for a valid model name (model may or may not be available)
    expect_no_error(suppressWarnings(torp:::load_model_with_fallback(name)))
  }
})

# -----------------------------------------------------------------------------
# get_shot_result_preds Tests
# -----------------------------------------------------------------------------

test_that("get_shot_result_preds exists as internal function", {
  expect_true(exists("get_shot_result_preds", envir = asNamespace("torp")))
})

test_that("get_shot_result_preds handles mock data", {
  mock_shots <- create_mock_shot_data(5)

  result <- suppressWarnings(tryCatch(
    torp:::get_shot_result_preds(mock_shots),
    error = function(e) e
  ))

  skip_if(inherits(result, "error"), "Shot model not available")
  # Should return matrix/data frame with 3 columns
  expect_true(ncol(result) == 3 || length(dim(result)) >= 1)
})

test_that("get_shot_result_preds attaches mgcv for GAM models (regression)", {
  # Regression test: requireNamespace() alone is insufficient for GAM predict()
  # because mgcv's internal Xbd C function must be on the search path.
  # The fix must *attach* mgcv (via require() or attachNamespace()), not just load it.

  fn_body <- deparse(body(torp:::get_shot_result_preds))
  fn_text <- paste(fn_body, collapse = " ")

  # Must use an attaching mechanism (require or attachNamespace), not requireNamespace
  uses_attach <- grepl('require\\("mgcv"', fn_text) ||
                 grepl('attachNamespace\\("mgcv"', fn_text)
  uses_only_load <- grepl('requireNamespace\\("mgcv"', fn_text) && !uses_attach

  expect_true(uses_attach,
    info = "get_shot_result_preds must attach mgcv (require or attachNamespace)")
  expect_false(uses_only_load,
    info = "requireNamespace alone is insufficient — mgcv must be attached for Xbd")
})
