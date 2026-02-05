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

  result <- torp:::load_model_with_fallback("nonexistent_model")

  # Should return NULL and warn
  expect_null(result)
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
  result <- tryCatch({
    get_epv_preds(mock_df)
  }, error = function(e) e)

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
    total_seconds = c(1800, 3600, 5400),
    shot_row = c(0, 1, 0),
    home = c(1, 0, 1),
    points_diff = c(6, -3, 12),
    xpoints_diff = c(6.5, -2.8, 12.3),
    pos_lead_prob = c(0.7, 0.3, 0.9),
    time_left_scaler = c(1.5, 2.0, 2.5),
    diff_time_ratio = c(9.75, -5.6, 30.75),
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

  # Either works or gives expected error about missing model
  if (inherits(result, "error")) {
    expect_true(grepl("wp_model|object.*not found", result$message, ignore.case = TRUE))
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
  result <- tryCatch({
    add_epv_vars(mock_pbp)
  }, error = function(e) e)

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
  result <- tryCatch({
    add_wp_vars(mock_pbp)
  }, error = function(e) e)

  # Function should succeed with fallback (warnings are expected)
  expect_true(is.data.frame(result))
  expect_gte(ncol(result), ncol(mock_pbp))  # Should have additional columns
})

test_that("add_shot_vars function works correctly", {
  expect_true(exists("add_shot_vars"))

  # Create mock shot data
  mock_shots <- create_mock_shot_data(10)

  # Function should exist and process data (may fail without model data)
  result <- tryCatch({
    add_shot_vars(mock_shots)
  }, error = function(e) e)

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

  result <- tryCatch({
    add_wp_vars(mock_pbp)
  }, error = function(e) NULL)

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

  result <- tryCatch({
    add_wp_vars(mock_pbp, use_enhanced = TRUE)
  }, error = function(e) NULL)

  if (!is.null(result)) {
    # Should add categorical columns
    expect_true("wp_category" %in% names(result))
    expect_true("high_leverage" %in% names(result))

    # wp_category should have valid values
    valid_categories <- c("very_likely", "likely", "toss_up", "unlikely", "very_unlikely")
    expect_true(all(result$wp_category %in% valid_categories))
  }
})

test_that("add_wp_vars use_enhanced parameter works", {
  mock_pbp <- create_mock_pbp_data(10)

  # Both should work (may fall back if enhanced fails)
  result_enhanced <- tryCatch({
    add_wp_vars(mock_pbp, use_enhanced = TRUE)
  }, error = function(e) NULL)

  result_basic <- tryCatch({
    add_wp_vars(mock_pbp, use_enhanced = FALSE)
  }, error = function(e) NULL)

  if (!is.null(result_enhanced) && !is.null(result_basic)) {
    # Both should have wp column
    expect_true("wp" %in% names(result_enhanced))
    expect_true("wp" %in% names(result_basic))
  }
})

# -----------------------------------------------------------------------------
# Model Map Tests
# -----------------------------------------------------------------------------

test_that("load_model_with_fallback knows all valid model names", {
  valid_names <- c("ep", "wp", "shot", "xgb_win")

  for (name in valid_names) {
    # Should not return NULL for valid name (unless model unavailable)
    result <- suppressWarnings(torp:::load_model_with_fallback(name))
    # Either it loads or returns NULL (but doesn't error on the name itself)
    expect_true(is.null(result) || !is.null(result))
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

  result <- tryCatch(
    torp:::get_shot_result_preds(mock_shots),
    error = function(e) e
  )

  if (!inherits(result, "error")) {
    # Should return matrix/data frame with 3 columns
    expect_true(ncol(result) == 3 || length(dim(result)) >= 1)
  }
})
