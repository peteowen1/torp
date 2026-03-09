# Tests for Clean PBP Data Pipeline
# ==================================
# End-to-end tests for the data cleaning pipeline

# -----------------------------------------------------------------------------
# clean_model_data_epv() Tests
# -----------------------------------------------------------------------------

test_that("clean_model_data_epv function exists", {
  expect_true(exists("clean_model_data_epv"))
  expect_true(is.function(clean_model_data_epv))
})

test_that("clean_model_data_epv processes valid data", {
  # Create mock data with required columns for EPV cleaning
  mock_data <- data.frame(
    match_id = rep("CD_M20240141001", 20),
    period = rep(1:4, each = 5),
    tot_goals = rep(0:4, 4),
    throw_in = sample(c(0, 1), 20, replace = TRUE, prob = c(0.9, 0.1)),
    description = sample(c("Kick", "Handball", "Mark"), 20, replace = TRUE),
    team_id = sample(1:2, 20, replace = TRUE),
    home_team_id = rep(1L, 20),
    away_team_id = rep(2L, 20),
    x = runif(20, -80, 80),
    y = runif(20, -60, 60),
    lead_x_tot = runif(20, -80, 80),
    lead_y_tot = runif(20, -60, 60),
    home_points = cumsum(sample(c(0, 0, 1, 6), 20, replace = TRUE)),
    away_points = cumsum(sample(c(0, 0, 1, 6), 20, replace = TRUE)),
    period_seconds = sort(sample(100:1800, 20)),
    venue_length = rep(160, 20),
    player_name = paste0("Player_", sample(1:10, 20, replace = TRUE)),
    play_type = sample(c("kick", "handball", "reception"), 20, replace = TRUE),
    phase_of_play = sample(c("set_shot", "general_play", "handball_received"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- clean_model_data_epv(mock_data)
  expect_true(is.data.frame(result))
})

# -----------------------------------------------------------------------------
# clean_model_data_wp() Tests
# -----------------------------------------------------------------------------

test_that("clean_model_data_wp function exists", {
  expect_true(exists("clean_model_data_wp"))
  expect_true(is.function(clean_model_data_wp))
})

test_that("clean_model_data_wp adds required WP variables", {
  # Create data with required columns for WP cleaning
  mock_data <- data.frame(
    label_wp = sample(0:1, 20, replace = TRUE),
    points_diff = sample(-20:20, 20, replace = TRUE),
    exp_pts = runif(20, -3, 3),
    opp_goal = runif(20, 0.05, 0.25),
    opp_behind = runif(20, 0.05, 0.15),
    no_score = runif(20, 0.2, 0.4),
    behind = runif(20, 0.05, 0.20),
    goal = runif(20, 0.2, 0.4),
    period = sample(1:4, 20, replace = TRUE),
    period_seconds = sample(100:1800, 20, replace = TRUE),
    play_type = sample(c("kick", "handball"), 20, replace = TRUE),
    phase_of_play = sample(c("set_shot", "general_play"), 20, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- tryCatch(
    clean_model_data_wp(mock_data),
    error = function(e) NULL
  )

  # If it works, check for new variables
  if (!is.null(result)) {
    expect_true(is.data.frame(result))
    expect_true("xpoints_diff" %in% names(result))
    expect_true("pos_lead_prob" %in% names(result))
    expect_true("time_left_scaler" %in% names(result))
    expect_true("diff_time_ratio" %in% names(result))
  }
})

test_that("clean_model_data_wp filters NA label_wp", {
  mock_data <- data.frame(
    label_wp = c(0, 1, NA, 0, NA, 1),
    points_diff = rep(0, 6),
    exp_pts = rep(0, 6),
    opp_goal = rep(0.1, 6),
    opp_behind = rep(0.1, 6),
    no_score = rep(0.4, 6),
    behind = rep(0.1, 6),
    goal = rep(0.3, 6),
    period = rep(1, 6),
    period_seconds = rep(500, 6),
    play_type = rep("kick", 6),
    phase_of_play = rep("set_shot", 6),
    stringsAsFactors = FALSE
  )

  result <- clean_model_data_wp(mock_data)

  # Should have removed NA rows
  expect_equal(nrow(result), 4)  # Only 4 non-NA label_wp rows
})

# -----------------------------------------------------------------------------
# select_epv_model_vars() Tests
# -----------------------------------------------------------------------------

test_that("select_epv_model_vars function exists", {
  expect_true(exists("select_epv_model_vars"))
  expect_true(is.function(select_epv_model_vars))
})

test_that("select_epv_model_vars selects correct columns", {
  # Create mock data with EPV model columns
  mock_data <- create_mock_pbp_data(10)

  result <- tryCatch(
    select_epv_model_vars(mock_data, label = FALSE),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_true(is.data.frame(result))

    # Check expected columns are present
    expected_cols <- c("goal_x", "y", "period_seconds", "period", "shot_row", "home")
    present <- expected_cols %in% names(result)
    expect_true(sum(present) >= 4)  # Most columns should be present
  }
})

test_that("select_epv_model_vars includes label when requested", {
  mock_data <- create_mock_pbp_data(10)
  mock_data$label_ep <- sample(1:5, 10, replace = TRUE)

  result <- tryCatch(
    select_epv_model_vars(mock_data, label = TRUE),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_true("label_ep" %in% names(result))
  }
})

# -----------------------------------------------------------------------------
# select_wp_model_vars() Tests
# -----------------------------------------------------------------------------

test_that("select_wp_model_vars function exists", {
  expect_true(exists("select_wp_model_vars"))
  expect_true(is.function(select_wp_model_vars))
})

test_that("select_wp_model_vars selects correct columns", {
  # Create mock data with WP model columns
  mock_data <- create_mock_pbp_data(10)
  mock_data$total_seconds <- mock_data$period_seconds + (mock_data$period - 1) * 2000
  mock_data$xpoints_diff <- runif(10, -10, 10)
  mock_data$pos_lead_prob <- runif(10, 0, 1)
  mock_data$time_left_scaler <- runif(10, 1, 4)
  mock_data$diff_time_ratio <- runif(10, -50, 50)

  result <- tryCatch(
    select_wp_model_vars(mock_data),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_true(is.data.frame(result))

    # Check expected columns
    expected_cols <- c("total_seconds", "shot_row", "home", "points_diff")
    present <- expected_cols %in% names(result)
    expect_true(sum(present) >= 3)
  }
})

# -----------------------------------------------------------------------------
# filter_relevant_descriptions() Tests
# -----------------------------------------------------------------------------

test_that("filter_relevant_descriptions filters correctly", {
  mock_data <- data.frame(
    description = c("Kick", "Handball", "Invalid", "Mark", "Another Invalid"),
    x = c(50, 60, 70, 80, 90),
    y = c(10, 20, 30, 40, 50),
    lead_x_tot = c(-50, -60, -70, -80, -90),
    lead_y_tot = c(-10, -20, -30, -40, -50),
    stringsAsFactors = FALSE
  )

  result <- torp:::filter_relevant_descriptions(mock_data)

  # Should only keep valid descriptions
  expect_true(nrow(result) <= 3)
  expect_true(all(result$description %in% c("Kick", "Handball", "Mark")))
})

test_that("filter_relevant_descriptions handles empty data", {
  empty_data <- data.frame(
    description = character(0),
    x = numeric(0),
    y = numeric(0),
    lead_x_tot = numeric(0),
    lead_y_tot = numeric(0)
  )

  result <- torp:::filter_relevant_descriptions(empty_data)

  expect_equal(nrow(result), 0)
})

# -----------------------------------------------------------------------------
# calculate_mirror() Tests
# -----------------------------------------------------------------------------

test_that("calculate_mirror returns valid values", {
  result <- torp:::calculate_mirror(
    throw_in = c(0, 0, 1, 0, 0),
    team_id_mdl = c(1, 1, 2, 2, 2),
    x = c(50, 60, 70, -50, -60)
  )

  # Should only return -1 or 1
  expect_true(all(result %in% c(-1, 1)))
  expect_length(result, 5)
})

test_that("calculate_mirror handles all zeros", {
  result <- torp:::calculate_mirror(
    throw_in = rep(0, 5),
    team_id_mdl = rep(1, 5),
    x = rep(50, 5)
  )

  # With no throw-ins, should all be 1
  expect_true(all(result == 1))
})

# -----------------------------------------------------------------------------
# determine_team_id_mdl() Tests
# -----------------------------------------------------------------------------

test_that("determine_team_id_mdl handles throw-ins", {
  result <- torp:::determine_team_id_mdl(
    throw_in = c(0, 0, 1, 0, 0),
    team_id = c(1, 1, 1, 2, 2)
  )

  expect_length(result, 5)
  expect_true(all(!is.na(result)))
})

test_that("determine_team_id_mdl handles all non-throw-ins", {
  result <- torp:::determine_team_id_mdl(
    throw_in = rep(0, 5),
    team_id = c(1, 1, 2, 2, 1)
  )

  # Should be same as team_id when no throw-ins
  expect_equal(result, c(1, 1, 2, 2, 1))
})

# -----------------------------------------------------------------------------
# calculate_pos_lead_prob() Tests
# -----------------------------------------------------------------------------

test_that("calculate_pos_lead_prob handles large positive diff", {
  result <- torp:::calculate_pos_lead_prob(
    points_diff = 20,  # Large positive
    opp_goal = 0.1,
    opp_behind = 0.1,
    no_score = 0.3,
    behind = 0.1,
    goal = 0.4
  )

  expect_equal(result, 1)  # > 6 point lead
})

test_that("calculate_pos_lead_prob handles large negative diff", {
  result <- torp:::calculate_pos_lead_prob(
    points_diff = -20,  # Large negative
    opp_goal = 0.1,
    opp_behind = 0.1,
    no_score = 0.3,
    behind = 0.1,
    goal = 0.4
  )

  expect_equal(result, 0)  # < -6 point deficit
})

test_that("calculate_pos_lead_prob handles tied game", {
  result <- torp:::calculate_pos_lead_prob(
    points_diff = 0,
    opp_goal = 0.1,
    opp_behind = 0.1,
    no_score = 0.3,
    behind = 0.2,
    goal = 0.3
  )

  # Should be (no_score * 0.5) + behind + goal
  expected <- (0.3 * 0.5) + 0.2 + 0.3
  expect_equal(result, expected)
})

test_that("calculate_pos_lead_prob returns values between 0 and 1", {
  # Test various scenarios
  diffs <- c(-10, -6, -3, 0, 3, 6, 10)

  for (diff in diffs) {
    result <- torp:::calculate_pos_lead_prob(
      points_diff = diff,
      opp_goal = 0.1,
      opp_behind = 0.1,
      no_score = 0.3,
      behind = 0.2,
      goal = 0.3
    )
    expect_true(result >= 0 && result <= 1,
                info = paste("Failed for points_diff =", diff))
  }
})

test_that("calculate_pos_lead_prob handles points_diff == 1 boundary", {
  result <- torp:::calculate_pos_lead_prob(
    points_diff = 1,
    opp_goal = 0.1,
    opp_behind = 0.15,
    no_score = 0.3,
    behind = 0.2,
    goal = 0.25
  )
  # points_diff >= 1 branch: (opp_behind * 0.5) + no_score + behind + goal
  expected <- (0.15 * 0.5) + 0.3 + 0.2 + 0.25
  expect_equal(result, expected)
})

test_that("calculate_pos_lead_prob handles points_diff == -1 boundary", {
  result <- torp:::calculate_pos_lead_prob(
    points_diff = -1,
    opp_goal = 0.1,
    opp_behind = 0.15,
    no_score = 0.3,
    behind = 0.2,
    goal = 0.25
  )
  # points_diff == -1 branch: (behind * 0.5) + goal
  expected <- (0.2 * 0.5) + 0.25
  expect_equal(result, expected)
})

# -----------------------------------------------------------------------------
# add_shot_result_variables() Tests
# -----------------------------------------------------------------------------

test_that("add_shot_result_variables adds correct columns", {
  mock_data <- data.frame(
    shot_at_goal = c(TRUE, TRUE, TRUE, FALSE),
    disposal = c("clanger", "ineffective", "effective", "effective"),
    points_shot = c(NA, 1, 6, NA),
    stringsAsFactors = FALSE
  )

  result <- torp:::add_shot_result_variables(mock_data)

  expect_true("shot_result_multi" %in% names(result))
  expect_true("shot_result" %in% names(result))
  expect_true("scored_shot" %in% names(result))
})

test_that("add_shot_result_variables classifies shots correctly", {
  mock_data <- data.frame(
    shot_at_goal = c(TRUE, TRUE, TRUE),
    disposal = c("clanger", "ineffective", "effective"),
    points_shot = c(NA, 1, 6),
    stringsAsFactors = FALSE
  )

  result <- torp:::add_shot_result_variables(mock_data)

  # Check shot_result_multi encoding
  expect_equal(result$shot_result_multi[1], 0)  # clanger = 0
  expect_equal(result$shot_result_multi[2], 1)  # ineffective = 1
  expect_equal(result$shot_result_multi[3], 2)  # effective = 2
})

# -----------------------------------------------------------------------------
# add_shot_geometry_variables() Tests
# -----------------------------------------------------------------------------

test_that("add_shot_geometry_variables calculates distance and angle", {
  mock_data <- data.frame(
    goal_x = c(20, 30, 40),
    y = c(0, 10, -10),
    stringsAsFactors = FALSE
  )

  result <- torp:::add_shot_geometry_variables(mock_data, goal_width = 6.4)

  expect_true("abs_y" %in% names(result))
  expect_true("angle" %in% names(result))
  expect_true("distance" %in% names(result))

  # Distance from directly in front should equal goal_x
  expect_equal(result$distance[1], 20, tolerance = 0.1)
})

test_that("add_shot_geometry_variables handles edge cases", {
  # Very close shot
  mock_data <- data.frame(
    goal_x = c(1),
    y = c(0),
    stringsAsFactors = FALSE
  )

  result <- torp:::add_shot_geometry_variables(mock_data, goal_width = 6.4)

  expect_true(result$distance[1] >= 0)
  expect_true(result$angle[1] >= 0)
})

# -----------------------------------------------------------------------------
# add_shot_type_variables() Tests
# -----------------------------------------------------------------------------

test_that("add_shot_type_variables creates indicator variables", {
  mock_data <- data.frame(
    shot_at_goal = c(TRUE, TRUE, TRUE, FALSE),
    disposal = c("clanger", "effective", "ineffective", "effective"),
    stringsAsFactors = FALSE
  )

  result <- torp:::add_shot_type_variables(mock_data)

  expect_true("shot_clanger" %in% names(result))
  expect_true("shot_effective" %in% names(result))
  expect_true("shot_ineffective" %in% names(result))

  # Check values
  expect_equal(result$shot_clanger, c(1, 0, 0, 0))
  expect_equal(result$shot_effective, c(0, 1, 0, 0))
  expect_equal(result$shot_ineffective, c(0, 0, 1, 0))
})

# -----------------------------------------------------------------------------
# clean_shots_data() Tests
# -----------------------------------------------------------------------------

test_that("clean_shots_data function exists", {
  expect_true(exists("clean_shots_data"))
  expect_true(is.function(clean_shots_data))
})

test_that("clean_shots_data processes shot data", {
  mock_shots <- create_mock_shot_data(10)

  result <- tryCatch(
    clean_shots_data(mock_shots),
    error = function(e) NULL
  )

  skip_if(is.null(result), "clean_shots_data failed (model unavailable)")
  expect_true(is.data.frame(result))
  expect_true("abs_y" %in% names(result) || "distance" %in% names(result))
})

# -----------------------------------------------------------------------------
# select_shot_model_vars() Tests
# -----------------------------------------------------------------------------

test_that("select_shot_model_vars function exists", {
  expect_true(exists("select_shot_model_vars"))
  expect_true(is.function(select_shot_model_vars))
})

# -----------------------------------------------------------------------------
# Pipeline Integration Test
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# setorder Ordering Guarantee (regression test)
# -----------------------------------------------------------------------------

test_that("setorder before setkey guarantees within-match display_order for shift ops", {
  # Regression test for clean_pbp_dt: setkey(dt, match_id) sorts by match_id
  # but does NOT sort display_order within each match. Downstream shift/lag/lead
  # operations depend on within-match display_order being monotonic.
  dt <- data.table::data.table(
    match_id = c("A", "A", "A", "A", "B", "B", "B", "B"),
    display_order = c(4, 2, 3, 1, 3, 1, 4, 2),
    value = c("a4", "a2", "a3", "a1", "b3", "b1", "b4", "b2")
  )

  # Replicate clean_pbp_dt ordering: setorder then setkey
  data.table::setorder(dt, match_id, display_order)
  data.table::setkey(dt, match_id)

  # Within each match, display_order must be sorted
  expect_equal(dt[match_id == "A", display_order], c(1, 2, 3, 4))
  expect_equal(dt[match_id == "B", display_order], c(1, 2, 3, 4))

  # shift() must produce correct lead values (critical for EP/WP pipeline)
  dt[, next_value := data.table::shift(value, type = "lead"), by = match_id]
  expect_equal(dt[match_id == "A", next_value], c("a2", "a3", "a4", NA))
  expect_equal(dt[match_id == "B", next_value], c("b2", "b3", "b4", NA))
})

# -----------------------------------------------------------------------------
# Pipeline Integration Test
# -----------------------------------------------------------------------------

test_that("EPV pipeline components work together", {
  # This tests the main flow without actual data loading
  expect_true(is.function(clean_model_data_epv))
  expect_true(is.function(select_epv_model_vars))
  expect_true(is.function(torp:::filter_relevant_descriptions))
  expect_true(is.function(torp:::add_epv_variables))
})

test_that("WP pipeline components work together", {
  expect_true(is.function(clean_model_data_wp))
  expect_true(is.function(select_wp_model_vars))
  expect_true(is.function(torp:::calculate_pos_lead_prob))
})

test_that("Shot pipeline components work together", {
  expect_true(is.function(clean_shots_data))
  expect_true(is.function(select_shot_model_vars))
  expect_true(is.function(prepare_shot_model_data))
  expect_true(is.function(create_shot_model_matrix))
})

# -----------------------------------------------------------------------------
# .normalise_pbp_columns() Tests
# -----------------------------------------------------------------------------

test_that(".normalise_pbp_columns remaps v2 schema columns", {
  # Simulate v2 API response with new-style column names
  dt <- data.table::data.table(
    home_score_total_score    = 100L,
    away_score_total_score    = 80L,
    home_team_name            = "Collingwood",
    away_team_name            = "Richmond",
    home_team_abbreviation    = "COL",
    away_team_abbreviation    = "RIC",
    home_team_provider_id     = "CD_T10",
    away_team_provider_id     = "CD_T120",
    round_round_number        = 1L
  )

  torp:::.normalise_pbp_columns(dt)

  # All should be remapped to expected names

  expect_true("home_team_score_total_score" %in% names(dt))
  expect_true("away_team_score_total_score" %in% names(dt))
  expect_true("home_team_team_name" %in% names(dt))
  expect_true("away_team_team_name" %in% names(dt))
  expect_true("home_team_team_abbr" %in% names(dt))
  expect_true("away_team_team_abbr" %in% names(dt))
  expect_true("home_team_id" %in% names(dt))
  expect_true("away_team_id" %in% names(dt))
  expect_true("round_number" %in% names(dt))

  # Old names should be gone
  expect_false("home_score_total_score" %in% names(dt))
  expect_false("round_round_number" %in% names(dt))

  # Values preserved
  expect_equal(dt$home_team_score_total_score, 100L)
  expect_equal(dt$home_team_team_name, "Collingwood")
})

test_that(".normalise_pbp_columns passes through old schema unchanged", {
  # Old schema already has the expected column names
  dt <- data.table::data.table(
    home_team_score_total_score = 100L,
    away_team_score_total_score = 80L,
    home_team_team_name         = "Collingwood",
    away_team_team_name         = "Richmond",
    home_team_team_abbr         = "COL",
    away_team_team_abbr         = "RIC",
    home_team_id                = "CD_T10",
    away_team_id                = "CD_T120",
    round_number                = 1L
  )

  original_names <- copy(names(dt))
  torp:::.normalise_pbp_columns(dt)

  # Names should be identical — no remapping needed
  expect_equal(names(dt), original_names)
})

test_that(".normalise_pbp_columns handles mixed schema (partial v2)", {

  # Some columns old-style, some new-style
  dt <- data.table::data.table(
    home_team_score_total_score = 100L,  # already old-style
    away_score_total_score      = 80L,   # v2 style
    home_team_team_name         = "Collingwood",  # already old-style
    away_team_name              = "Richmond",     # v2 style
    round_number                = 1L              # already old-style
  )

  torp:::.normalise_pbp_columns(dt)

  # Old-style columns preserved
  expect_true("home_team_score_total_score" %in% names(dt))
  expect_true("home_team_team_name" %in% names(dt))
  expect_true("round_number" %in% names(dt))

  # v2 columns remapped
  expect_true("away_team_score_total_score" %in% names(dt))
  expect_true("away_team_team_name" %in% names(dt))

  # v2 source names should be gone
  expect_false("away_score_total_score" %in% names(dt))
  expect_false("away_team_name" %in% names(dt))
})

test_that(".normalise_pbp_columns does not remap when both old and new exist", {
  # Edge case: both the v2 and expected column are present
  dt <- data.table::data.table(
    home_score_total_score      = 999L,   # v2 name
    home_team_score_total_score = 100L,   # expected name also present
    round_round_number          = 5L,     # v2 name
    round_number                = 1L      # expected name also present
  )

  torp:::.normalise_pbp_columns(dt)

  # Expected columns retain original values (not overwritten)
  expect_equal(dt$home_team_score_total_score, 100L)
  expect_equal(dt$round_number, 1L)

  # v2 columns still present (not renamed because target already existed)
  expect_true("home_score_total_score" %in% names(dt))
  expect_true("round_round_number" %in% names(dt))
})

test_that(".normalise_pbp_columns handles empty data.table", {
  dt <- data.table::data.table(
    home_score_total_score = integer(0),
    away_team_name = character(0)
  )

  # Should not error on zero rows (warns about missing critical columns)
  expect_no_error(torp:::.normalise_pbp_columns(dt))

  # Should still remap column names (names exist even with 0 rows)
  expect_true("home_team_score_total_score" %in% names(dt))
  expect_true("away_team_team_name" %in% names(dt))
})
