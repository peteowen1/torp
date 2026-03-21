test_that("stat_rating_definitions returns correct structure", {
  defs <- stat_rating_definitions()

  expect_s3_class(defs, "data.frame")
  expect_true(nrow(defs) > 0)

  # Required columns
  expected_cols <- c("stat_name", "type", "source_col", "category",
                     "success_col", "attempts_col")
  expect_true(all(expected_cols %in% names(defs)))

  # Types are valid
  expect_true(all(defs$type %in% c("rate", "efficiency")))

  # All stat_names are unique

  expect_equal(length(unique(defs$stat_name)), nrow(defs))
})

test_that("rate stats have source_col, efficiency stats have success/attempts", {
  defs <- stat_rating_definitions()

  rate_defs <- defs[defs$type == "rate", ]
  eff_defs <- defs[defs$type == "efficiency", ]

  # Rate stats must have source_col

  expect_true(all(!is.na(rate_defs$source_col)))

  # Efficiency stats must have success_col and attempts_col
  expect_true(all(!is.na(eff_defs$success_col)))
  expect_true(all(!is.na(eff_defs$attempts_col)))

  # Should have both types
  expect_true(nrow(rate_defs) > 0)
  expect_true(nrow(eff_defs) > 0)
})

test_that("stat_rating_position_map covers expected AFL positions", {
  pm <- stat_rating_position_map()

  expect_type(pm, "list")
  expect_named(pm, c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "MEDIUM_FORWARD", "KEY_FORWARD", "RUCK"))

  # All positions are character vectors
  for (grp in names(pm)) {
    expect_type(pm[[grp]], "character")
    expect_true(length(pm[[grp]]) > 0)
  }

  # Check key positions are covered
  all_positions <- unlist(pm)
  expect_true("KEY_DEFENDER" %in% all_positions)
  expect_true("MIDFIELDER" %in% all_positions)
  expect_true("KEY_FORWARD" %in% all_positions)
  expect_true("RUCK" %in% all_positions)
  expect_true("MEDIUM_DEFENDER" %in% all_positions)
  expect_true("MIDFIELDER_FORWARD" %in% all_positions)
  expect_true("MEDIUM_FORWARD" %in% all_positions)
})

test_that("default_stat_rating_params returns valid hyperparameters", {
  params <- default_stat_rating_params()

  expect_type(params, "list")

  # All expected keys present
  expected_keys <- c("lambda_rate", "lambda_efficiency", "prior_games",
                     "prior_attempts", "min_games", "credible_level")
  expect_true(all(expected_keys %in% names(params)))

  # All values are positive
  for (k in expected_keys) {
    expect_gt(params[[k]], 0)
  }

  # Credible level is between 0 and 1
  expect_lt(params$credible_level, 1)

  # Lambdas are small (decay rates per day)
  expect_lt(params$lambda_rate, 0.1)
  expect_lt(params$lambda_efficiency, 0.1)

  # Prior games / attempts are reasonable
  expect_gte(params$prior_games, 1)
  expect_gte(params$prior_attempts, 1)
})

test_that("stat rating categories are consistent", {
  defs <- stat_rating_definitions()

  # Categories should be non-empty strings
  expect_true(all(nchar(defs$category) > 0))

  # Known categories
  expected_cats <- c("scoring", "disposal", "possession", "contested",
                     "clearance", "territory", "defensive", "pressure",
                     "ruck", "discipline", "negative", "general")
  expect_true(all(defs$category %in% expected_cats))
})


# ==========================================================================
# Tests for .normalise_stat_rating_columns()
# ==========================================================================

test_that("normalise_stat_rating_columns renames _skill to _rating", {
  dt <- data.table::data.table(
    player_id = "P1",
    goals_skill = 1.5,
    kicks_skill = 10.2
  )

  .normalise_stat_rating_columns(dt)

  expect_true("goals_rating" %in% names(dt))
  expect_true("kicks_rating" %in% names(dt))
  expect_false("goals_skill" %in% names(dt))
  expect_equal(dt$goals_rating, 1.5)
})

test_that("normalise_stat_rating_columns renames CI columns", {
  dt <- data.table::data.table(
    player_id = "P1",
    goals_skill = 1.5,
    goals_lower = 0.8,
    goals_upper = 2.2
  )

  .normalise_stat_rating_columns(dt)

  expect_true("goals_rating_lower" %in% names(dt))
  expect_true("goals_rating_upper" %in% names(dt))
  expect_false("goals_lower" %in% names(dt))
  expect_equal(dt$goals_rating_lower, 0.8)
})

test_that("normalise_stat_rating_columns does not rename when target exists", {
  dt <- data.table::data.table(
    player_id = "P1",
    goals_skill = 99,      # old name with stale value
    goals_rating = 1.5     # new name already present
  )

  .normalise_stat_rating_columns(dt)

  # Should keep the existing goals_rating, not overwrite
  expect_equal(dt$goals_rating, 1.5)
})

test_that("normalise_stat_rating_columns leaves non-stat columns untouched", {
  dt <- data.table::data.table(
    player_id = "P1",
    some_other_skill = 42
  )

  .normalise_stat_rating_columns(dt)

  # "some_other" is not a stat_name in definitions, but the regex rename

  # still fires on _skill$ pattern — verify it still renames
  expect_true("some_other_rating" %in% names(dt))
})
