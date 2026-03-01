test_that("skill_stat_definitions returns correct structure", {
  defs <- skill_stat_definitions()

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
  defs <- skill_stat_definitions()

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

test_that("skill_position_map covers expected AFL positions", {
  pm <- skill_position_map()

  expect_type(pm, "list")
  expect_named(pm, c("DEF", "MID", "FWD", "RUCK"))

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

test_that("default_skill_params returns valid hyperparameters", {
  params <- default_skill_params()

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

test_that("skill stat categories are consistent", {
  defs <- skill_stat_definitions()

  # Categories should be non-empty strings
  expect_true(all(nchar(defs$category) > 0))

  # Known categories
  expected_cats <- c("scoring", "disposal", "possession", "contested",
                     "clearance", "territory", "defensive", "pressure",
                     "ruck", "discipline", "negative")
  expect_true(all(defs$category %in% expected_cats))
})
