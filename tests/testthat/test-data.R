test_that("package data objects exist", {
  # Test that all documented data objects exist
  expect_true("ep_model" %in% data(package = "torp")$results[, "Item"])
  expect_true("player_shot_score" %in% data(package = "torp")$results[, "Item"])
  expect_true("plyr_gm_df" %in% data(package = "torp")$results[, "Item"])
  expect_true("shot_ocat_mdl" %in% data(package = "torp")$results[, "Item"])
  expect_true("shot_player_df" %in% data(package = "torp")$results[, "Item"])
  expect_true("torp_df_total" %in% data(package = "torp")$results[, "Item"])
  expect_true("wp_model" %in% data(package = "torp")$results[, "Item"])
})

test_that("data objects have correct structure", {
  # Load data and test basic structure
  data(plyr_gm_df, package = "torp", envir = environment())
  expect_true(is.data.frame(plyr_gm_df))
  expect_true(nrow(plyr_gm_df) > 0)
})

test_that("model objects exist", {
  # Test that model objects exist and are of appropriate class
  data(ep_model, package = "torp", envir = environment())
  expect_true(exists("ep_model"))
  
  data(wp_model, package = "torp", envir = environment())
  expect_true(exists("wp_model"))
  
  data(shot_ocat_mdl, package = "torp", envir = environment())
  expect_true(exists("shot_ocat_mdl"))
})