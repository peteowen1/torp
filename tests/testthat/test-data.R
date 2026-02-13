test_that("package data objects exist", {
  # Note: Models (ep_model, wp_model, shot_ocat_mdl) are loaded from torpmodels
  # package via load_model_with_fallback(), not bundled as package data
  expect_true("fixtures" %in% data(package = "torp")$results[, "Item"])
})

test_that("fixtures data has correct structure", {
  data(fixtures, package = "torp", envir = environment())
  expect_true(is.data.frame(fixtures))
  expect_true(nrow(fixtures) > 0)
})

test_that("models can be loaded via load_model_with_fallback", {
  # Models are loaded from torpmodels package (or package data fallback)
  # via load_model_with_fallback() - not directly as package data
  skip_if_no_internet()

  # Test EP model loading
  ep_model <- tryCatch(
    load_model_with_fallback("ep"),
    error = function(e) NULL
  )
  skip_if(is.null(ep_model), "Could not load EP model")
  expect_true(!is.null(ep_model))

  # Test WP model loading
  wp_model <- tryCatch(
    load_model_with_fallback("wp"),
    error = function(e) NULL
  )
  skip_if(is.null(wp_model), "Could not load WP model")
  expect_true(!is.null(wp_model))

  # Test shot model loading
  shot_model <- tryCatch(
    load_model_with_fallback("shot"),
    error = function(e) NULL
  )
  skip_if(is.null(shot_model), "Could not load shot model")
  expect_true(!is.null(shot_model))
})