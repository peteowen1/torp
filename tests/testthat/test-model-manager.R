# Tests for Model Manager and Registry
# ====================================

test_that("ModelRegistry can be instantiated", {
  registry <- ModelRegistry$new()
  
  expect_true(R6::is.R6(registry))
  expect_true(inherits(registry, "ModelRegistry"))
  
  # Should start empty
  expect_equal(length(registry$list_models()), 0)
})

test_that("ModelRegistry can register and retrieve models", {
  registry <- ModelRegistry$new()
  
  # Create a mock model
  mock_model <- list(coefficients = c(1, 2, 3))
  class(mock_model) <- "glm"
  
  # Register model
  success <- registry$register_model("test_model", mock_model, "glm", "1.0")
  expect_true(success)
  
  # Check it's listed
  models <- registry$list_models()
  expect_true("test_model" %in% models)
  
  # Retrieve model
  retrieved <- registry$get_model("test_model")
  expect_equal(retrieved$coefficients, c(1, 2, 3))
  expect_true(inherits(retrieved, "glm"))
})

test_that("ModelRegistry validates model types correctly", {
  registry <- ModelRegistry$new()
  
  # Valid GLM model
  valid_glm <- list(coefficients = c(1, 2))
  class(valid_glm) <- "glm"
  
  expect_true(registry$register_model("valid_glm", valid_glm, "glm"))
  
  # Invalid model for type
  invalid_model <- "not_a_model"
  
  expect_error(registry$register_model("invalid", invalid_model, "glm"))
})

test_that("ModelRegistry handles model metadata", {
  registry <- ModelRegistry$new()
  
  mock_model <- list(data = "test")
  metadata <- list(
    created_by = "test_user",
    accuracy = 0.85,
    notes = "Test model"
  )
  
  registry$register_model("test_model", mock_model, "unknown", "2.0", metadata)
  
  info <- registry$get_model_info("test_model")
  
  expect_equal(info$name, "test_model")
  expect_equal(info$version, "2.0")
  expect_equal(info$metadata$created_by, "test_user")
  expect_equal(info$metadata$accuracy, 0.85)
})

test_that("ModelRegistry can save and load registry", {
  registry <- ModelRegistry$new()
  
  # Register a test model
  mock_model <- list(weights = c(0.1, 0.2, 0.3))
  registry$register_model("persistent_model", mock_model, "unknown")
  
  # Save registry
  temp_file <- tempfile(fileext = ".rds")
  success <- registry$save_registry(temp_file)
  expect_true(success)
  expect_true(file.exists(temp_file))
  
  # Create new registry and load
  new_registry <- ModelRegistry$new()
  expect_equal(length(new_registry$list_models()), 0)
  
  load_success <- new_registry$load_registry(temp_file)
  expect_true(load_success)
  
  # Check model was loaded
  models <- new_registry$list_models()
  expect_true("persistent_model" %in% models)
  
  retrieved <- new_registry$get_model("persistent_model")
  expect_equal(retrieved$weights, c(0.1, 0.2, 0.3))
  
  # Clean up
  file.remove(temp_file)
})

test_that("ModelRegistry handles hash validation", {
  registry <- ModelRegistry$new()
  
  mock_model <- list(data = "original")
  registry$register_model("hash_test", mock_model, "unknown")
  
  # Normal retrieval should work
  retrieved <- registry$get_model("hash_test", validate_hash = FALSE)
  expect_equal(retrieved$data, "original")
  
  # Hash validation should also work initially
  retrieved_validated <- registry$get_model("hash_test", validate_hash = TRUE)
  expect_equal(retrieved_validated$data, "original")
})

test_that("load_torp_models handles missing models gracefully", {
  # This should not crash even if models are missing
  result <- load_torp_models(c("nonexistent_model"), force_reload = FALSE)
  
  expect_true(is.list(result))
  expect_true("nonexistent_model" %in% names(result))
  expect_true(grepl("error|not_found", result$nonexistent_model))
})

test_that("get_model_safe provides fallback behavior", {
  # Test with non-existent model
  result <- get_model_safe("definitely_nonexistent_model", fallback_to_data = FALSE)
  expect_null(result)
  
  # With fallback (should still be null for fake model, but shouldn't error)
  result_fallback <- tryCatch({
    get_model_safe("definitely_nonexistent_model", fallback_to_data = TRUE)
  }, error = function(e) e)
  
  # Should either be null (model not found) or error message
  expect_true(is.null(result_fallback) || inherits(result_fallback, "error"))
})

test_that("determine_model_type identifies model types correctly", {
  # Test different model types
  
  # GLM model
  glm_model <- list(coefficients = c(1, 2))
  class(glm_model) <- "glm"
  expect_equal(determine_model_type(glm_model), "glm")
  
  # Ensemble model
  ensemble_model <- list(
    xgb_model = "fake_xgb",
    gam_model = "fake_gam",
    ensemble_weights = c(0.5, 0.5)
  )
  expect_equal(determine_model_type(ensemble_model), "ensemble")
  
  # Unknown model
  unknown_model <- list(data = "unknown")
  expect_equal(determine_model_type(unknown_model), "unknown")
})

test_that("check_model_health provides status information", {
  # Should work even with empty registry
  health <- check_model_health()
  
  expect_true(is.list(health))
  expect_true("status" %in% names(health))
  expect_true(health$status %in% c("no_models", "healthy", "degraded", "unhealthy"))
  
  # With models
  if (length(torp_model_registry$list_models()) > 0) {
    expect_true("models" %in% names(health))
    expect_true(is.list(health$models))
  }
})

test_that("ModelRegistry status provides useful information", {
  registry <- ModelRegistry$new()
  
  status <- registry$get_status()
  
  expect_true(is.list(status))
  expect_true("n_models" %in% names(status))
  expect_true("model_names" %in% names(status))
  expect_true("cache_dir" %in% names(status))
  expect_true("memory_usage" %in% names(status))
  
  expect_true(is.numeric(status$n_models))
  expect_true(is.character(status$model_names))
})