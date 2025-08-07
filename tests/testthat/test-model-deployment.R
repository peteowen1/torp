# Tests for Model Deployment System
# ==================================

test_that("ModelDeployment can be instantiated", {
  deployment <- ModelDeployment$new()
  
  expect_true(R6::is.R6(deployment))
  expect_true(inherits(deployment, "ModelDeployment"))
})

test_that("ModelDeployment creates model versions", {
  deployment <- ModelDeployment$new()
  
  # Create mock model
  mock_model <- list(coefficients = c(1, 2, 3))
  class(mock_model) <- "glm"
  
  # Create version
  version_record <- deployment$create_version(
    model_object = mock_model,
    model_name = "test_model",
    version = "1.0.0",
    metadata = list(test = TRUE)
  )
  
  expect_true(is.list(version_record))
  expect_equal(version_record$model_name, "test_model")
  expect_equal(version_record$version, "1.0.0")
  expect_true("version_hash" %in% names(version_record))
  expect_true("created_at" %in% names(version_record))
})

test_that("ModelDeployment validates deployment configurations", {
  deployment <- ModelDeployment$new()
  
  # Create and register a test model version first
  mock_model <- list(data = "test")
  version_record <- deployment$create_version(mock_model, "test_model", "1.0")
  
  # Valid deployment should work
  expect_silent({
    deployment_record <- deployment$deploy_version(
      model_name = "test_model",
      version = "1.0",
      deployment_strategy = "canary",
      rollout_percentage = 10
    )
  })
  
  # Invalid strategy should error
  expect_error({
    deployment$deploy_version(
      model_name = "test_model", 
      version = "1.0",
      deployment_strategy = "invalid_strategy",
      rollout_percentage = 10
    )
  })
  
  # Invalid rollout percentage should error
  expect_error({
    deployment$deploy_version(
      model_name = "test_model",
      version = "1.0", 
      deployment_strategy = "canary",
      rollout_percentage = 150  # > 100
    )
  })
})

test_that("ModelDeployment handles different deployment strategies", {
  deployment <- ModelDeployment$new()
  
  # Create test model version
  mock_model <- list(weights = c(0.1, 0.2))
  deployment$create_version(mock_model, "strategy_test", "1.0")
  
  strategies <- c("blue_green", "canary", "rolling", "shadow")
  
  for (strategy in strategies) {
    expect_silent({
      deployment_record <- deployment$deploy_version(
        model_name = "strategy_test",
        version = "1.0",
        deployment_strategy = strategy,
        rollout_percentage = 25
      )
      
      expect_equal(deployment_record$deployment_strategy, strategy)
      expect_true(deployment_record$status %in% c("deployed", "deploying", "failed"))
    })
  }
})

test_that("ModelDeployment tracks deployment history", {
  deployment <- ModelDeployment$new()
  
  # Create test model
  mock_model <- list(data = "history_test")
  deployment$create_version(mock_model, "history_model", "1.0")
  
  # Deploy model
  deployment_record <- deployment$deploy_version(
    model_name = "history_model",
    version = "1.0",
    deployment_strategy = "canary"
  )
  
  # Check deployment history
  deployments <- deployment$list_deployments()
  expect_true(length(deployments) >= 1)
  
  # Check specific model deployments
  model_deployments <- deployment$list_deployments(model_name = "history_model")
  expect_true(length(model_deployments) >= 1)
  expect_equal(model_deployments[[1]]$model_name, "history_model")
})

test_that("ModelDeployment provides deployment status", {
  deployment <- ModelDeployment$new()
  
  # Create and deploy test model
  mock_model <- list(data = "status_test")
  deployment$create_version(mock_model, "status_model", "1.0")
  
  deployment_record <- deployment$deploy_version(
    model_name = "status_model",
    version = "1.0"
  )
  
  # Get deployment status
  status <- deployment$get_deployment_status(deployment_record$deployment_id)
  
  expect_true(is.list(status))
  expect_equal(status$deployment_id, deployment_record$deployment_id)
  expect_equal(status$model_name, "status_model")
  expect_equal(status$version, "1.0")
  expect_true("status" %in% names(status))
})

test_that("ModelDeployment handles rollbacks", {
  deployment <- ModelDeployment$new()
  
  # Create two versions
  mock_model_v1 <- list(data = "v1")
  mock_model_v2 <- list(data = "v2")
  
  deployment$create_version(mock_model_v1, "rollback_test", "1.0")
  deployment$create_version(mock_model_v2, "rollback_test", "2.0")
  
  # Deploy v1 first
  deploy_v1 <- deployment$deploy_version("rollback_test", "1.0", rollout_percentage = 100)
  
  # Deploy v2
  deploy_v2 <- deployment$deploy_version("rollback_test", "2.0", rollout_percentage = 100)
  
  # Test rollback
  if (deploy_v2$status == "deployed") {
    rollback_record <- deployment$rollback_deployment(deploy_v2$deployment_id, "Test rollback")
    
    expect_true(is.list(rollback_record))
    expect_equal(rollback_record$model_name, "rollback_test")
    expect_equal(rollback_record$rolled_back_from, "2.0")
    expect_equal(rollback_record$rolled_back_to, "1.0")
  }
})

test_that("deploy_model_safe function works end-to-end", {
  # Test the high-level deployment function
  mock_model <- list(params = c(1, 2, 3))
  class(mock_model) <- "glm"
  
  expect_silent({
    deployment_record <- deploy_model_safe(
      model_object = mock_model,
      model_name = "safe_deploy_test",
      version = "1.0.0",
      deployment_strategy = "canary",
      rollout_percentage = 5
    )
  })
  
  expect_true(is.list(deployment_record))
  expect_equal(deployment_record$model_name, "safe_deploy_test")
  expect_equal(deployment_record$version, "1.0.0")
  expect_equal(deployment_record$deployment_strategy, "canary")
})

test_that("ModelDeployment handles missing model versions gracefully", {
  deployment <- ModelDeployment$new()
  
  # Try to deploy non-existent model version
  expect_error({
    deployment$deploy_version(
      model_name = "nonexistent_model",
      version = "1.0",
      deployment_strategy = "canary"
    )
  }, "Model version not found")
})

test_that("ModelDeployment validates deployment health", {
  deployment <- ModelDeployment$new()
  
  # Create test model
  mock_model <- list(data = "health_test")
  deployment$create_version(mock_model, "health_model", "1.0")
  
  deployment_record <- list(
    model_name = "health_model",
    version = "1.0"
  )
  
  # Should not error with valid model
  expect_silent({
    health_results <- deployment$validate_deployment_health(deployment_record)
  })
  
  expect_true(is.list(health_results))
  expect_true("model_loadable" %in% names(health_results))
  expect_true("validated_at" %in% names(health_results))
})

test_that("ModelDeployment manages active deployments", {
  deployment <- ModelDeployment$new()
  
  # Create and deploy test model
  mock_model <- list(data = "active_test")
  deployment$create_version(mock_model, "active_model", "1.0")
  
  deployment_record <- deployment$deploy_version(
    model_name = "active_model",
    version = "1.0"
  )
  
  # Check active deployments
  active_deployments <- deployment$get_active_deployments()
  expect_true(is.list(active_deployments))
  
  if (deployment_record$status == "deployed") {
    expect_true("active_model" %in% names(active_deployments))
  }
})