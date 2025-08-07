# Tests for Model Validation Framework
# =====================================

test_that("create_grouped_cv_folds works correctly", {
  # Create test data
  test_data <- data.frame(
    match_id = rep(paste0("M", 1:5), each = 20),
    value = rnorm(100)
  )
  
  # Test basic functionality
  folds <- create_grouped_cv_folds(test_data, "match_id", k = 3)
  
  expect_equal(length(folds), 3)
  expect_true(all(sapply(folds, function(x) all(c("train", "test") %in% names(x)))))
  
  # Check that matches stay together
  for (fold in folds) {
    test_matches <- unique(test_data$match_id[fold$test])
    train_matches <- unique(test_data$match_id[fold$train])
    expect_equal(length(intersect(test_matches, train_matches)), 0)
  }
  
  # Test with different parameters
  folds_5 <- create_grouped_cv_folds(test_data, "match_id", k = 5, seed = 123)
  expect_equal(length(folds_5), 5)
})

test_that("create_temporal_splits works correctly", {
  # Create test data with seasons
  test_data <- data.frame(
    match_id = paste0("CD_M", rep(2021:2023, each = 10), "014", sprintf("%02d", 1:10)),
    season = rep(2021:2023, each = 10),
    value = rnorm(30)
  )
  
  splits <- create_temporal_splits(test_data, 
                                  train_seasons = 2021, 
                                  val_seasons = 2022, 
                                  test_seasons = 2023)
  
  expect_true(all(c("train", "validation", "test") %in% names(splits)))
  expect_equal(nrow(splits$train), 10)
  expect_equal(nrow(splits$validation), 10)  
  expect_equal(nrow(splits$test), 10)
  
  # Check season separation
  expect_true(all(splits$train$season == 2021))
  expect_true(all(splits$validation$season == 2022))
  expect_true(all(splits$test$season == 2023))
})

test_that("evaluate_model_comprehensive works correctly", {
  # Create test data
  set.seed(42)
  n <- 1000
  actual <- rbinom(n, 1, 0.6)
  predicted <- plogis(rnorm(n, mean = qlogis(0.6), sd = 1))
  
  # Test basic evaluation
  results <- evaluate_model_comprehensive(actual, predicted, "Test Model", bootstrap_ci = FALSE)
  
  expect_true(is.list(results))
  expect_true("auc" %in% names(results))
  expect_true("log_loss" %in% names(results))
  expect_true("brier_score" %in% names(results))
  expect_true("calibration_slope" %in% names(results))
  
  # Check metric ranges
  expect_true(results$auc >= 0 && results$auc <= 1)
  expect_true(results$log_loss >= 0)
  expect_true(results$brier_score >= 0 && results$brier_score <= 1)
  
  # Test with bootstrap
  results_boot <- evaluate_model_comprehensive(actual[1:200], predicted[1:200], 
                                              "Test Model", bootstrap_ci = TRUE, n_bootstrap = 100)
  expect_true(!is.na(results_boot$auc_ci_lower))
  expect_true(!is.na(results_boot$auc_ci_upper))
})

test_that("compare_models_statistical works correctly", {
  # Create mock evaluation results
  set.seed(42)
  n <- 500
  actual <- rbinom(n, 1, 0.5)
  
  # Model 1: slightly better
  pred1 <- plogis(rnorm(n, mean = ifelse(actual == 1, 0.2, -0.2), sd = 1))
  # Model 2: slightly worse  
  pred2 <- plogis(rnorm(n, mean = ifelse(actual == 1, 0.1, -0.1), sd = 1))
  
  eval1 <- evaluate_model_comprehensive(actual, pred1, "Model1", bootstrap_ci = FALSE)
  eval2 <- evaluate_model_comprehensive(actual, pred2, "Model2", bootstrap_ci = FALSE)
  
  comparison <- compare_models_statistical(list(eval1, eval2), test_type = "delong")
  
  expect_true(is.data.frame(comparison))
  expect_true("p_value" %in% names(comparison))
  expect_true("auc_diff" %in% names(comparison))
  expect_equal(nrow(comparison), 1)
})

test_that("create_validation_report generates proper output", {
  # Create mock results
  mock_results <- list(
    list(model_name = "Test Model", auc = 0.75, log_loss = 0.6, 
         calibration_slope = 1.05, n_observations = 1000)
  )
  
  report <- create_validation_report(mock_results)
  
  expect_true(is.character(report))
  expect_true(nchar(report) > 100)
  expect_true(grepl("MODEL VALIDATION REPORT", report))
  expect_true(grepl("Test Model", report))
})