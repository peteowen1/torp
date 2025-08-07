# Model Validation Framework
# ==========================
# Comprehensive validation framework for TORP models with statistical rigor

library(dplyr)
library(caret)
library(pROC)
library(boot)
library(broom)

#' Create Grouped Cross-Validation Folds
#'
#' Creates cross-validation folds ensuring matches stay together to prevent data leakage
#'
#' @param data A dataframe containing model data
#' @param group_var Character string specifying the grouping variable (default: "match_id")
#' @param k Number of folds (default: 5)
#' @param seed Random seed for reproducibility
#' @return List of fold indices
#' @export
#' @importFrom dplyr distinct pull
create_grouped_cv_folds <- function(data, group_var = "match_id", k = 5, seed = 42) {
  set.seed(seed)
  
  # Get unique groups (matches)
  unique_groups <- data %>%
    dplyr::distinct(!!sym(group_var)) %>%
    dplyr::pull(!!sym(group_var))
  
  # Shuffle groups
  unique_groups <- sample(unique_groups)
  
  # Create approximately equal-sized folds
  fold_assignment <- cut(seq_along(unique_groups), breaks = k, labels = FALSE)
  
  # Create fold indices
  folds <- list()
  for (i in 1:k) {
    test_groups <- unique_groups[fold_assignment == i]
    test_indices <- which(data[[group_var]] %in% test_groups)
    folds[[i]] <- list(
      train = setdiff(seq_len(nrow(data)), test_indices),
      test = test_indices
    )
  }
  
  return(folds)
}

#' Time-Based Train/Validation/Test Split
#'
#' Creates proper temporal splits for time series data to prevent data leakage
#'
#' @param data A dataframe containing model data with date/season information
#' @param train_seasons Vector of seasons for training
#' @param val_seasons Vector of seasons for validation
#' @param test_seasons Vector of seasons for testing
#' @return List containing train, validation, and test datasets
#' @export
create_temporal_splits <- function(data, train_seasons, val_seasons, test_seasons) {
  # Extract season from match_id or use provided season column
  if (!"season" %in% names(data) && "match_id" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(
        season = as.numeric(substr(.data$match_id, 5, 8))
      )
  }
  
  train_data <- data %>% dplyr::filter(.data$season %in% train_seasons)
  val_data <- data %>% dplyr::filter(.data$season %in% val_seasons)
  test_data <- data %>% dplyr::filter(.data$season %in% test_seasons)
  
  return(list(
    train = train_data,
    validation = val_data,
    test = test_data
  ))
}

#' Comprehensive Model Evaluation
#'
#' Evaluates model performance with multiple metrics and statistical rigor
#'
#' @param actual Vector of actual outcomes
#' @param predicted Vector of predicted probabilities
#' @param model_name Name of the model being evaluated
#' @param bootstrap_ci Logical, whether to compute bootstrap confidence intervals
#' @param n_bootstrap Number of bootstrap samples (default: 1000)
#' @return List containing evaluation metrics with confidence intervals
#' @export
#' @importFrom pROC roc auc ci.auc
#' @importFrom boot boot boot.ci
evaluate_model_comprehensive <- function(actual, predicted, model_name = "Model", 
                                       bootstrap_ci = TRUE, n_bootstrap = 1000) {
  
  # Input validation
  if (length(actual) != length(predicted)) {
    stop("Actual and predicted vectors must have the same length")
  }
  
  if (any(is.na(actual)) || any(is.na(predicted))) {
    warning("Missing values detected, removing them")
    complete_cases <- complete.cases(actual, predicted)
    actual <- actual[complete_cases]
    predicted <- predicted[complete_cases]
  }
  
  # Core metrics
  n <- length(actual)
  
  # Discrimination metrics
  roc_obj <- pROC::roc(actual, predicted, quiet = TRUE)
  auc_value <- as.numeric(pROC::auc(roc_obj))
  
  # Calibration metrics
  log_loss <- -mean(actual * log(predicted + 1e-15) + (1 - actual) * log(1 - predicted + 1e-15))
  brier_score <- mean((predicted - actual)^2)
  
  # Calibration slope (should be close to 1.0)
  cal_data <- data.frame(
    actual = actual,
    predicted = predicted,
    pred_decile = cut(predicted, breaks = quantile(predicted, probs = 0:10/10), include.lowest = TRUE)
  )
  
  cal_summary <- cal_data %>%
    group_by(pred_decile) %>%
    summarise(
      pred_mean = mean(predicted),
      actual_mean = mean(actual),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n >= 10)  # Only use deciles with sufficient data
  
  if (nrow(cal_summary) >= 3) {
    cal_model <- lm(actual_mean ~ pred_mean, data = cal_summary)
    calibration_slope <- coef(cal_model)[2]
    calibration_intercept <- coef(cal_model)[1]
    calibration_r2 <- summary(cal_model)$r.squared
  } else {
    calibration_slope <- NA
    calibration_intercept <- NA
    calibration_r2 <- NA
  }
  
  # Bootstrap confidence intervals if requested
  if (bootstrap_ci && n >= 100) {
    
    # Bootstrap function for AUC
    boot_auc <- function(data, indices) {
      d <- data[indices, ]
      roc_boot <- pROC::roc(d$actual, d$predicted, quiet = TRUE)
      return(as.numeric(pROC::auc(roc_boot)))
    }
    
    # Bootstrap function for Brier Score
    boot_brier <- function(data, indices) {
      d <- data[indices, ]
      return(mean((d$predicted - d$actual)^2))
    }
    
    boot_data <- data.frame(actual = actual, predicted = predicted)
    
    # Bootstrap AUC
    boot_auc_results <- boot::boot(boot_data, boot_auc, R = n_bootstrap)
    auc_ci <- boot::boot.ci(boot_auc_results, type = "perc")
    
    # Bootstrap Brier Score
    boot_brier_results <- boot::boot(boot_data, boot_brier, R = n_bootstrap)
    brier_ci <- boot::boot.ci(boot_brier_results, type = "perc")
    
    auc_lower <- auc_ci$percent[4]
    auc_upper <- auc_ci$percent[5]
    brier_lower <- brier_ci$percent[4]
    brier_upper <- brier_ci$percent[5]
    
  } else {
    auc_lower <- auc_upper <- NA
    brier_lower <- brier_upper <- NA
  }
  
  # Prediction intervals (simple approach)
  pred_residuals <- actual - predicted
  pred_std <- sd(pred_residuals)
  
  # Return comprehensive results
  results <- list(
    model_name = model_name,
    n_observations = n,
    
    # Discrimination
    auc = auc_value,
    auc_ci_lower = auc_lower,
    auc_ci_upper = auc_upper,
    
    # Calibration
    log_loss = log_loss,
    brier_score = brier_score,
    brier_ci_lower = brier_lower,
    brier_ci_upper = brier_upper,
    
    calibration_slope = calibration_slope,
    calibration_intercept = calibration_intercept,
    calibration_r2 = calibration_r2,
    
    # Prediction uncertainty
    prediction_std = pred_std,
    
    # Additional info
    calibration_summary = cal_summary,
    roc_object = roc_obj
  )
  
  class(results) <- "torp_model_evaluation"
  return(results)
}

#' Compare Multiple Models Statistically
#'
#' Performs statistical comparison between multiple models using paired tests
#'
#' @param model_results List of model evaluation results
#' @param test_type Type of statistical test ("mcnemar", "delong")
#' @return Dataframe with pairwise comparison results
#' @export
compare_models_statistical <- function(model_results, test_type = "delong") {
  
  n_models <- length(model_results)
  if (n_models < 2) {
    stop("Need at least 2 models for comparison")
  }
  
  model_names <- sapply(model_results, function(x) x$model_name)
  
  # Create comparison matrix
  comparison_results <- data.frame()
  
  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      
      model1 <- model_results[[i]]
      model2 <- model_results[[j]]
      
      if (test_type == "delong") {
        # DeLong test for AUC comparison
        test_result <- pROC::roc.test(model1$roc_object, model2$roc_object, method = "delong")
        
        comparison_results <- rbind(comparison_results, data.frame(
          model1 = model1$model_name,
          model2 = model2$model_name,
          test_statistic = test_result$statistic,
          p_value = test_result$p.value,
          auc_diff = model1$auc - model2$auc,
          significant = test_result$p.value < 0.05,
          test_type = "DeLong AUC"
        ))
      }
    }
  }
  
  return(comparison_results)
}

#' Create Model Validation Report
#'
#' Generates a comprehensive model validation report
#'
#' @param evaluation_results List of evaluation results
#' @param comparison_results Dataframe of model comparisons (optional)
#' @return Character string containing formatted report
#' @export
create_validation_report <- function(evaluation_results, comparison_results = NULL) {
  
  report <- paste0(
    "MODEL VALIDATION REPORT\n",
    "=======================\n\n"
  )
  
  # Individual model results
  for (result in evaluation_results) {
    report <- paste0(report,
      sprintf("Model: %s\n", result$model_name),
      sprintf("Observations: %d\n", result$n_observations),
      sprintf("AUC: %.4f", result$auc),
      if (!is.na(result$auc_ci_lower)) sprintf(" (95%% CI: %.4f - %.4f)", result$auc_ci_lower, result$auc_ci_upper) else "",
      "\n",
      sprintf("Log Loss: %.4f\n", result$log_loss),
      sprintf("Brier Score: %.4f", result$brier_score),
      if (!is.na(result$brier_ci_lower)) sprintf(" (95%% CI: %.4f - %.4f)", result$brier_ci_lower, result$brier_ci_upper) else "",
      "\n",
      sprintf("Calibration Slope: %.4f (ideal: 1.0)\n", result$calibration_slope %||% NA),
      sprintf("Calibration R²: %.4f\n", result$calibration_r2 %||% NA),
      "\n"
    )
  }
  
  # Model comparisons
  if (!is.null(comparison_results) && nrow(comparison_results) > 0) {
    report <- paste0(report, "\nMODEL COMPARISONS\n")
    report <- paste0(report, "-----------------\n")
    
    for (i in 1:nrow(comparison_results)) {
      row <- comparison_results[i, ]
      report <- paste0(report,
        sprintf("%s vs %s: p = %.4f %s\n", 
                row$model1, row$model2, row$p_value,
                if (row$significant) "(significant)" else "(not significant)")
      )
    }
  }
  
  return(report)
}

#' Null coalescing operator
#'
#' @param x First value
#' @param y Second value
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}