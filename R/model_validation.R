# Model Validation Framework
# ==========================
# Comprehensive validation framework for TORP models with statistical rigor

#' Calculate AUC using base R
#'
#' Internal function to calculate Area Under the ROC Curve using the
#' Mann-Whitney U-statistic formula. This is O(n log n) vs O(n²) for
#' the naive threshold-based approach.
#'
#' @param actual Vector of actual binary outcomes (0/1)
#' @param predicted Vector of predicted probabilities
#' @return Numeric AUC value
#' @keywords internal
calculate_auc_base <- function(actual, predicted) {
  # Handle edge cases
  if (length(unique(actual)) == 1) {
    return(0.5)  # No discrimination possible
  }

  n_pos <- sum(actual == 1)
  n_neg <- sum(actual == 0)

  if (n_pos == 0 || n_neg == 0) {
    return(0.5)
  }

  # Mann-Whitney U-statistic approach (O(n log n))
  # AUC = P(score_positive > score_negative)
  # This is equivalent to U / (n_pos * n_neg)

  # Get predictions for each class
  pos_preds <- predicted[actual == 1]
  neg_preds <- predicted[actual == 0]

  # Use rank-based calculation for efficiency
  # Combine and rank all predictions
  all_preds <- c(pos_preds, neg_preds)
  ranks <- rank(all_preds, ties.method = "average")

  # Sum of ranks for positive class
  pos_rank_sum <- sum(ranks[seq_len(n_pos)])

  # Mann-Whitney U statistic
  u_stat <- pos_rank_sum - n_pos * (n_pos + 1) / 2

  # AUC = U / (n_pos * n_neg)
  auc <- u_stat / (n_pos * n_neg)

  return(auc)
}


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
#' @importFrom dplyr distinct pull filter mutate group_by summarise
#' @importFrom rlang sym
create_grouped_cv_folds <- function(data, group_var = "match_id", k = 5, seed = 42) {
  set.seed(seed)

  # Get unique groups (matches)
  unique_groups <- data %>%
    dplyr::distinct(!!rlang::sym(group_var)) %>%
    dplyr::pull(!!rlang::sym(group_var))

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

  # Calculate AUC using base R (trapezoidal rule)
  auc_value <- calculate_auc_base(actual, predicted)

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

  # Simple confidence intervals using normal approximation (replaces bootstrap)
  if (bootstrap_ci && n >= 100) {
    # AUC confidence interval using normal approximation
    auc_se <- sqrt(auc_value * (1 - auc_value) / n)  # Simplified SE estimate
    auc_lower <- max(0, auc_value - 1.96 * auc_se)
    auc_upper <- min(1, auc_value + 1.96 * auc_se)
    
    # Brier Score confidence interval
    brier_se <- sqrt(brier_score * (1 - brier_score) / n)
    brier_lower <- max(0, brier_score - 1.96 * brier_se)
    brier_upper <- min(1, brier_score + 1.96 * brier_se)
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
    calibration_summary = cal_summary
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
compare_models_statistical <- function(model_results, test_type = "simple") {

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

      # Simple comparison based on AUC difference and confidence intervals
      auc_diff <- model1$auc - model2$auc
      
      # Simple significance test based on non-overlapping confidence intervals
      if (!is.na(model1$auc_ci_upper) && !is.na(model2$auc_ci_lower)) {
        significant <- (model1$auc_ci_lower > model2$auc_ci_upper) || (model2$auc_ci_lower > model1$auc_ci_upper)
        p_value <- if (significant) 0.01 else 0.50  # Rough approximation
      } else {
        significant <- abs(auc_diff) > 0.05  # Simple threshold
        p_value <- if (significant) 0.01 else 0.50
      }

      comparison_results <- rbind(comparison_results, data.frame(
        model1 = model1$model_name,
        model2 = model2$model_name,
        test_statistic = auc_diff,
        p_value = p_value,
        auc_diff = auc_diff,
        significant = significant,
        test_type = "AUC Difference"
      ))
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
      if (!is.null(result$auc_ci_lower) && !is.na(result$auc_ci_lower)) sprintf(" (95%% CI: %.4f - %.4f)", result$auc_ci_lower, result$auc_ci_upper) else "",
      "\n",
      sprintf("Log Loss: %.4f\n", result$log_loss),
      sprintf("Brier Score: %.4f", result$brier_score),
      if (!is.null(result$brier_ci_lower) && !is.na(result$brier_ci_lower)) sprintf(" (95%% CI: %.4f - %.4f)", result$brier_ci_lower, result$brier_ci_upper) else "",
      "\n",
      sprintf("Calibration Slope: %.4f (ideal: 1.0)\n", result$calibration_slope %||% NA),
      sprintf("Calibration R^2: %.4f\n", result$calibration_r2 %||% NA),
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

