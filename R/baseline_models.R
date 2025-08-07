# Baseline Models and Model Comparison Framework
# ===============================================
# Implementations of baseline models for proper model evaluation and comparison

library(dplyr)
library(mgcv)
library(glm2)

#' Naive Baseline Model for Win Probability
#'
#' Simple baseline that always predicts 50% win probability
#'
#' @param data Input dataframe (not used, for consistency)
#' @return Vector of 0.5 predictions
#' @export
predict_wp_naive <- function(data) {
  rep(0.5, nrow(data))
}

#' Score-Only Baseline Model for Win Probability
#'
#' Simple logistic regression using only score difference
#'
#' @param train_data Training data with 'points_diff' and 'label_wp'
#' @param pred_data Data to make predictions on
#' @return Vector of win probability predictions
#' @export
#' @importFrom stats glm predict
predict_wp_score_only <- function(train_data, pred_data) {
  
  # Fit simple logistic regression
  model <- stats::glm(
    label_wp ~ points_diff,
    data = train_data,
    family = binomial(link = "logit")
  )
  
  # Make predictions
  predictions <- stats::predict(model, newdata = pred_data, type = "response")
  
  # Bound predictions
  pmax(0.001, pmin(0.999, predictions))
}

#' Time-Aware Baseline Model for Win Probability
#'
#' Logistic regression using score difference and time remaining
#'
#' @param train_data Training data
#' @param pred_data Data to make predictions on
#' @return Vector of win probability predictions
#' @export
predict_wp_time_score <- function(train_data, pred_data) {
  
  # Create time remaining feature if not present
  if (!"time_remaining_pct" %in% names(train_data)) {
    train_data <- train_data %>%
      mutate(
        time_remaining = (4 - period) * 2000 + (2000 - period_seconds),
        time_remaining_pct = time_remaining / 8000
      )
  }
  
  if (!"time_remaining_pct" %in% names(pred_data)) {
    pred_data <- pred_data %>%
      mutate(
        time_remaining = (4 - period) * 2000 + (2000 - period_seconds),
        time_remaining_pct = time_remaining / 8000
      )
  }
  
  # Fit logistic regression with interaction
  model <- stats::glm(
    label_wp ~ points_diff * time_remaining_pct + I(points_diff^2),
    data = train_data,
    family = binomial(link = "logit")
  )
  
  predictions <- stats::predict(model, newdata = pred_data, type = "response")
  pmax(0.001, pmin(0.999, predictions))
}

#' Expected Points Baseline Model
#'
#' Uses expected points differential as the primary predictor
#'
#' @param train_data Training data
#' @param pred_data Data to make predictions on  
#' @return Vector of win probability predictions
#' @export
predict_wp_expected_points <- function(train_data, pred_data) {
  
  # Check for required columns
  required_cols <- c("label_wp", "xpoints_diff", "time_remaining_pct")
  missing_cols <- setdiff(required_cols, names(train_data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in training data:", paste(missing_cols, collapse = ", ")))
  }
  
  # Fit model with expected points
  model <- stats::glm(
    label_wp ~ xpoints_diff + time_remaining_pct + xpoints_diff:time_remaining_pct,
    data = train_data,
    family = binomial(link = "logit")
  )
  
  predictions <- stats::predict(model, newdata = pred_data, type = "response")
  pmax(0.001, pmin(0.999, predictions))
}

#' GAM Baseline Model
#'
#' Generalized Additive Model baseline with smooth terms
#'
#' @param train_data Training data
#' @param pred_data Data to make predictions on
#' @return Vector of win probability predictions  
#' @export
#' @importFrom mgcv gam
predict_wp_gam_baseline <- function(train_data, pred_data) {
  
  # Prepare time features if needed
  if (!"time_remaining_pct" %in% names(train_data)) {
    train_data <- train_data %>%
      mutate(
        time_remaining = (4 - period) * 2000 + (2000 - period_seconds),
        time_remaining_pct = time_remaining / 8000
      )
  }
  
  if (!"time_remaining_pct" %in% names(pred_data)) {
    pred_data <- pred_data %>%
      mutate(
        time_remaining = (4 - period) * 2000 + (2000 - period_seconds),
        time_remaining_pct = time_remaining / 8000
      )
  }
  
  # Fit GAM with smooth terms
  model <- mgcv::gam(
    label_wp ~ s(points_diff, k = 10) + 
               s(time_remaining_pct, k = 8) +
               ti(points_diff, time_remaining_pct, k = c(6, 6)) +
               s(home, bs = "re"),
    data = train_data,
    family = binomial(),
    method = "REML"
  )
  
  predictions <- predict(model, newdata = pred_data, type = "response")
  pmax(0.001, pmin(0.999, predictions))
}

#' Comprehensive Baseline Model Comparison
#'
#' Compares multiple baseline models against the main model
#'
#' @param train_data Training dataset
#' @param test_data Test dataset  
#' @param main_model_preds Predictions from the main model
#' @param include_gam Logical, whether to include GAM baseline (can be slow)
#' @return Dataframe with comparison results
#' @export
compare_baseline_models <- function(train_data, test_data, main_model_preds, include_gam = FALSE) {
  
  # Ensure we have the target variable
  if (!"label_wp" %in% names(test_data)) {
    stop("Test data must contain 'label_wp' column")
  }
  
  actual <- test_data$label_wp
  results <- data.frame()
  
  # Main model performance
  main_eval <- evaluate_model_comprehensive(actual, main_model_preds, "Main Model", 
                                           bootstrap_ci = FALSE)
  results <- rbind(results, data.frame(
    model = "Main Model",
    auc = main_eval$auc,
    log_loss = main_eval$log_loss,
    brier_score = main_eval$brier_score,
    calibration_slope = main_eval$calibration_slope,
    n_params = NA  # Unknown for main model
  ))
  
  # Naive baseline
  naive_preds <- predict_wp_naive(test_data)
  naive_eval <- evaluate_model_comprehensive(actual, naive_preds, "Naive (50%)", 
                                           bootstrap_ci = FALSE)
  results <- rbind(results, data.frame(
    model = "Naive (50%)",
    auc = naive_eval$auc,
    log_loss = naive_eval$log_loss,
    brier_score = naive_eval$brier_score,
    calibration_slope = naive_eval$calibration_slope,
    n_params = 0
  ))
  
  # Score-only baseline
  tryCatch({
    score_preds <- predict_wp_score_only(train_data, test_data)
    score_eval <- evaluate_model_comprehensive(actual, score_preds, "Score Only", 
                                             bootstrap_ci = FALSE)
    results <- rbind(results, data.frame(
      model = "Score Only",
      auc = score_eval$auc,
      log_loss = score_eval$log_loss,
      brier_score = score_eval$brier_score,
      calibration_slope = score_eval$calibration_slope,
      n_params = 2  # intercept + slope
    ))
  }, error = function(e) {
    warning(paste("Score-only baseline failed:", e$message))
  })
  
  # Time + Score baseline
  tryCatch({
    time_score_preds <- predict_wp_time_score(train_data, test_data)
    time_score_eval <- evaluate_model_comprehensive(actual, time_score_preds, "Time + Score", 
                                                   bootstrap_ci = FALSE)
    results <- rbind(results, data.frame(
      model = "Time + Score",
      auc = time_score_eval$auc,
      log_loss = time_score_eval$log_loss,
      brier_score = time_score_eval$brier_score,
      calibration_slope = time_score_eval$calibration_slope,
      n_params = 4  # intercept + 3 terms
    ))
  }, error = function(e) {
    warning(paste("Time + Score baseline failed:", e$message))
  })
  
  # Expected Points baseline (if available)
  if ("xpoints_diff" %in% names(train_data) && "xpoints_diff" %in% names(test_data)) {
    tryCatch({
      exp_preds <- predict_wp_expected_points(train_data, test_data)
      exp_eval <- evaluate_model_comprehensive(actual, exp_preds, "Expected Points", 
                                             bootstrap_ci = FALSE)
      results <- rbind(results, data.frame(
        model = "Expected Points",
        auc = exp_eval$auc,
        log_loss = exp_eval$log_loss,
        brier_score = exp_eval$brier_score,
        calibration_slope = exp_eval$calibration_slope,
        n_params = 3
      ))
    }, error = function(e) {
      warning(paste("Expected Points baseline failed:", e$message))
    })
  }
  
  # GAM baseline (optional, can be slow)
  if (include_gam) {
    tryCatch({
      gam_preds <- predict_wp_gam_baseline(train_data, test_data)
      gam_eval <- evaluate_model_comprehensive(actual, gam_preds, "GAM Baseline", 
                                             bootstrap_ci = FALSE)
      results <- rbind(results, data.frame(
        model = "GAM Baseline",
        auc = gam_eval$auc,
        log_loss = gam_eval$log_loss,
        brier_score = gam_eval$brier_score,
        calibration_slope = gam_eval$calibration_slope,
        n_params = NA  # Variable for GAM
      ))
    }, error = function(e) {
      warning(paste("GAM baseline failed:", e$message))
    })
  }
  
  # Calculate improvements over naive baseline
  naive_log_loss <- results[results$model == "Naive (50%)", "log_loss"]
  results$log_loss_improvement <- (naive_log_loss - results$log_loss) / naive_log_loss * 100
  
  # Rank models
  results <- results %>%
    arrange(log_loss) %>%
    mutate(rank = row_number())
  
  return(results)
}

#' Model Calibration Assessment
#'
#' Comprehensive calibration analysis including reliability plots
#'
#' @param actual Vector of actual binary outcomes
#' @param predicted Vector of predicted probabilities
#' @param n_bins Number of bins for reliability analysis (default: 10)
#' @return List with calibration assessment results
#' @export
assess_model_calibration <- function(actual, predicted, n_bins = 10) {
  
  # Input validation
  if (length(actual) != length(predicted)) {
    stop("Actual and predicted vectors must have same length")
  }
  
  # Remove missing values
  complete_cases <- complete.cases(actual, predicted)
  actual <- actual[complete_cases]
  predicted <- predicted[complete_cases]
  
  # Create calibration bins
  pred_bins <- cut(predicted, 
                   breaks = quantile(predicted, probs = seq(0, 1, length.out = n_bins + 1)),
                   include.lowest = TRUE,
                   labels = paste0("Bin_", 1:n_bins))
  
  # Calculate bin statistics
  calibration_data <- data.frame(
    actual = actual,
    predicted = predicted,
    bin = pred_bins
  ) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      mean_predicted = mean(predicted),
      mean_actual = mean(actual),
      min_pred = min(predicted),
      max_pred = max(predicted),
      .groups = "drop"
    ) %>%
    filter(n >= 10)  # Only use bins with sufficient data
  
  # Calibration slope and intercept
  if (nrow(calibration_data) >= 3) {
    cal_model <- lm(mean_actual ~ mean_predicted, data = calibration_data)
    cal_slope <- coef(cal_model)[2]
    cal_intercept <- coef(cal_model)[1]
    cal_r2 <- summary(cal_model)$r.squared
    
    # Perfect calibration would have slope = 1, intercept = 0
  } else {
    cal_slope <- cal_intercept <- cal_r2 <- NA
  }
  
  # Hosmer-Lemeshow test approximation
  if (nrow(calibration_data) >= 3) {
    hl_stat <- sum((calibration_data$mean_actual * calibration_data$n - 
                    calibration_data$mean_predicted * calibration_data$n)^2 / 
                   (calibration_data$n * calibration_data$mean_predicted * 
                    (1 - calibration_data$mean_predicted)))
    hl_df <- nrow(calibration_data) - 2
    hl_p_value <- 1 - pchisq(hl_stat, df = hl_df)
  } else {
    hl_stat <- hl_p_value <- NA
  }
  
  # Brier score decomposition
  brier_score <- mean((predicted - actual)^2)
  
  # Reliability (within-bin variance)
  reliability <- sum(calibration_data$n * (calibration_data$mean_predicted - calibration_data$mean_actual)^2) / 
                 sum(calibration_data$n)
  
  # Resolution (between-bin variance)
  overall_base_rate <- mean(actual)
  resolution <- sum(calibration_data$n * (calibration_data$mean_actual - overall_base_rate)^2) / 
                sum(calibration_data$n)
  
  # Uncertainty (irreducible)
  uncertainty <- overall_base_rate * (1 - overall_base_rate)
  
  # Brier score = Reliability - Resolution + Uncertainty
  brier_decomp_check <- reliability - resolution + uncertainty
  
  # Calibration quality assessment
  cal_quality <- case_when(
    abs(cal_slope - 1) <= 0.05 & abs(cal_intercept) <= 0.05 ~ "Excellent",
    abs(cal_slope - 1) <= 0.1 & abs(cal_intercept) <= 0.1 ~ "Good", 
    abs(cal_slope - 1) <= 0.2 & abs(cal_intercept) <= 0.2 ~ "Fair",
    TRUE ~ "Poor"
  )
  
  return(list(
    # Basic calibration metrics
    calibration_slope = cal_slope,
    calibration_intercept = cal_intercept,
    calibration_r2 = cal_r2,
    calibration_quality = cal_quality,
    
    # Statistical tests
    hosmer_lemeshow_stat = hl_stat,
    hosmer_lemeshow_p = hl_p_value,
    
    # Brier score decomposition
    brier_score = brier_score,
    reliability = reliability,
    resolution = resolution,
    uncertainty = uncertainty,
    brier_decomp_check = brier_decomp_check,
    
    # Bin-level data for plotting
    calibration_data = calibration_data,
    
    # Summary
    n_observations = length(actual),
    n_bins_used = nrow(calibration_data),
    base_rate = overall_base_rate
  ))
}

#' Generate Calibration Plot Data
#'
#' Prepares data for creating reliability/calibration plots
#'
#' @param calibration_results Results from assess_model_calibration
#' @return ggplot2-ready dataframe for calibration plots
#' @export
prepare_calibration_plot <- function(calibration_results) {
  
  if (is.null(calibration_results$calibration_data)) {
    stop("Calibration results must contain calibration_data")
  }
  
  plot_data <- calibration_results$calibration_data %>%
    mutate(
      # Add perfect calibration line
      perfect_line = mean_predicted,
      
      # Add confidence intervals (approximate)
      se = sqrt(mean_predicted * (1 - mean_predicted) / n),
      ci_lower = pmax(0, mean_actual - 1.96 * se),
      ci_upper = pmin(1, mean_actual + 1.96 * se),
      
      # Size for plotting (based on sample size)
      point_size = pmax(1, pmin(5, n / 100))
    )
  
  return(plot_data)
}

#' Create Model Comparison Report
#'
#' Generates a comprehensive report comparing models including baselines
#'
#' @param comparison_results Results from compare_baseline_models
#' @param calibration_results Optional calibration assessment results
#' @return Character string with formatted report
#' @export
create_model_comparison_report <- function(comparison_results, calibration_results = NULL) {
  
  report <- paste0(
    "MODEL COMPARISON REPORT\n",
    "=======================\n\n"
  )
  
  # Model rankings
  report <- paste0(report, "MODEL RANKINGS (by Log Loss):\n")
  for (i in 1:nrow(comparison_results)) {
    row <- comparison_results[i, ]
    report <- paste0(report, sprintf(
      "%d. %s: Log Loss = %.4f, AUC = %.4f, Improvement = +%.1f%%\n",
      row$rank, row$model, row$log_loss, row$auc, row$log_loss_improvement
    ))
  }
  
  # Best model summary
  best_model <- comparison_results[1, ]
  report <- paste0(report, sprintf(
    "\nBEST MODEL: %s\n",
    best_model$model
  ))
  
  # Performance gaps
  if (nrow(comparison_results) > 1) {
    main_model_row <- comparison_results[comparison_results$model == "Main Model", ]
    if (nrow(main_model_row) > 0) {
      naive_row <- comparison_results[comparison_results$model == "Naive (50%)", ]
      
      if (nrow(naive_row) > 0) {
        improvement_vs_naive <- main_model_row$log_loss_improvement
        report <- paste0(report, sprintf(
          "Main Model improves %.1f%% over naive baseline\n",
          improvement_vs_naive
        ))
      }
    }
  }
  
  # Calibration assessment
  if (!is.null(calibration_results)) {
    report <- paste0(report, sprintf(
      "\nCALIBRATION ASSESSMENT:\n",
      "Calibration Quality: %s\n",
      "Calibration Slope: %.4f (ideal: 1.0)\n",
      "Calibration Intercept: %.4f (ideal: 0.0)\n",
      "Hosmer-Lemeshow p-value: %.4f\n",
      calibration_results$calibration_quality,
      calibration_results$calibration_slope %||% NA,
      calibration_results$calibration_intercept %||% NA,
      calibration_results$hosmer_lemeshow_p %||% NA
    ))
  }
  
  return(report)
}