# Baseline Models and Model Comparison Framework
# ===============================================
# Implementations of baseline models for proper model evaluation and comparison


#' Naive Baseline Model for Win Probability
#'
#' Simple baseline that always predicts 50% win probability
#'
#' @param data Input dataframe (not used, for consistency)
#' @return Vector of 0.5 predictions
#' @keywords internal
predict_wp_naive <- function(data) {
  rep(0.5, nrow(data))
}

#' Score-Only Baseline Model for Win Probability
#'
#' Simple logistic regression using only score difference
#'
#' @param data Data with 'points_diff' column (if only one parameter provided, uses internal training)
#' @param pred_data Optional - data to make predictions on
#' @return Vector of win probability predictions
#' @export
#' @importFrom stats glm predict
predict_wp_score_only <- function(data, pred_data = NULL) {
  
  # If only one parameter provided, use simple heuristic
  if (is.null(pred_data)) {
    # Simple score-based prediction without training
    if (!"points_diff" %in% names(data)) {
      cli::cli_abort("Data must contain 'points_diff' column")
    }
    
    # Simple logistic transformation of points difference
    # Roughly: WP = 1 / (1 + exp(-points_diff/WP_SCORE_SCALING))
    predictions <- 1 / (1 + exp(-data$points_diff / WP_SCORE_SCALING))
    return(pmax(0.001, pmin(0.999, predictions)))
  }
  
  # Original two-parameter version for when training data is provided
  train_data <- data
  
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
#' @importFrom dplyr mutate
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
    cli::cli_abort("Missing required columns in training data: {paste(missing_cols, collapse = ', ')}")
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
#' @param data Data to make predictions on (if only one parameter provided)
#' @param pred_data Optional - data to make predictions on
#' @return Vector of win probability predictions  
#' @export
#' @importFrom mgcv gam
#' @importFrom dplyr mutate
predict_wp_gam_baseline <- function(data, pred_data = NULL) {
  
  # If only one parameter provided, use simple heuristic
  if (is.null(pred_data)) {
    # Simple GAM-like prediction without training
    if (!all(c("points_diff", "period", "period_seconds") %in% names(data))) {
      cli::cli_abort("Data must contain 'points_diff', 'period', and 'period_seconds' columns")
    }
    
    # Calculate time remaining
    time_remaining <- (AFL_MAX_PERIODS - data$period) * AFL_QUARTER_DURATION +
                      (AFL_QUARTER_DURATION - data$period_seconds)
    time_remaining_pct <- pmax(0, pmin(1, time_remaining / AFL_TOTAL_GAME_SECONDS))

    # Simple non-linear combination of score and time
    score_effect <- 1 / (1 + exp(-data$points_diff / WP_TIME_SCALING))
    time_effect <- 0.5 + 0.2 * (0.5 - time_remaining_pct)
    
    # Combine effects
    predictions <- 0.7 * score_effect + 0.3 * time_effect
    return(pmax(0.001, pmin(0.999, predictions)))
  }
  
  # Original two-parameter version
  train_data <- data
  
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
#' @keywords internal
#' @importFrom dplyr mutate row_number group_by summarise filter bind_rows arrange
compare_baseline_models <- function(train_data, test_data, main_model_preds, include_gam = FALSE) {

  # Ensure we have the target variable
  if (!"label_wp" %in% names(test_data)) {
    cli::cli_abort("Test data must contain 'label_wp' column")
  }

  actual <- test_data$label_wp

  # Pre-allocate list for results (more efficient than repeated rbind)
  results_list <- list()

  # Helper to create result row
  make_result_row <- function(model_name, eval_result, n_params) {
    data.frame(
      model = model_name,
      auc = eval_result$auc,
      log_loss = eval_result$log_loss,
      brier_score = eval_result$brier_score,
      calibration_slope = eval_result$calibration_slope,
      n_params = n_params,
      stringsAsFactors = FALSE
    )
  }

  # Main model performance
  main_eval <- evaluate_model_comprehensive(actual, main_model_preds, "Main Model",
                                           bootstrap_ci = FALSE)
  results_list[["main"]] <- make_result_row("Main Model", main_eval, NA)

  # Naive baseline
  naive_preds <- predict_wp_naive(test_data)
  naive_eval <- evaluate_model_comprehensive(actual, naive_preds, "Naive (50%)",
                                           bootstrap_ci = FALSE)
  results_list[["naive"]] <- make_result_row("Naive (50%)", naive_eval, 0)

  # Score-only baseline
  tryCatch({
    score_preds <- predict_wp_score_only(train_data, test_data)
    score_eval <- evaluate_model_comprehensive(actual, score_preds, "Score Only",
                                             bootstrap_ci = FALSE)
    results_list[["score"]] <- make_result_row("Score Only", score_eval, 2)
  }, error = function(e) {
    warning(paste("Score-only baseline failed:", e$message))
  })

  # Time + Score baseline
  tryCatch({
    time_score_preds <- predict_wp_time_score(train_data, test_data)
    time_score_eval <- evaluate_model_comprehensive(actual, time_score_preds, "Time + Score",
                                                   bootstrap_ci = FALSE)
    results_list[["time_score"]] <- make_result_row("Time + Score", time_score_eval, 4)
  }, error = function(e) {
    warning(paste("Time + Score baseline failed:", e$message))
  })

  # Expected Points baseline (if available)
  if ("xpoints_diff" %in% names(train_data) && "xpoints_diff" %in% names(test_data)) {
    tryCatch({
      exp_preds <- predict_wp_expected_points(train_data, test_data)
      exp_eval <- evaluate_model_comprehensive(actual, exp_preds, "Expected Points",
                                             bootstrap_ci = FALSE)
      results_list[["expected_pts"]] <- make_result_row("Expected Points", exp_eval, 3)
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
      results_list[["gam"]] <- make_result_row("GAM Baseline", gam_eval, NA)
    }, error = function(e) {
      warning(paste("GAM baseline failed:", e$message))
    })
  }

  # Combine results efficiently
  results <- dplyr::bind_rows(results_list)

  # Calculate improvements over naive baseline
  naive_log_loss <- results[results$model == "Naive (50%)", "log_loss"]
  results$log_loss_improvement <- (naive_log_loss - results$log_loss) / naive_log_loss * 100

  # Rank models
  results <- results %>%
    dplyr::arrange(log_loss) %>%
    dplyr::mutate(rank = dplyr::row_number())

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
#' @keywords internal
assess_model_calibration <- function(actual, predicted, n_bins = 10) {
  
  # Input validation
  if (length(actual) != length(predicted)) {
    cli::cli_abort("Actual and predicted vectors must have same length")
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
  cal_quality <- dplyr::case_when(
    abs(cal_slope - 1) <= 0.05 & abs(cal_intercept) <= 0.05 ~ "Excellent",
    abs(cal_slope - 1) <= 0.1 & abs(cal_intercept) <= 0.1 ~ "Good", 
    abs(cal_slope - 1) <= 0.2 & abs(cal_intercept) <= 0.2 ~ "Fair",
    TRUE ~ "Poor"
  )
  
  # Calibration in the large (overall calibration)
  calibration_in_large <- abs(mean(predicted) - mean(actual))
  
  return(list(
    # Basic calibration metrics
    calibration_slope = cal_slope,
    calibration_intercept = cal_intercept,
    calibration_r2 = cal_r2,
    calibration_quality = cal_quality,
    calibration_in_large = calibration_in_large,
    
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
#' @keywords internal
#' @importFrom dplyr mutate
prepare_calibration_plot <- function(calibration_results) {
  
  if (is.null(calibration_results$calibration_data)) {
    cli::cli_abort("Calibration results must contain calibration_data")
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

#' Simple Time-Only Win Probability Prediction
#'
#' Predicts win probability based only on time remaining in the game
#'
#' @param data Input data with period and period_seconds columns
#' @return Vector of win probability predictions
#' @keywords internal
predict_wp_time_only <- function(data) {
  # Calculate time remaining
  time_remaining <- (4 - data$period) * 2000 + (2000 - data$period_seconds)
  time_remaining_pct <- pmax(0, pmin(1, time_remaining / 8000))
  
  # Simple logistic model based on time remaining
  # More time remaining = closer to 50% (more uncertain)
  # Less time remaining = more extreme (closer to current state)
  predictions <- 0.5 + 0.3 * (0.5 - time_remaining_pct)
  
  # Bound predictions
  pmax(0.001, pmin(0.999, predictions))
}

#' Ensemble Baseline Win Probability Prediction
#'
#' Combines multiple baseline models using simple averaging
#'
#' @param data Input data 
#' @return Vector of ensemble win probability predictions
#' @export
predict_wp_ensemble_baseline <- function(data) {
  # Combine different baseline approaches
  naive_pred <- predict_wp_naive(data)
  
  # If we have the required columns, use more sophisticated baselines
  if (all(c("points_diff", "period", "period_seconds") %in% names(data))) {
    time_pred <- predict_wp_time_only(data)
    
    # Simple ensemble: average of naive and time-based
    ensemble_pred <- (naive_pred + time_pred) / 2
  } else {
    # Fall back to naive if columns missing
    ensemble_pred <- naive_pred
  }
  
  # Bound predictions
  pmax(0.001, pmin(0.999, ensemble_pred))
}

#' Evaluate Multiple Baseline Models
#'
#' Comprehensive evaluation of baseline models against actual outcomes
#'
#' @param actual Vector of actual binary outcomes
#' @param data Input data for making predictions
#' @return Data frame with model evaluation results
#' @keywords internal
#' @importFrom dplyr arrange bind_rows desc
evaluate_baseline_models <- function(actual, data) {
  # Pre-allocate list for results (more efficient than repeated rbind)
  results_list <- list()

  # Helper to create result row
  make_result_row <- function(model_name, preds) {
    auc_val <- tryCatch({
      calculate_auc_base(actual, preds)
    }, error = function(e) NA)

    data.frame(
      model_name = model_name,
      auc = auc_val,
      log_loss = calculate_log_loss(actual, preds),
      brier_score = calculate_brier_score(actual, preds),
      stringsAsFactors = FALSE
    )
  }

  # Naive baseline
  naive_preds <- predict_wp_naive(data)
  results_list[["naive"]] <- make_result_row("Naive", naive_preds)

  # Time-only baseline
  if (all(c("period", "period_seconds") %in% names(data))) {
    time_preds <- predict_wp_time_only(data)
    results_list[["time"]] <- make_result_row("Time Only", time_preds)
  }

  # Ensemble baseline
  ensemble_preds <- predict_wp_ensemble_baseline(data)
  results_list[["ensemble"]] <- make_result_row("Ensemble", ensemble_preds)

  # Combine and sort by AUC descending
  dplyr::bind_rows(results_list) %>%
    dplyr::arrange(dplyr::desc(auc))
}

#' Simple Log Loss Calculation
#'
#' @param actual Vector of actual binary outcomes
#' @param predicted Vector of predicted probabilities
#' @return Numeric log loss value
#' @keywords internal
calculate_log_loss <- function(actual, predicted) {
  # Bound predictions to avoid log(0)
  predicted <- pmax(1e-15, pmin(1 - 1e-15, predicted))
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

#' Simple Brier Score Calculation
#'
#' @param actual Vector of actual binary outcomes
#' @param predicted Vector of predicted probabilities
#' @return Numeric Brier score value
#' @keywords internal
calculate_brier_score <- function(actual, predicted) {
  mean((predicted - actual)^2)
}

#' Create Calibration Plot Data
#'
#' Creates data structure for plotting model calibration
#'
#' A simplified wrapper around assess_model_calibration for creating calibration
#' plot data. For more comprehensive calibration analysis, use assess_model_calibration
#' directly followed by prepare_calibration_plot.
#'
#' @param actual Vector of actual binary outcomes
#' @param predicted Vector of predicted probabilities
#' @param n_bins Number of bins for calibration plot
#' @return List with plot data and statistics
#' @export
#' @seealso \code{\link{assess_model_calibration}}, \code{\link{prepare_calibration_plot}}
create_calibration_plot <- function(actual, predicted, n_bins = 10) {
  # Use the comprehensive assess_model_calibration internally
  calibration_results <- assess_model_calibration(actual, predicted, n_bins)

  return(list(
    plot_data = calibration_results$calibration_data,
    n_bins = calibration_results$n_bins_used,
    total_n = length(actual)
  ))
}