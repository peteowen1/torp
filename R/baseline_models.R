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
#' @keywords internal
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

#' GAM Baseline Model
#'
#' Generalized Additive Model baseline with smooth terms
#'
#' @param data Data to make predictions on (if only one parameter provided)
#' @param pred_data Optional - data to make predictions on
#' @return Vector of win probability predictions
#' @keywords internal
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
    time_remaining_pct <- pmax(0, pmin(1, calculate_time_remaining_pct(data$period, data$period_seconds)))

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
    train_data <- train_data |>
      mutate(time_remaining_pct = calculate_time_remaining_pct(period, period_seconds))
  }

  if (!"time_remaining_pct" %in% names(pred_data)) {
    pred_data <- pred_data |>
      mutate(time_remaining_pct = calculate_time_remaining_pct(period, period_seconds))
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
  ) |>
    group_by(bin) |>
    summarise(
      n = n(),
      mean_predicted = mean(predicted),
      mean_actual = mean(actual),
      min_pred = min(predicted),
      max_pred = max(predicted),
      .groups = "drop"
    ) |>
    filter(n >= 10)  # Only use bins with sufficient data
  
  # Calibration slope and intercept. Uses the GLM logit convention --
  # coef(glm(actual ~ qlogis(predicted), binomial))[2] -- fit on the raw
  # observations, matching model_validation.R's `evaluate_predictions()` and
  # the experiments/trainer convention used elsewhere (e.g. torpmodels'
  # train_lib.R wp_gate_slope()), rather than the previous decile-binned OLS
  # (lm(mean_actual ~ mean_predicted) over `calibration_data`'s bins), which
  # measures a related but distinct quantity. `calibration_data` (the
  # per-bin breakdown) is retained unchanged for the Hosmer-Lemeshow test and
  # reliability/resolution/uncertainty decomposition below -- it no longer
  # feeds the slope/intercept calculation.
  if (length(unique(actual)) >= 2) {
    p_clamped <- pmin(pmax(predicted, 1e-6), 1 - 1e-6)
    cal_model <- glm(actual ~ qlogis(p_clamped), family = binomial())
    co <- unname(coef(cal_model))
    cal_slope <- co[2]
    cal_intercept <- co[1]
    # McFadden pseudo-R^2 -- the GLM analog of lm's R^2 (not directly
    # comparable in scale); informational only.
    cal_r2 <- 1 - cal_model$deviance / cal_model$null.deviance

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

#' Simple Time-Only Win Probability Prediction
#'
#' Predicts win probability based only on time remaining in the game
#'
#' @param data Input data with period and period_seconds columns
#' @return Vector of win probability predictions
#' @keywords internal
predict_wp_time_only <- function(data) {
  # Calculate time remaining
  time_remaining_pct <- pmax(0, pmin(1, calculate_time_remaining_pct(data$period, data$period_seconds)))
  
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
#' @keywords internal
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
    }, error = function(e) {
      cli::cli_warn("AUC calculation failed for {.val {model_name}}: {conditionMessage(e)}")
      NA
    })

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
  dplyr::bind_rows(results_list) |>
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
#' @description This function is intended for internal use and may be unexported in a future release.
#' Creates data structure for plotting model calibration.
#'
#' A simplified wrapper around assess_model_calibration for creating calibration
#' plot data. For more comprehensive calibration analysis, use assess_model_calibration
#' directly followed by prepare_calibration_plot.
#'
#' @param actual Vector of actual binary outcomes
#' @param predicted Vector of predicted probabilities
#' @param n_bins Number of bins for calibration plot
#' @return List with plot data and statistics
#' @keywords internal
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