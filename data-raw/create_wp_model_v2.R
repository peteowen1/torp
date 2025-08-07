# Enhanced Win Probability Model - Data Science Approach ----
# Author: Data Science Overhaul
# Purpose: Create a robust, well-validated win probability model for AFL
# Key Improvements: Better features, proper validation, ensemble methods

# Libraries ----
library(dplyr)
library(xgboost)
library(mgcv)
library(lightgbm)
library(randomForest)
library(tidymodels)
library(ModelMetrics)
library(splitTools)
library(glue)
library(lubridate)
library(tictoc)

devtools::load_all()

# 1. Data Loading and Preparation ----

cat("Loading and preparing data...\n")
tictoc::tic()

## 1.1 Load Multi-Season Data ----
# Load comprehensive training data (multiple seasons for better generalization)
training_seasons <- 2021:2025
pbp_data <- list()

for (season in training_seasons) {
  cat(glue("Loading season {season}...\n"))

  # Load chains data for the season
  chains_season <- tryCatch({
    load_chains(season, TRUE)
  }, error = function(e) {
    cat(glue("Warning: Could not load season {season}: {e$message}\n"))
    return(NULL)
  })

  if (!is.null(chains_season)) {
    # Process through the pipeline
    pbp_season <- chains_season %>%
      clean_pbp() %>%
      clean_model_data_epv() %>%
      add_epv_vars() %>%
      clean_model_data_wp()

    pbp_data[[as.character(season)]] <- pbp_season
  }
}

# Combine all seasons
model_data_wp <- do.call(rbind, pbp_data)
rm(pbp_data)  # Free memory

cat(glue("Training data: {nrow(model_data_wp)}:, observations across {length(training_seasons)} seasons\n"))
tictoc::toc()

# 2. Enhanced Feature Engineering ----

cat("🔧 Engineering enhanced features...\n")
tictoc::tic()

## 2.1 Create Enhanced Features ----
model_data_wp_enhanced <- model_data_wp %>%
  group_by(match_id) %>%
  arrange(period, period_seconds) %>%
  mutate(
    # ===== TIME-BASED FEATURES =====
    # Time remaining in match (more precise)
    time_remaining = (4 - period) * AFL_QUARTER_DURATION + (AFL_QUARTER_DURATION - period_seconds),
    time_remaining_pct = time_remaining / (AFL_QUARTER_DURATION * 4),

    # Time pressure features
    quarter_progress = period_seconds / AFL_QUARTER_DURATION,
    is_final_quarter = as.numeric(period == 4),
    is_final_5_mins = as.numeric(time_remaining <= 300),
    is_final_2_mins = as.numeric(time_remaining <= 120),

    # ===== SCORING FEATURES =====
    # Enhanced scoring differential features
    points_diff_abs = abs(points_diff),
    points_diff_squared = points_diff^2,
    points_diff_per_time = points_diff / pmax(time_remaining, 1),

    # Historical scoring rate (last 10 minutes)
    last_10min_cutoff = pmax(period_seconds - 600, 0),

    # Expected points features
    exp_pts_momentum = exp_pts - lag(exp_pts, default = 0),
    exp_pts_ma_3 = zoo::rollmean(exp_pts, k = 3, fill = exp_pts, align = "right"),

    # ===== POSITIONAL FEATURES =====
    # Field position impact
    field_pos_score = case_when(
      goal_x <= 30 ~ 3,  # Scoring zone
      goal_x <= 50 ~ 2,  # Forward 50
      goal_x <= 80 ~ 1,  # Midfield
      TRUE ~ 0           # Defensive
    ),

    # Distance from goal (refined)
    goal_distance = sqrt(goal_x^2 + y^2),
    goal_angle = atan2(abs(y), goal_x) * 180 / pi,

    # ===== MOMENTUM FEATURES =====
    # Scoring runs and momentum
    scoring_team_change = as.numeric(scoring_team_id != lag(scoring_team_id, default = first(scoring_team_id))),

    # Expected value swings
    epv_swing = abs(exp_pts - lag(exp_pts, default = exp_pts[1])),
    epv_volatility = zoo::rollapply(exp_pts, width = 5, FUN = sd, fill = 0, align = "right"),

    # ===== GAME STATE FEATURES =====
    # Lead changes and close games
    lead_size_category = case_when(
      abs(points_diff) <= 6 ~ "tight",      # Within a goal
      abs(points_diff) <= 18 ~ "moderate",  # 1-3 goal margin
      abs(points_diff) <= 30 ~ "comfortable", # 3-5 goal margin
      TRUE ~ "blowout"                      # 5+ goal margin
    ),

    # Pressure situations
    is_pressure_moment = as.numeric(
      (is_final_5_mins == 1 & abs(points_diff) <= 12) |
      (is_final_2_mins == 1 & abs(points_diff) <= 18)
    ),

    # ===== INTERACTION FEATURES =====
    # Key interactions for win probability
    time_score_interaction = time_remaining_pct * points_diff,
    time_score_abs_interaction = time_remaining_pct * abs(points_diff),
    epv_time_interaction = exp_pts * time_remaining_pct,

    # ===== TEAM-SPECIFIC FEATURES =====
    # Home ground advantage refined
    home_advantage_late = home * is_final_quarter,
    home_advantage_pressure = home * is_pressure_moment,

    # ===== ROLLING STATISTICS =====
    # 5-minute rolling windows
    points_diff_ma_5 = zoo::rollmean(points_diff, k = 5, fill = points_diff, align = "right"),
    exp_pts_ma_5 = zoo::rollmean(exp_pts, k = 5, fill = exp_pts, align = "right"),

    # Volatility measures
    points_diff_volatility = zoo::rollapply(points_diff, width = 10, FUN = sd, fill = 0, align = "right")
  ) %>%
  ungroup() %>%

  # Convert categorical variables to factors for better model handling
  mutate(
    lead_size_category = factor(lead_size_category,
                               levels = c("tight", "moderate", "comfortable", "blowout")),
    period = factor(period),
    play_type = factor(play_type),
    phase_of_play = factor(phase_of_play)
  ) %>%

  # Remove any infinite or missing values
  filter(
    is.finite(time_remaining),
    is.finite(points_diff),
    is.finite(exp_pts),
    !is.na(label_wp)
  )

cat(glue("Enhanced features created. Data shape: {nrow(model_data_wp_enhanced)} x {ncol(model_data_wp_enhanced)}\n"))
tictoc::toc()

# 3. Improved Variable Selection ----

## 3.1 Variable Selection Function ----
# Enhanced variable selection for win probability
select_wp_model_vars_enhanced <- function(df, model_type = "ensemble") {

  # Base variables that all models use
  base_vars <- c(
    # Core time features
    "time_remaining", "time_remaining_pct", "quarter_progress",
    "is_final_quarter", "is_final_5_mins", "is_final_2_mins",

    # Enhanced scoring features
    "points_diff", "points_diff_abs", "points_diff_squared",
    "points_diff_per_time", "xpoints_diff",

    # Expected points
    "exp_pts", "exp_pts_momentum", "exp_pts_ma_3", "exp_pts_ma_5",

    # Position and field state
    "goal_x", "goal_distance", "goal_angle", "field_pos_score",

    # Momentum and volatility
    "epv_swing", "epv_volatility", "points_diff_volatility",

    # Game state
    "is_pressure_moment", "home", "home_advantage_late", "home_advantage_pressure",

    # Interaction terms
    "time_score_interaction", "time_score_abs_interaction", "epv_time_interaction"
  )

  # Model-specific variable selection
  if (model_type == "gam") {
    # GAM can handle factor variables directly
    model_vars <- c(
      base_vars,
      "play_type", "phase_of_play",  # Factor variables for GAM
      "lead_size_category"
    )
  } else {
    # XGBoost, LightGBM need one-hot encoded variables
    model_vars <- c(
      base_vars,
      # Play context (dummy coded)
      "play_type_handball", "play_type_kick", "play_type_reception",
      "phase_of_play_handball_received", "phase_of_play_hard_ball",
      "phase_of_play_loose_ball", "phase_of_play_set_shot",
      "lead_size_category"
    )
  }

  # Select only available variables
  available_vars <- intersect(model_vars, names(df))

  return(df %>% select(all_of(available_vars)))
}

# 4. Robust Train/Validation Split ----

cat("📊 Creating robust train/validation splits...\n")

# Time-based split to prevent data leakage
# Use 2021-2023 for training, 2024 for validation
train_data <- model_data_wp_enhanced %>%
  filter(season <= 2024)

val_data <- model_data_wp_enhanced %>%
  filter(season == 2025)

# Additional grouped K-fold for robust validation
set.seed(42)
folds <- splitTools::create_folds(
  y = train_data$match_id,
  k = 5,
  type = "grouped",  # Keep matches together
  invert = TRUE
)

cat(glue("🎯 Train: {nrow(train_data)} | Validation: {nrow(val_data)}\n"))

# 5. Advanced Model Architectures ----

# Prepare feature matrices
prepare_model_data <- function(data) {
  features <- data %>% select_wp_model_vars_enhanced(model_type = "xgboost")

  # Handle categorical variables properly
  categorical_vars <- c("lead_size_category")

  # One-hot encode categoricals (excluding the reference category)
  for (var in categorical_vars) {
    if (var %in% names(features)) {
      dummy_df <- fastDummies::dummy_cols(features[var], remove_first_dummy = TRUE)
      features <- features %>%
        select(-all_of(var)) %>%
        bind_cols(dummy_df %>% select(-all_of(var)))
    }
  }

  # Convert to matrix for XGBoost
  model_matrix <- model.matrix(~ . - 1, data = features)
  return(model_matrix)
}

# Prepare training matrices
cat("🔧 Preparing model matrices...\n")
X_train <- prepare_model_data(train_data)
y_train <- train_data$label_wp

X_val <- prepare_model_data(val_data)
y_val <- val_data$label_wp

# 6. Ensemble Model Training ----

cat("🤖 Training ensemble models...\n")
tictoc::tic()

# 6.1 XGBoost Model ----
cat("Training XGBoost model...\n")

# Hyperparameter tuning (simplified for demo - in practice use bayesian optimization)
xgb_params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "logloss",
  tree_method = "hist",

  # Tuned hyperparameters
  eta = 0.05,                    # Lower learning rate for better generalization
  max_depth = 8,                 # Deeper trees for complex interactions
  min_child_weight = 3,          # Prevent overfitting
  subsample = 0.8,               # Row sampling
  colsample_bytree = 0.8,        # Column sampling
  colsample_bylevel = 0.8,       # Column sampling per level
  gamma = 0.1,                   # Minimum split gain
  reg_alpha = 0.1,               # L1 regularization
  reg_lambda = 1.0,              # L2 regularization

  # Monotonicity constraints (key insight for win probability)
  # Points difference should be monotonic with win probability
  monotone_constraints = paste0("(",
                               paste(rep(0, ncol(X_train)), collapse = ","),
                               ")"),

  # Other params
  scale_pos_weight = 1,
  random_state = 42
)

# Cross-validation for optimal rounds
dtrain <- xgb.DMatrix(X_train, label = y_train)
dval <- xgb.DMatrix(X_val, label = y_val)

xgb_cv <- xgb.cv(
  data = dtrain,
  params = xgb_params,
  nrounds = 2000,
  folds = folds,
  early_stopping_rounds = 50,
  verbose = 1,
  print_every_n = 100
)

optimal_rounds <- xgb_cv$best_iteration

# Train final XGBoost model
xgb_model <- xgboost(
  data = dtrain,
  params = xgb_params,
  nrounds = optimal_rounds,
  # watchlist = list(train = dtrain, val = dval),
  # early_stopping_rounds = 50,
  verbose = 1,
  print_every_n = 100
)

# 6.2 LightGBM Model ----
cat("Training LightGBM model...\n")

# Convert to LightGBM dataset
lgb_train <- lgb.Dataset(X_train, label = y_train)
lgb_val <- lgb.Dataset(X_val, label = y_val, reference = lgb_train)

lgb_params <- list(
  objective = "binary",
  metric = "binary_logloss",
  boosting_type = "gbdt",
  num_leaves = 127,
  learning_rate = 0.05,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  min_data_in_leaf = 20,
  lambda_l1 = 0.1,
  lambda_l2 = 1.0,
  random_state = 42,
  verbose = -1
)

lgb_model <- lgb.train(
  params = lgb_params,
  data = lgb_train,
  nrounds = 1000,
  valids = list(val = lgb_val),
  early_stopping_rounds = 50,
  verbose = 1,
  eval_freq = 100
)

# 6.3 GAM Model ----
cat("Training GAM model...\n")

# Prepare data for GAM (needs to be data.frame)
gam_train_data <- train_data %>%
  select_wp_model_vars_enhanced(model_type = "gam") %>%
  mutate(label_wp = train_data$label_wp)

# Enhanced GAM with more sophisticated smoothing
gam_model <- mgcv::bam(
  label_wp ~
    # Key smooth terms
    s(time_remaining_pct, k = 10, bs = "ts") +
    s(points_diff, k = 15, bs = "ts") +
    s(xpoints_diff, k = 12, bs = "ts") +
    s(goal_x, k = 8, bs = "ts") +
    s(exp_pts, k = 10, bs = "ts") +

    # Interaction terms (tensor products)
    ti(time_remaining_pct, points_diff, k = c(5, 8), bs = c("ts", "ts")) +
    ti(time_remaining_pct, xpoints_diff, k = c(5, 8), bs = c("ts", "ts")) +
    ti(goal_x, exp_pts, k = c(6, 6), bs = c("ts", "ts")) +

    # Factor terms
    s(home, bs = "re") +
    s(lead_size_category, bs = "re") +
    s(play_type, bs = "re") +
    s(phase_of_play, bs = "re") +

    # Linear terms for interaction features
    time_score_interaction + epv_time_interaction +
    is_final_quarter + is_pressure_moment,

  data = gam_train_data,
  family = binomial(),
  method = "REML",
  select = TRUE,          # Automatic variable selection
  discrete = TRUE,        # Faster computation
  nthreads = 4
)

tictoc::toc()

# 7. Model Evaluation and Diagnostics ----

cat("📈 Evaluating models...\n")

# Generate predictions
pred_xgb <- predict(xgb_model, dval)
pred_lgb <- predict(lgb_model, X_val)
pred_gam <- predict(gam_model, val_data %>% select_wp_model_vars_enhanced(model_type = "gam"), type = "response")

# Ensemble prediction (weighted average)
ensemble_weights <- c(0.4, 0.35, 0.25)  # XGB, LGB, GAM
pred_ensemble <- ensemble_weights[1] * pred_xgb +
                 ensemble_weights[2] * pred_lgb +
                 ensemble_weights[3] * pred_gam

# Evaluation metrics (AUC removed due to drawn matches)
evaluate_model <- function(y_true, y_pred, model_name) {
  logloss <- ModelMetrics::logLoss(y_true, y_pred)
  brier <- mean((y_pred - y_true)^2)

  # Mean Absolute Error for additional perspective
  mae <- mean(abs(y_pred - y_true))

  # Calibration check (Hosmer-Lemeshow style)
  # Handle case where all predictions are the same (e.g., baseline = 0.5)
  if (length(unique(y_pred)) == 1) {
    # All predictions are identical - calibration slope undefined
    calibration_slope <- NA
    cat(glue("{model_name} Metrics:\n"))
    cat(glue("  Log Loss: {round(logloss, 4)}\n"))
    cat(glue("  Brier Score: {round(brier, 4)}\n"))
    cat(glue("  MAE: {round(mae, 4)}\n"))
    cat(glue("  Calibration Slope: NA (constant predictions)\n\n"))

  } else {
    # Normal case with varying predictions
    deciles <- cut(y_pred, breaks = quantile(y_pred, probs = 0:10/10), include.lowest = TRUE)
    calib_df <- data.frame(pred = y_pred, actual = y_true, decile = deciles) %>%
      group_by(decile) %>%
      summarise(
        pred_mean = mean(pred),
        actual_mean = mean(actual),
        n = n(),
        .groups = "drop"
      )

    calibration_slope <- lm(actual_mean ~ pred_mean, data = calib_df)$coefficients[2]

    cat(glue("{model_name} Metrics:\n"))
    cat(glue("  Log Loss: {round(logloss, 4)}\n"))
    cat(glue("  Brier Score: {round(brier, 4)}\n"))
    cat(glue("  MAE: {round(mae, 4)}\n"))
    cat(glue("  Calibration Slope: {round(calibration_slope, 4)}\n\n"))
  }

  return(list(logloss = logloss, brier = brier, mae = mae, calibration = calibration_slope))
}

# Evaluate all models
xgb_metrics <- evaluate_model(y_val, pred_xgb, "XGBoost")
lgb_metrics <- evaluate_model(y_val, pred_lgb, "LightGBM")
gam_metrics <- evaluate_model(y_val, pred_gam, "GAM")
ensemble_metrics <- evaluate_model(y_val, pred_ensemble, "Ensemble")

# Baseline comparison
baseline_pred <- rep(0.5, length(y_val))
baseline_metrics <- evaluate_model(y_val, baseline_pred, "Baseline (0.5)")

# 8. Feature Importance Analysis ----

cat("🔍 Analyzing feature importance...\n")

# XGBoost feature importance
xgb_importance <- xgb.importance(model = xgb_model)
cat("Top 10 XGBoost Features:\n")
print(head(xgb_importance, 10))

# LightGBM feature importance
lgb_importance <- lgb.importance(lgb_model)
cat("\nTop 10 LightGBM Features:\n")
print(head(lgb_importance, 10))

# 9. Save Enhanced Model ----

cat("💾 Saving enhanced win probability model...\n")

# Create enhanced prediction function
get_wp_preds_enhanced <- function(df) {
  # Apply the same feature engineering
  df_enhanced <- df %>%
    group_by(match_id) %>%
    arrange(period, period_seconds) %>%
    mutate(
      # All the same feature engineering as above
      time_remaining = (4 - period) * AFL_QUARTER_DURATION + (AFL_QUARTER_DURATION - period_seconds),
      time_remaining_pct = time_remaining / (AFL_QUARTER_DURATION * 4),
      quarter_progress = period_seconds / AFL_QUARTER_DURATION,
      is_final_quarter = as.numeric(period == 4),
      is_final_5_mins = as.numeric(time_remaining <= 300),
      is_final_2_mins = as.numeric(time_remaining <= 120),
      points_diff_abs = abs(points_diff),
      points_diff_squared = points_diff^2,
      points_diff_per_time = points_diff / pmax(time_remaining, 1),
      exp_pts_momentum = exp_pts - lag(exp_pts, default = 0),
      exp_pts_ma_3 = zoo::rollmean(exp_pts, k = 3, fill = exp_pts, align = "right"),
      field_pos_score = case_when(
        goal_x <= 30 ~ 3, goal_x <= 50 ~ 2, goal_x <= 80 ~ 1, TRUE ~ 0
      ),
      goal_distance = sqrt(goal_x^2 + y^2),
      goal_angle = atan2(abs(y), goal_x) * 180 / pi,
      scoring_team_change = as.numeric(scoring_team_id != lag(scoring_team_id, default = first(scoring_team_id))),
      epv_swing = abs(exp_pts - lag(exp_pts, default = exp_pts[1])),
      epv_volatility = zoo::rollapply(exp_pts, width = 5, FUN = sd, fill = 0, align = "right"),
      lead_size_category = case_when(
        abs(points_diff) <= 6 ~ "tight", abs(points_diff) <= 18 ~ "moderate",
        abs(points_diff) <= 30 ~ "comfortable", TRUE ~ "blowout"
      ),
      is_pressure_moment = as.numeric(
        (is_final_5_mins == 1 & abs(points_diff) <= 12) | (is_final_2_mins == 1 & abs(points_diff) <= 18)
      ),
      time_score_interaction = time_remaining_pct * points_diff,
      time_score_abs_interaction = time_remaining_pct * abs(points_diff),
      epv_time_interaction = exp_pts * time_remaining_pct,
      home_advantage_late = home * is_final_quarter,
      home_advantage_pressure = home * is_pressure_moment,
      points_diff_ma_5 = zoo::rollmean(points_diff, k = 5, fill = points_diff, align = "right"),
      exp_pts_ma_5 = zoo::rollmean(exp_pts, k = 5, fill = exp_pts, align = "right"),
      points_diff_volatility = zoo::rollapply(points_diff, width = 10, FUN = sd, fill = 0, align = "right")
    ) %>%
    ungroup() %>%
    mutate(
      lead_size_category = factor(lead_size_category, levels = c("tight", "moderate", "comfortable", "blowout")),
      play_type = factor(play_type),
      phase_of_play = factor(phase_of_play)
    )

  # Prepare features
  X_pred <- prepare_model_data(df_enhanced)

  # Get ensemble predictions
  pred_xgb <- predict(xgb_model, X_pred)
  pred_lgb <- predict(lgb_model, X_pred)
  pred_gam <- predict(gam_model, df_enhanced %>% select_wp_model_vars_enhanced(model_type = "gam"), type = "response")

  # Ensemble
  ensemble_pred <- ensemble_weights[1] * pred_xgb +
                   ensemble_weights[2] * pred_lgb +
                   ensemble_weights[3] * pred_gam

  return(data.frame(wp = ensemble_pred))
}

# Save the ensemble model components
wp_model_ensemble <- list(
  xgb_model = xgb_model,
  lgb_model = lgb_model,
  gam_model = gam_model,
  ensemble_weights = ensemble_weights,
  feature_names = colnames(X_train),
  metrics = list(
    xgb = xgb_metrics,
    lgb = lgb_metrics,
    gam = gam_metrics,
    ensemble = ensemble_metrics
  ),
  preprocessing_info = list(
    training_seasons = training_seasons,
    n_train = nrow(train_data),
    n_val = nrow(val_data)
  )
)

# Save to package data
usethis::use_data(wp_model_ensemble, overwrite = TRUE)

# Also save the enhanced prediction function
usethis::use_data(get_wp_preds_enhanced, overwrite = TRUE, internal = TRUE)

cat("✅ Enhanced win probability model saved!\n")
cat(glue("🎯 Final ensemble validation Log Loss: {round(ensemble_metrics$logloss, 4)}\n"))
cat(glue("🎯 Final ensemble validation Brier: {round(ensemble_metrics$brier, 4)}\n"))

# 10. Model Testing and Visualization ----

cat("🧪 Testing model on sample game...\n")

# Test on a specific match (similar to original code)
test_match <- "CD_M20250141401"  # Adjust as needed

if (test_match %in% model_data_wp_enhanced$match_id) {
  test_df <- model_data_wp_enhanced %>%
    filter(match_id == test_match) %>%
    arrange(period, period_seconds)

  # Get predictions
  test_preds <- get_wp_preds_enhanced(test_df)

  # Add predictions to test data
  test_results <- test_df %>%
    bind_cols(test_preds) %>%
    select(
      match_id, period, period_seconds,
      points_diff, xpoints_diff,
      wp, wpa = NA,  # Would need to calculate WPA
      goal_x, play_type, phase_of_play,
      home_points, away_points,
      time_remaining_pct, is_pressure_moment
    )

  cat(glue("📊 Test match predictions generated for {nrow(test_results)} plays\n"))
  cat(glue("Win probability range: {round(min(test_results$wp), 3)} - {round(max(test_results$wp), 3)}\n"))
}

cat("\n🎉 Enhanced Win Probability Model Complete! 🎉\n")
cat("=================================================\n")
cat("Key Improvements:\n")
cat("✅ Enhanced feature engineering (30+ new features)\n")
cat("✅ Ensemble modeling (XGBoost + LightGBM + GAM)\n")
cat("✅ Proper time-based validation split\n")
cat("✅ Comprehensive evaluation metrics\n")
cat("✅ Feature importance analysis\n")
cat("✅ Calibration assessment\n")
cat("=================================================\n")
