#' Build Match Predictions with XGBoost
#'
#' This script provides an alternative modelling approach using XGBoost.
#' It assumes that the data preparation steps from `build_match_predictions.R`
#' have been executed and that `team_mdl_df` exists in the environment.
#'
#' The workflow performs hyper parameter tuning with cross validation and then
#' fits a final model on the training data.  Predictions are stored in the
#' `pred_win_xgb` column of `team_mdl_df`.

library(tidyverse)
library(caret)
library(xgboost)
library(MLmetrics)

devtools::load_all()

# ---------------------------------------------------------------------------
# Check that the modelling data exists
if (!exists("team_mdl_df")) {
  stop("`team_mdl_df` not found. Run build_match_predictions.R first.")
}

# Prepare clean modeling dataset
model_df <- team_mdl_df %>%
  dplyr::filter(!is.na(win)) %>%
  dplyr::mutate(win = as.numeric(win)) %>%
  select_afl_model_vars() %>%  # Select model variables first
  tidyr::drop_na()  # Remove any rows with missing values

# Create train/test split
set.seed(1234)
# train_idx <- caret::createDataPartition(model_df$win, p = 0.8, list = FALSE)
train_idx <- which(model_df$season.x < get_afl_season())
train_df  <- model_df[train_idx, ]
test_df   <- model_df[-train_idx, ]

# Store providerId and team_type before creating matrices (if needed for later joining)
if ("providerId" %in% names(team_mdl_df)) {
  # Create a clean dataset with IDs
  clean_with_ids <- team_mdl_df %>%
    dplyr::filter(!is.na(win)) %>%
    dplyr::mutate(win = as.numeric(win)) %>%
    dplyr::select(providerId, team_type, everything()) %>%
    dplyr::select(providerId, team_type, all_of(names(select_afl_model_vars(.)))) %>%
    tidyr::drop_na()

  # Extract just the model variables for matrices
  model_df <- clean_with_ids %>% dplyr::select(-providerId, -team_type)

  # Store the IDs separately
  id_df <- clean_with_ids %>% dplyr::select(providerId, team_type)
}

# Create model matrices - exclude response variable and any ID columns
feature_cols <- setdiff(names(train_df), c("win", "providerId", "team_type"))

train_matrix <- model.matrix(~ . - 1, data = train_df[, feature_cols])
train_label  <- train_df$win

test_matrix  <- model.matrix(~ . - 1, data = test_df[, feature_cols])
test_label   <- test_df$win

# Verify dimensions match
cat("Train matrix rows:", nrow(train_matrix), "Train label length:", length(train_label), "\n")
cat("Test matrix rows:", nrow(test_matrix), "Test label length:", length(test_label), "\n")

# Create XGBoost matrices
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest  <- xgb.DMatrix(data = test_matrix, label = test_label)

# Hyperparameter tuning grid
gr <- expand.grid(
  eta = c(0.05, 0.1, 0.2),
  max_depth = c(4, 6, 8),
  subsample = c(0.8, 1),
  colsample_bytree = c(0.8, 1),
  min_child_weight = c(1, 5),
  gamma = c(0, 1)
)

# Cross-validation for hyperparameter tuning
cv_results <- purrr::pmap(gr, function(eta, max_depth, subsample,
                                       colsample_bytree, min_child_weight, gamma) {
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    gamma = gamma
  )

  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,
    nfold = 5,
    verbose = 0,
    early_stopping_rounds = 50
  )

  tibble(
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    gamma = gamma,
    best_logloss = min(cv$evaluation_log$test_logloss_mean),
    best_nrounds = cv$best_iteration
  )
},
.progress = T
) %>% bind_rows()

# Select best parameters
best <- cv_results %>% dplyr::arrange(best_logloss) %>% dplyr::slice(1)
cat("Best CV LogLoss:", round(best$best_logloss, 4), "\n")

best_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = best$eta,
  max_depth = best$max_depth,
  subsample = best$subsample,
  colsample_bytree = best$colsample_bytree,
  min_child_weight = best$min_child_weight,
  gamma = best$gamma
)

# Train final model
final_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best$best_nrounds,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0
)

# Evaluate on test data
test_pred <- predict(final_model, dtest)
logloss <- MLmetrics::LogLoss(test_pred, test_label)
print(glue::glue("Test LogLoss: {round(logloss, 4)}"))

# Feature importance
importance <- xgb.importance(model = final_model)
print("Top 10 Most Important Features:")
print(head(importance, 10))

# Add predictions back to full data
# Create matrix for the full cleaned dataset
full_matrix <- model.matrix(~ . - 1, data = model_df[, feature_cols])
dfull <- xgb.DMatrix(data = full_matrix)
model_df$pred_win_xgb <- predict(final_model, dfull)

# Approach 1: Remove column if it exists before joining
if ("providerId" %in% names(team_mdl_df)) {
  # Create a dataframe with IDs and predictions
  pred_df <- id_df %>%
    dplyr::mutate(pred_win_xgb = model_df$pred_win_xgb)

  # Remove the column if it already exists
  team_mdl_df <- team_mdl_df %>%
    dplyr::select(-any_of("pred_win_xgb")) %>%  # Remove if exists, ignore if doesn't
    dplyr::left_join(pred_df, by = c("providerId", "team_type"))

} else {
  team_mdl_df$pred_win_xgb <- NA
  warning("No providerId found - predictions may not align correctly with original data")
}

# Store the fitted model for later use
xgb_win_model <- final_model
usethis::use_data(xgb_win_model, overwrite = TRUE)

# Print summary
cat("\nModel Summary:\n")
cat("Training samples:", nrow(train_df), "\n")
cat("Test samples:", nrow(test_df), "\n")
cat("Features:", ncol(train_matrix), "\n")
cat("Test LogLoss:", round(logloss, 4), "\n")
