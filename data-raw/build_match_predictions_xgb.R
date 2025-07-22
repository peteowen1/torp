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

# ---------------------------------------------------------------------------
# Check that the modelling data exists
if (!exists("team_mdl_df")) {
  stop("`team_mdl_df` not found. Run build_match_predictions.R first.")
}

model_df <- team_mdl_df %>%
  dplyr::filter(!is.na(win)) %>%
  dplyr::mutate(win = as.numeric(win))

set.seed(1234)
train_idx <- caret::createDataPartition(model_df$win, p = 0.8, list = FALSE)
train_df  <- model_df[train_idx, ]
test_df   <- model_df[-train_idx, ]

# create model matrices ------------------------------------------------------
train_matrix <- model.matrix(win ~ . , data = train_df %>% dplyr::select(-providerId))
train_label  <- train_df$win

test_matrix  <- model.matrix(win ~ . , data = test_df %>% dplyr::select(-providerId))
test_label   <- test_df$win

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)

gr <- expand.grid(
  eta = c(0.05, 0.1, 0.2),
  max_depth = c(4, 6, 8),
  subsample = c(0.8, 1),
  colsample_bytree = c(0.8, 1),
  min_child_weight = c(1, 5),
  gamma = c(0, 1)
)

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
}) %>% bind_rows()

best <- cv_results %>% arrange(best_logloss) %>% slice(1)

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

final_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best$best_nrounds
)

# evaluate on test data ------------------------------------------------------
dtest <- xgb.DMatrix(data = test_matrix)
test_pred <- predict(final_model, dtest)
logloss <- MLmetrics::LogLoss(test_pred, test_label)
print(glue::glue("Test LogLoss: {round(logloss, 4)}"))

# add predictions back to full data -----------------------------------------
full_matrix <- model.matrix(win ~ . , data = model_df %>% dplyr::select(-providerId))
dfull <- xgb.DMatrix(data = full_matrix)
model_df$pred_win_xgb <- predict(final_model, dfull)

team_mdl_df <- team_mdl_df %>%
  left_join(model_df %>% dplyr::select(providerId, team_type, pred_win_xgb),
            by = c("providerId", "team_type"))

# keep the fitted model for later use
xgb_win_model <- final_model

usethis::use_data(xgb_win_model, overwrite = TRUE)

