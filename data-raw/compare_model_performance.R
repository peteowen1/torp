#' Compare mgcv and XGBoost Models
#'
#' This script assumes that both modelling scripts
#' `build_match_predictions.R` (mgcv) and
#' `build_match_predictions_xgb.R` (xgboost)
#' have been run in the current session and that `team_mdl_df`
#' contains prediction columns `pred_win` and `pred_win_xgb`.

library(tidyverse)
library(MLmetrics)

if (!all(c("pred_win", "pred_win_xgb") %in% names(team_mdl_df))) {
  stop("Required prediction columns not found in team_mdl_df")
}

test_df <- team_mdl_df %>%
  dplyr::filter(!is.na(win), team_type == "home", season.x == get_afl_season())

mgcv_logloss <- MLmetrics::LogLoss(test_df$pred_win, test_df$win)
xgb_logloss  <- MLmetrics::LogLoss(test_df$pred_win_xgb, test_df$win)

comparison <- tibble(
  model = c("mgcv", "xgboost"),
  logloss = c(mgcv_logloss, xgb_logloss)
)

print(comparison)


