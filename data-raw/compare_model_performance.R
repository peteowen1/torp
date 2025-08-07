#' Compare mgcv and XGBoost Models
#'
#' This script assumes that both modelling scripts
#' `build_match_predictions.R` (mgcv) and
#' `build_match_predictions_xgb.R` (xgboost)
#' have been run in the current session and that `team_mdl_df`
#' contains prediction columns `pred_win` and `pred_win_xgb`.

library(tidyverse)
library(MLmetrics)
#
# if (!all(c("pred_win", "pred_win_xgb") %in% names(team_mdl_df))) {
#   stop("Required prediction columns not found in team_mdl_df")
# }

test_df <- team_mdl_df %>%
  dplyr::filter(
    !is.na(win),
    team_type == "home",
    season.x == get_afl_season()
    #fix these later
    ,!is.na(pred_win_xgb)
    ,!is.na(pred_win)
    )

mgcv_logloss <- MLmetrics::LogLoss(test_df$pred_win, test_df$win)
xgb_logloss  <- MLmetrics::LogLoss(test_df$pred_win_xgb, test_df$win)

comparison <- tibble(
  model = c("mgcv", "xgboost"),
  logloss = c(mgcv_logloss, xgb_logloss)
)

print(comparison)

####
test_df$bits <- ifelse(test_df$win == 1,
                       1 + log2(test_df$pred_win),
                       ifelse(test_df$win == 0,
                              1 + log2(1 - test_df$pred_win),
                              1 + 0.5 * log2(test_df$pred_win * (1 - test_df$pred_win))
                       )
)
test_df$tips <- ifelse(round(test_df$pred_win) == test_df$win, 1,
                       ifelse(test_df$win == 0.5, 1, 0)
)
test_df$mae <- abs(test_df$score_diff - test_df$pred_score_diff)

# library(MLmetrics)
MLmetrics::LogLoss(test_df$pred_win, test_df$win)
MLmetrics::MAE(test_df$pred_score_diff, test_df$score_diff)
sum(test_df$bits)
sum(test_df$tips)
mean(test_df$bits)
mean(test_df$tips)
nrow(test_df)


####
test_df$bits <- ifelse(test_df$win == 1,
                       1 + log2(test_df$pred_win_xgb),
                       ifelse(test_df$win == 0,
                              1 + log2(1 - test_df$pred_win_xgb),
                              1 + 0.5 * log2(test_df$pred_win_xgb * (1 - test_df$pred_win_xgb))
                       )
)
test_df$tips <- ifelse(round(test_df$pred_win_xgb) == test_df$win, 1,
                       ifelse(test_df$win == 0.5, 1, 0)
)

# library(MLmetrics)
MLmetrics::LogLoss(test_df$pred_win_xgb, test_df$win)
sum(test_df$bits)
sum(test_df$tips)
mean(test_df$bits)
mean(test_df$tips)
nrow(test_df)
