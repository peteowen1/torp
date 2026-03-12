# Train the 5-model sequential XGBoost pipeline

Mirrors the GAM pipeline structure: total xPoints -\> xScore diff -\>
conv diff -\> score diff -\> win probability, each step feeding the
next.

## Usage

``` r
.train_match_xgb(team_mdl_df, train_filter = NULL)
```

## Arguments

- team_mdl_df:

  Complete model dataset (with GAM predictions already added)

- train_filter:

  Logical vector indicating training rows (NULL = all completed matches)

## Value

List with \$models (named list of 5 XGBoost models) and \$data
(team_mdl_df with xgb_pred_score_diff and xgb_pred_win columns added)
