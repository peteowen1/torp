# Select Enhanced Win Probability Model Variables

Selects the appropriate variables for enhanced win probability modeling.
Different models handle different variable types - GAM can use factors
directly, while XGBoost/LightGBM need one-hot encoded variables.

## Usage

``` r
select_wp_model_vars_enhanced(df, model_type = "ensemble")
```

## Arguments

- df:

  A dataframe with enhanced features

- model_type:

  Type of model ("gam", "xgboost", "lightgbm", "ensemble")

## Value

A dataframe with selected model variables
