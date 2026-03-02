# Select AFL XGBoost Model Variables (Lean Feature Set)

Selects a reduced set of variables optimised for XGBoost match
prediction. Removes high-cardinality factor columns (team_name,
team_name_season, venue) that create hundreds of sparse dummies via
model.matrix(), and drops sample weights that aren't predictive
features.

## Usage

``` r
select_afl_xgb_vars(df)
```

## Arguments

- df:

  A dataframe containing AFL team model data with GAM predictions

## Value

A dataframe with ~25 numeric/low-cardinality features + response
