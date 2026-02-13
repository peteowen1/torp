# Comprehensive Baseline Model Comparison

Compares multiple baseline models against the main model

## Usage

``` r
compare_baseline_models(
  train_data,
  test_data,
  main_model_preds,
  include_gam = FALSE
)
```

## Arguments

- train_data:

  Training dataset

- test_data:

  Test dataset

- main_model_preds:

  Predictions from the main model

- include_gam:

  Logical, whether to include GAM baseline (can be slow)

## Value

Dataframe with comparison results
