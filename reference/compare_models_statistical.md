# Compare Multiple Models Statistically

This function is intended for internal use and may be unexported in a
future release. Compares multiple models using AUC difference and CI
overlap

## Usage

``` r
compare_models_statistical(model_results, test_type = "simple")
```

## Arguments

- model_results:

  List of model evaluation results

- test_type:

  Unused; retained for backwards compatibility. Comparison is always
  based on AUC confidence interval overlap.

## Value

Dataframe with pairwise comparison results
