# Compare Multiple Models Statistically

This function is intended for internal use and may be unexported in a
future release. Performs statistical comparison between multiple models
using paired tests

## Usage

``` r
compare_models_statistical(model_results, test_type = "simple")
```

## Arguments

- model_results:

  List of model evaluation results

- test_type:

  Type of statistical test ("mcnemar", "delong")

## Value

Dataframe with pairwise comparison results
