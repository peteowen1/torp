# Comprehensive Model Evaluation

This function is intended for internal use and may be unexported in a
future release. Evaluates model performance with multiple metrics and
statistical rigor

## Usage

``` r
evaluate_model_comprehensive(
  actual,
  predicted,
  model_name = "Model",
  bootstrap_ci = TRUE,
  n_bootstrap = 1000
)
```

## Arguments

- actual:

  Vector of actual outcomes

- predicted:

  Vector of predicted probabilities

- model_name:

  Name of the model being evaluated

- bootstrap_ci:

  Logical, whether to compute bootstrap confidence intervals

- n_bootstrap:

  Number of bootstrap samples (default: 1000)

## Value

List containing evaluation metrics with confidence intervals
