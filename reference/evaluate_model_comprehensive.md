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
  compute_ci = TRUE,
  bootstrap_ci,
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

- compute_ci:

  Logical, whether to compute approximate confidence intervals using
  normal approximation.

- bootstrap_ci:

  Deprecated alias for `compute_ci`; retained for backwards
  compatibility. If provided, overrides `compute_ci`.

- n_bootstrap:

  Deprecated; retained for backwards compatibility but unused.

## Value

List containing evaluation metrics with confidence intervals
