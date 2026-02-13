# Calculate AUC using base R

Internal function to calculate Area Under the ROC Curve using the
Mann-Whitney U-statistic formula. This is O(n log n) vs O(n²) for the
naive threshold-based approach.

## Usage

``` r
calculate_auc_base(actual, predicted)
```

## Arguments

- actual:

  Vector of actual binary outcomes (0/1)

- predicted:

  Vector of predicted probabilities

## Value

Numeric AUC value
