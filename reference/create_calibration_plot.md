# Create Calibration Plot Data

This function is intended for internal use and may be unexported in a
future release. Creates data structure for plotting model calibration.

A simplified wrapper around assess_model_calibration for creating
calibration plot data. For more comprehensive calibration analysis, use
assess_model_calibration directly followed by prepare_calibration_plot.

## Usage

``` r
create_calibration_plot(actual, predicted, n_bins = 10)
```

## Arguments

- actual:

  Vector of actual binary outcomes

- predicted:

  Vector of predicted probabilities

- n_bins:

  Number of bins for calibration plot

## Value

List with plot data and statistics

## See also

[`assess_model_calibration`](https://peteowen1.github.io/torp/reference/assess_model_calibration.md),
[`prepare_calibration_plot`](https://peteowen1.github.io/torp/reference/prepare_calibration_plot.md)
