# Create Model Comparison Report

This function is intended for internal use and may be unexported in a
future release. Generates a comprehensive report comparing models
including baselines

## Usage

``` r
create_model_comparison_report(comparison_results, calibration_results = NULL)
```

## Arguments

- comparison_results:

  Results from compare_baseline_models

- calibration_results:

  Optional calibration assessment results

## Value

Character string with formatted report
