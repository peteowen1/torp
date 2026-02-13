# Monitor Model Drift

This function is intended for internal use and may be unexported in a
future release. Monitors for potential model drift by comparing
performance metrics

## Usage

``` r
monitor_model_drift(
  model_name,
  current_metrics,
  baseline_metrics,
  drift_threshold = 0.05
)
```

## Arguments

- model_name:

  Name of the model to monitor

- current_metrics:

  Current performance metrics

- baseline_metrics:

  Baseline metrics for comparison

- drift_threshold:

  Threshold for triggering drift alert (default: 0.05)
