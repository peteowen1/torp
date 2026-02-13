# Log Model Performance Metrics

This function is intended for internal use and may be unexported in a
future release. Logs model performance metrics in a structured format
for monitoring

## Usage

``` r
log_model_performance(
  model_name,
  metrics,
  data_info = NULL,
  model_version = NULL
)
```

## Arguments

- model_name:

  Name of the model

- metrics:

  Named list of performance metrics

- data_info:

  Information about the data used

- model_version:

  Version of the model
