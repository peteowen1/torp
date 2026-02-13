# Log Prediction Event

Logs individual prediction events for debugging and monitoring

## Usage

``` r
log_prediction_event(
  model_name,
  input_hash,
  n_predictions,
  summary = NULL,
  ...
)
```

## Arguments

- model_name:

  Name of the model making predictions

- input_hash:

  Hash of input data for tracking

- n_predictions:

  Number of predictions made

- summary:

  Optional prediction summary statistics

- ...:

  Additional parameters (ignored)
