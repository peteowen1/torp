# Time-Based Train/Validation/Test Split

This function is intended for internal use and may be unexported in a
future release. Creates proper temporal splits for time series data to
prevent data leakage

## Usage

``` r
create_temporal_splits(data, train_seasons, val_seasons, test_seasons)
```

## Arguments

- data:

  A dataframe containing model data with date/season information

- train_seasons:

  Vector of seasons for training

- val_seasons:

  Vector of seasons for validation

- test_seasons:

  Vector of seasons for testing

## Value

List containing train, validation, and test datasets
