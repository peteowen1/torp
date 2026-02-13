# Validate Data Freshness

This function is intended for internal use and may be unexported in a
future release. Checks if data is recent enough for reliable predictions

## Usage

``` r
validate_data_freshness(
  data_timestamp,
  timestamp_col = "utc_start_time",
  max_age_days = NULL,
  max_age_hours = 24
)
```

## Arguments

- data_timestamp:

  Timestamp of the most recent data (can be dataframe with timestamp
  column or direct timestamp)

- timestamp_col:

  Name of timestamp column if data_timestamp is a dataframe

- max_age_days:

  Maximum acceptable age in days (default: 7, overrides hours if
  provided)

- max_age_hours:

  Maximum acceptable age in hours (default: 24, used if max_age_days not
  provided)

## Value

Logical indicating if data is fresh enough
