# Add Expected Points Value (EPV) Variables

This function adds EPV-related variables to the input dataframe.

## Usage

``` r
add_epv_vars(df, reference_date = Sys.Date())
```

## Arguments

- df:

  A dataframe containing play-by-play data.

- reference_date:

  Date used for computing game recency weights. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html). Set explicitly
  for reproducible historical analysis.

## Value

A dataframe with additional EPV-related variables.
