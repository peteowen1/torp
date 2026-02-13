# Clean Play-by-Play Data (data.table optimized)

Optimized version of clean_pbp using data.table for better performance.
Consolidates all variable additions into fewer passes over the data.

## Usage

``` r
clean_pbp_dt(df)
```

## Arguments

- df:

  A dataframe containing raw play-by-play data.

## Value

A cleaned and processed data.table with additional variables.
