# Clean Model Data for EPV (data.table optimized)

Optimized version using data.table shift() with by-reference helpers.
Follows the same pattern as clean_pbp() -\> clean_pbp_dt().

## Usage

``` r
clean_model_data_epv_dt(df)
```

## Arguments

- df:

  A dataframe containing cleaned play-by-play data from clean_pbp().

## Value

A data.table ready for EPV modeling.
