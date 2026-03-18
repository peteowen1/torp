# Save Injury Data to GitHub Release

Saves a timestamped snapshot of injury data to the torpdata repository
as a parquet file for historical tracking.

## Usage

``` r
save_injury_data(injuries_df, season)
```

## Arguments

- injuries_df:

  A data.frame of injuries (e.g., from
  [`get_all_injuries()`](https://peteowen1.github.io/torp/reference/get_all_injuries.md)).

- season:

  Numeric season year (e.g. 2026).

## Value

Invisible NULL. Called for side effects (upload).
