# Estimate player stat ratings for multiple reference dates efficiently

Internal function that eliminates redundant work when estimating stat
ratings across many dates: date conversion and avail_only setup are done
once, and the data is sorted so each iteration filters an ascending
subset.

## Usage

``` r
.estimate_stat_ratings_batch(
  stat_rating_data,
  ref_dates,
  params = NULL,
  stat_defs = NULL,
  compute_ci = FALSE
)
```

## Arguments

- stat_rating_data:

  A data.table from
  [`.prepare_stat_rating_data()`](https://peteowen1.github.io/torp/reference/dot-prepare_stat_rating_data.md).

- ref_dates:

  Date vector of reference dates.

- params:

  Named list of hyperparameters from
  [`default_stat_rating_params()`](https://peteowen1.github.io/torp/reference/default_stat_rating_params.md).

- stat_defs:

  Output of
  [`stat_rating_definitions()`](https://peteowen1.github.io/torp/reference/stat_rating_definitions.md).
  If NULL, uses default.

- compute_ci:

  Logical. If FALSE (default), skip credible interval computation.

## Value

Named list of data.tables (keyed by ref_date as character), one per
date.
