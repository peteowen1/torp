# Estimate player stat ratings for multiple reference dates efficiently

Uses cumulative-sum factorisation to avoid redundant aggregation across
dates. Decay weight `exp(-lambda * (ref - match))` is split into
`exp(-lambda * ref) * exp(lambda * match)`. The match-side factor is
fixed per row, so we pre-multiply values by it and take per-player
cumulative sums (sorted by date). A data.table rolling join then looks
up each ref_date in O(log n), and the ref-side scalar completes the
product.

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

## Details

Complexity drops from O(dates \* stats \* rows) to O(stats \* rows) for
the cumsums plus O(stats \* players \* dates) for the lookups.
