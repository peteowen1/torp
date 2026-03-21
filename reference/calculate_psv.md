# Calculate Player Stat Value (PSV) from Per-Game Stats

Applies the same glmnet coefficients used by PSR to raw single-game
stats to produce a per-game margin contribution score. While PSR uses
Bayesian smoothed career estimates (`_rating` columns), PSV uses actual
box-score stats from a single game.

## Usage

``` r
calculate_psv(player_stats, coef_df, tog_adjust = TRUE, center = TRUE)
```

## Arguments

- player_stats:

  A data.table/data.frame of per-game player data with raw stat columns
  (e.g. `goals`, `kicks`, `disposals`) and a `tog` column
  (time-on-ground as a fraction 0-1).

- coef_df:

  A data.frame with columns `stat_name` and `beta` (same format as for
  [`calculate_psr()`](https://peteowen1.github.io/torp/reference/calculate_psr.md)).
  If an `sd` column is present, raw rates are divided by SD before
  applying betas.

- tog_adjust:

  Logical. If TRUE (default), divide raw counts by TOG to get
  per-full-game rates (matching the scale the coefficients were trained
  on). If FALSE, use raw counts directly.

- center:

  Logical. If TRUE (default), subtract the per-round league mean so PSV
  represents contribution above the average player that round.

## Value

A data.table with identifier columns plus `psv_raw` and `psv`.
