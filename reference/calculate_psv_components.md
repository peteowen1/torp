# Calculate PSV with Offensive/Defensive Decomposition

Applies offensive and defensive coefficient models to per-game stats,
producing `psv`, `osv`, and `dsv` columns where `osv + dsv = psv`.

## Usage

``` r
calculate_psv_components(
  player_stats,
  coef_df,
  osr_coef_df,
  dsr_coef_df,
  tog_adjust = TRUE,
  center = TRUE
)
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

- osr_coef_df:

  Coefficient data.frame for the offensive model.

- dsr_coef_df:

  Coefficient data.frame for the defensive model.

- tog_adjust:

  Logical. If TRUE (default), divide raw counts by TOG to get
  per-full-game rates (matching the scale the coefficients were trained
  on). If FALSE, use raw counts directly.

- center:

  Logical. If TRUE (default), subtract the per-round league mean so PSV
  represents contribution above the average player that round.

## Value

A data.table with identifier columns plus `psv_raw`, `psv`, `osv`,
`dsv`.
