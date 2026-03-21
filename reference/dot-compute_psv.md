# Convenience wrapper to compute PSV from coefficient files

Loads the margin, offensive, and defensive coefficient CSVs from
`inst/extdata` and calls
[`calculate_psv_components`](https://peteowen1.github.io/torp/reference/calculate_psv_components.md).

## Usage

``` r
.compute_psv(
  player_stats,
  psr_coef_path = NULL,
  tog_adjust = TRUE,
  center = TRUE
)
```

## Arguments

- player_stats:

  A data.table/data.frame of per-game player data with raw stat columns
  (e.g. `goals`, `kicks`, `disposals`) and a `tog` column
  (time-on-ground as a fraction 0-1).

- psr_coef_path:

  Path to the margin PSR coefficient CSV. If NULL, searches
  `inst/extdata/psr_v2_coefficients.csv`.

- tog_adjust:

  Logical. If TRUE (default), divide raw counts by TOG to get
  per-full-game rates (matching the scale the coefficients were trained
  on). If FALSE, use raw counts directly.

- center:

  Logical. If TRUE (default), subtract the per-round league mean so PSV
  represents contribution above the average player that round.

## Value

A data.table with `psv`, `osv`, `dsv` columns.
