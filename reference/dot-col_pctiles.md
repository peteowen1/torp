# Compute percentile rank of a player's values within a reference data.table

Compute percentile rank of a player's values within a reference
data.table

## Usage

``` r
.col_pctiles(dt, cols, player_row, higher_is_better = NULL)
```

## Arguments

- dt:

  Reference data.table for comparison.

- cols:

  Column names to compute percentiles for.

- player_row:

  Single-row data.table of the target player.

- higher_is_better:

  Optional logical vector (same length as cols). If FALSE for a stat,
  percentile is flipped (lower = better).
