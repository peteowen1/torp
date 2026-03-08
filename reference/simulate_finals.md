# Simulate AFL finals series

Implements the full AFL top-8 finals bracket: qualifying finals,
elimination finals, semi-finals, preliminary finals, and grand final.

## Usage

``` r
simulate_finals(ladder_dt, sim_teams_dt)
```

## Arguments

- ladder_dt:

  A data.table from
  [`calculate_ladder()`](https://peteowen1.github.io/torp/reference/calculate_ladder.md)
  with `team` and `rank` columns.

- sim_teams_dt:

  A data.table with `team` and `torp` (hot ratings after regular
  season).

## Value

A data.table with columns: `team`, `finals_finish` (week eliminated or 5
for premier), `finals_wins`, `made_gf`, `won_gf`.
