# Simulate AFL finals series

Implements the full AFL top-8 finals bracket: qualifying finals,
elimination finals, semi-finals, preliminary finals, and grand final.

## Usage

``` r
simulate_finals(ladder_dt, sim_teams_dt, gf_familiarity = NULL)
```

## Arguments

- ladder_dt:

  A data.table from
  [`calculate_ladder()`](https://peteowen1.github.io/torp/reference/calculate_ladder.md)
  with `team` and `rank` columns.

- sim_teams_dt:

  A data.table with `team` and `torp` (hot ratings after regular
  season).

- gf_familiarity:

  Optional data.table with `team` and `gf_familiarity` columns
  (proportion of games played at GF venue). When provided, the Grand
  Final home advantage is based on familiarity difference between teams.

## Value

A data.table with columns: `team`, `finals_finish` (week eliminated or 5
for premier), `finals_wins`, `made_gf`, `won_gf`.
