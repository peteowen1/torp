# Calculate AFL ladder from game results

Pivots game-level results to team-perspective rows and aggregates to
produce a full AFL ladder sorted by ladder points then percentage.

## Usage

``` r
calculate_ladder(games_dt)
```

## Arguments

- games_dt:

  A data.table with columns `home_team`, `away_team`, `home_score`,
  `away_score`, and `result` (home margin).

## Value

A data.table with one row per team, sorted by ladder position.
