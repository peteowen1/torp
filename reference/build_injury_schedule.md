# Build Injury Return Schedule for Simulation

Takes an injury data frame (with `return_round` already parsed) and
player-level TORP ratings, and computes per-team TORP boosts at each
round when injured players return.

## Usage

``` r
build_injury_schedule(injuries_df, player_ratings_dt)
```

## Arguments

- injuries_df:

  A data.frame with at least `player_norm` and `return_round` columns.
  Typically from
  [`get_all_injuries()`](https://peteowen1.github.io/torp/reference/get_all_injuries.md)
  after calling
  [`parse_return_round()`](https://peteowen1.github.io/torp/reference/parse_return_round.md).

- player_ratings_dt:

  A data.table of player ratings with `player_name`, `team`, `torp`, and
  optionally `pred_tog` columns.

## Value

A data.table with columns `team`, `torp_boost`, `return_round`. One row
per (team, return_round) combination.
