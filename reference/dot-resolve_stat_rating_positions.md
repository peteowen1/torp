# Resolve position groups, filling NAs with each player's modal position

Resolve position groups, filling NAs with each player's modal position

## Usage

``` r
.resolve_stat_rating_positions(dt, teams = NULL)
```

## Arguments

- dt:

  A data.table with player_id and position columns.

- teams:

  Optional teams/lineup data. Used as a final fallback for players whose
  primary position source is always NA-mapping (e.g. a player whose only
  PBP appearances were with `lineup_position = INT`). Filtered to
  on-field roles before modal aggregation.

## Value

The data.table with added `pos_group` column.
