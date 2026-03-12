# Compute season W/L/D record for a team

Compute season W/L/D record for a team

## Usage

``` r
.compute_season_record(results, team_name)
```

## Arguments

- results:

  A data.table of match results.

- team_name:

  Canonical team name.

## Value

A data.table with season, wins, losses, draws, points_for,
points_against, percentage.
