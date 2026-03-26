# Get Season Games

Retrieves game data for an entire season using parallel HTTP requests.

## Usage

``` r
get_season_games(season, rounds = 28)
```

## Arguments

- season:

  The AFL season year (numeric).

- rounds:

  The maximum number of rounds to check (default: 28, covers all AFL
  season formats).

## Value

A dataframe containing game data for the entire season.
