# Update Player Stats Release

Scrapes player stats from the AFL API for a given season, normalises
column names, and uploads to the torpdata GitHub release.

## Usage

``` r
update_player_stats(season)
```

## Arguments

- season:

  Season year (numeric).

## Value

Invisible NULL. Called for side effects (upload).
