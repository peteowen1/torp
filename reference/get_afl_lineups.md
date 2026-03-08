# Fetch AFL Lineups

Fetches team lineups/rosters for a season (optionally filtered by
round). Uses a single shared auth token across all per-match roster
calls.

## Usage

``` r
get_afl_lineups(season = NULL, round = NULL)
```

## Arguments

- season:

  Numeric year (default: current season via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md))

- round:

  Optional round number to filter to. If NULL, fetches all rounds.

## Value

A tibble of player lineup data

## Examples

``` r
if (FALSE) { # \dontrun{
lineups <- get_afl_lineups(2025, round = 1)
} # }
```
