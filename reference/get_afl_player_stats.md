# Fetch AFL Player Stats

Fetches per-player per-match stats for a season. Uses cached fixtures
for match IDs and a single shared auth token.

## Usage

``` r
get_afl_player_stats(season = NULL)
```

## Arguments

- season:

  Numeric year (default: current season via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md))

## Value

A tibble of player match stats

## Examples

``` r
if (FALSE) { # \dontrun{
stats <- get_afl_player_stats(2025)
} # }
```
