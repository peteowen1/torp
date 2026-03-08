# Fetch AFL Player Details

Fetches player biographical/squad details for a season. Uses team IDs
from fixture data and the public squads endpoint.

## Usage

``` r
get_afl_player_details(season = NULL)
```

## Arguments

- season:

  Numeric year (default: current season via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md))

## Value

A tibble of player details with player_name, age, row_id columns

## Examples

``` r
if (FALSE) { # \dontrun{
details <- get_afl_player_details(2025)
} # }
```
