# Get season total ratings

Convenience wrapper around
[`load_player_season_ratings()`](https://peteowen1.github.io/torp/reference/load_player_season_ratings.md)
with filtering.

## Usage

``` r
player_season_ratings(season_val = get_afl_season(), round_num = NA)
```

## Arguments

- season_val:

  The season to calculate ratings for. Default is the current season.

- round_num:

  The round number to calculate ratings for. Default is NA (all rounds).

## Value

A data frame containing player season total ratings.
