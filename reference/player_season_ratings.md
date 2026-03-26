# Get season total ratings

Convenience wrapper around
[`load_player_season_ratings()`](https://peteowen1.github.io/torp/reference/load_player_season_ratings.md)
with filtering.

## Usage

``` r
player_season_ratings(
  season_val = get_afl_season(),
  per80 = FALSE,
  round_num = NULL
)
```

## Arguments

- season_val:

  The season to calculate ratings for. Default is the current season.

- per80:

  Logical. If `TRUE`, return per-80-minute averages instead of season
  totals. Default `FALSE` (totals).

- round_num:

  Deprecated and ignored. Retained for backwards compatibility.

## Value

A data frame containing player season ratings.
