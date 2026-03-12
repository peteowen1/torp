# Compute player season ratings from player game ratings

Internal function used by the pipeline to aggregate per-game ratings
into season totals for release to torpdata.

## Usage

``` r
.compute_player_season_ratings(player_game_ratings_df)
```

## Arguments

- player_game_ratings_df:

  Player game ratings data frame (output of
  [`.compute_player_game_ratings()`](https://peteowen1.github.io/torp/reference/dot-compute_player_game_ratings.md)).

## Value

A data frame with season-total ratings.
