# Get game ratings

Convenience wrapper around
[`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md)
with filtering by season, round, match, or team. Returns the same data
as the load function — both pull from the same pre-computed release.

## Usage

``` r
player_game_ratings(
  season_val = get_afl_season(),
  round_val = get_afl_week(),
  matchid = NULL,
  team = NULL,
  round_num = NULL
)
```

## Arguments

- season_val:

  The season to get ratings for. Default is the current season.

- round_val:

  The round number to get ratings for. Default is the current round.

- matchid:

  The match ID to filter by. Default is NULL (no filtering).

- team:

  The team to filter by. Default is NULL (no filtering).

- round_num:

  Deprecated. Use `round_val` instead.

## Value

A data frame containing player game ratings.
