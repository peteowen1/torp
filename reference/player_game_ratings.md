# Get game ratings

This function retrieves game ratings for players based on specified
criteria.

## Usage

``` r
player_game_ratings(
  season_val = get_afl_season(),
  round_num = get_afl_week(),
  matchid = NULL,
  team = NULL,
  player_game_data = NULL
)
```

## Arguments

- season_val:

  The season to get ratings for. Default is the current season.

- round_num:

  The round number to get ratings for. Default is the current round.

- matchid:

  The match ID to filter by. Default is NULL (no filtering).

- team:

  The team to filter by. Default is NULL (no filtering).

- player_game_data:

  Optional pre-loaded player game data. If NULL, will load
  automatically.

## Value

A data frame containing player game ratings.
