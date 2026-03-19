# Compute player game ratings from raw player game data

Internal function used by the pipeline to transform raw credits from
[`create_player_game_data()`](https://peteowen1.github.io/torp/reference/create_player_game_data.md)
into the display-friendly ratings format that gets released to torpdata.

## Usage

``` r
.compute_player_game_ratings(player_game_data, season_val, round_val)
```

## Arguments

- player_game_data:

  Player game data (output of
  [`create_player_game_data()`](https://peteowen1.github.io/torp/reference/create_player_game_data.md)).

- season_val:

  Season(s) to compute for.

- round_val:

  Round(s) to compute for.

## Value

A data frame in player game ratings format.
