# Prepare final dataframe

Prepare final dataframe

## Usage

``` r
prepare_final_dataframe(
  plyr_tm_df = NULL,
  player_game_data = NULL,
  season_val,
  round_val
)
```

## Arguments

- plyr_tm_df:

  Player team database

- player_game_data:

  Player statistics dataframe (aggregated from calculate_player_stats)

- season_val:

  Season value

- round_val:

  Round value

## Value

A final dataframe with player ratings
