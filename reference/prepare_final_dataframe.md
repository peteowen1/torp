# Prepare final dataframe

Prepare final dataframe

## Usage

``` r
prepare_final_dataframe(
  plyr_tm_df = NULL,
  player_game_data = NULL,
  season_val,
  round_val,
  fixtures = NULL,
  fix_summary = NULL
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

- fixtures:

  Optional pre-loaded fixtures data. If NULL, will load automatically.

- fix_summary:

  Optional pre-computed fixtures summary (season, round, ref_date). If
  NULL, computed from `fixtures` each call. Pass this when calling in a
  loop to avoid redundant summarisation.

## Value

A final dataframe with player ratings
