# Get TORP ratings

This function calculates TORP (Total Overall Rating Points) for players
based on their performance.

## Usage

``` r
calculate_torp_ratings(
  season_val = get_afl_season(type = "current"),
  round_val = get_afl_week(type = "next"),
  decay = RATING_DECAY_DEFAULT_DAYS,
  loading = RATING_LOADING_DEFAULT,
  prior_games_recv = RATING_PRIOR_GAMES_RECV,
  prior_games_disp = RATING_PRIOR_GAMES_DISP,
  plyr_tm_df = NULL,
  player_game_data = NULL,
  prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
  prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
  fixtures = NULL
)

torp_ratings(
  season_val = get_afl_season(type = "current"),
  round_val = get_afl_week(type = "next"),
  decay = RATING_DECAY_DEFAULT_DAYS,
  loading = RATING_LOADING_DEFAULT,
  prior_games_recv = RATING_PRIOR_GAMES_RECV,
  prior_games_disp = RATING_PRIOR_GAMES_DISP,
  plyr_tm_df = NULL,
  player_game_data = NULL,
  prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
  prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
  fixtures = NULL
)
```

## Arguments

- season_val:

  The season to calculate ratings for. Default is the next season.

- round_val:

  The round to calculate ratings for. Default is the next round.

- decay:

  The decay factor for weighting games. Default is
  `RATING_DECAY_DEFAULT_DAYS`.

- loading:

  The loading factor for TORP calculations. Default is
  `RATING_LOADING_DEFAULT`.

- prior_games_recv:

  The number of prior games to consider for receiving. Default is
  `RATING_PRIOR_GAMES_RECV`.

- prior_games_disp:

  The number of prior games to consider for disposal. Default is
  `RATING_PRIOR_GAMES_DISP`.

- plyr_tm_df:

  Optional pre-loaded player team data. If NULL, will load
  automatically.

- player_game_data:

  Optional pre-loaded player game data. If NULL, will load
  automatically.

- prior_games_spoil:

  Prior games for spoil shrinkage. Default is
  `RATING_PRIOR_GAMES_SPOIL`.

- prior_games_hitout:

  Prior games for hitout shrinkage. Default is
  `RATING_PRIOR_GAMES_HITOUT`.

- fixtures:

  Optional pre-loaded fixtures data. If NULL, will load automatically.

## Value

A data frame containing player TORP ratings.
