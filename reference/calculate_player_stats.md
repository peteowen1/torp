# Calculate player statistics

Calculate player statistics

## Usage

``` r
calculate_player_stats(
  player_game_data = NULL,
  match_ref,
  date_val,
  decay_recv = RATING_DECAY_RECV,
  decay_disp = RATING_DECAY_DISP,
  decay_spoil = RATING_DECAY_SPOIL,
  decay_hitout = RATING_DECAY_HITOUT,
  loading = RATING_LOADING_DEFAULT,
  prior_games_recv = RATING_PRIOR_GAMES_RECV,
  prior_games_disp = RATING_PRIOR_GAMES_DISP,
  prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
  prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
  prior_rate_recv = RATING_PRIOR_RATE_RECV,
  prior_rate_disp = RATING_PRIOR_RATE_DISP,
  prior_rate_spoil = RATING_PRIOR_RATE_SPOIL,
  prior_rate_hitout = RATING_PRIOR_RATE_HITOUT
)
```

## Arguments

- player_game_data:

  Player game data frame

- match_ref:

  Match reference

- date_val:

  Date value

- decay_recv:

  Decay factor (days) for receiving component

- decay_disp:

  Decay factor (days) for disposal component

- decay_spoil:

  Decay factor (days) for spoil component

- decay_hitout:

  Decay factor (days) for hitout component

- loading:

  Loading factor

- prior_games_recv:

  Prior games for receiving

- prior_games_disp:

  Prior games for disposal

- prior_games_spoil:

  Prior games for spoil shrinkage. Default is
  `RATING_PRIOR_GAMES_SPOIL`.

- prior_games_hitout:

  Prior games for hitout shrinkage. Default is
  `RATING_PRIOR_GAMES_HITOUT`.

- prior_rate_recv:

  Prior rate for receiving shrinkage target. Default is
  `RATING_PRIOR_RATE_RECV`.

- prior_rate_disp:

  Prior rate for disposal shrinkage target. Default is
  `RATING_PRIOR_RATE_DISP`.

- prior_rate_spoil:

  Prior rate for spoil shrinkage target. Default is
  `RATING_PRIOR_RATE_SPOIL`.

- prior_rate_hitout:

  Prior rate for hitout shrinkage target. Default is
  `RATING_PRIOR_RATE_HITOUT`.

## Value

A data frame with calculated player statistics
