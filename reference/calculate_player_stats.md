# Calculate player statistics

Calculate player statistics

## Usage

``` r
calculate_player_stats(
  player_game_data = NULL,
  match_ref,
  date_val,
  decay,
  loading,
  prior_games_recv,
  prior_games_disp,
  prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
  prior_games_hitout = RATING_PRIOR_GAMES_HITOUT
)
```

## Arguments

- player_game_data:

  Player game data frame

- match_ref:

  Match reference

- date_val:

  Date value

- decay:

  Decay factor

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

## Value

A data frame with calculated player statistics
