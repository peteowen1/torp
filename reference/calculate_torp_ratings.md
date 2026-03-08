# Get TORP ratings

This function calculates TORP (Total Overall Rating Points) for players
based on their performance.

## Usage

``` r
calculate_torp_ratings(
  season_val = get_afl_season(type = "current"),
  round_val = get_afl_week(type = "next"),
  decay_recv = RATING_DECAY_RECV,
  decay_disp = RATING_DECAY_DISP,
  decay_spoil = RATING_DECAY_SPOIL,
  decay_hitout = RATING_DECAY_HITOUT,
  loading = RATING_LOADING_DEFAULT,
  prior_games_recv = RATING_PRIOR_GAMES_RECV,
  prior_games_disp = RATING_PRIOR_GAMES_DISP,
  plyr_tm_df = NULL,
  player_game_data = NULL,
  prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
  prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
  fixtures = NULL,
  skills = TRUE,
  prior_rate_recv = RATING_PRIOR_RATE_RECV,
  prior_rate_disp = RATING_PRIOR_RATE_DISP,
  prior_rate_spoil = RATING_PRIOR_RATE_SPOIL,
  prior_rate_hitout = RATING_PRIOR_RATE_HITOUT
)

torp_ratings(
  season_val = get_afl_season(type = "current"),
  round_val = get_afl_week(type = "next"),
  decay_recv = RATING_DECAY_RECV,
  decay_disp = RATING_DECAY_DISP,
  decay_spoil = RATING_DECAY_SPOIL,
  decay_hitout = RATING_DECAY_HITOUT,
  loading = RATING_LOADING_DEFAULT,
  prior_games_recv = RATING_PRIOR_GAMES_RECV,
  prior_games_disp = RATING_PRIOR_GAMES_DISP,
  plyr_tm_df = NULL,
  player_game_data = NULL,
  prior_games_spoil = RATING_PRIOR_GAMES_SPOIL,
  prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
  fixtures = NULL,
  skills = TRUE,
  prior_rate_recv = RATING_PRIOR_RATE_RECV,
  prior_rate_disp = RATING_PRIOR_RATE_DISP,
  prior_rate_spoil = RATING_PRIOR_RATE_SPOIL,
  prior_rate_hitout = RATING_PRIOR_RATE_HITOUT
)
```

## Arguments

- season_val:

  The season to calculate ratings for. Default is the next season.

- round_val:

  The round to calculate ratings for. Default is the next round.

- decay_recv:

  Decay factor (days) for receiving component. Default is
  `RATING_DECAY_RECV`.

- decay_disp:

  Decay factor (days) for disposal component. Default is
  `RATING_DECAY_DISP`.

- decay_spoil:

  Decay factor (days) for spoil component. Default is
  `RATING_DECAY_SPOIL`.

- decay_hitout:

  Decay factor (days) for hitout component. Default is
  `RATING_DECAY_HITOUT`.

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

- skills:

  Controls TOG-weighted average adjustment. When active, TORP components
  are re-centered to "above TOG-weighted average" by subtracting the
  weighted mean of each component (weighted by pred_tog =
  `squad_selection_skill * cond_tog_skill`). Accepts:

  - `TRUE` (default): auto-loads skills via
    `get_player_skills(current = FALSE)`.

  - A data.frame with `player_id`, `cond_tog_skill`, and
    `squad_selection_skill` columns.

  - `FALSE` or `NULL`: skip adjustment.

  Players not in `skills` default to weight 0 (excluded from the
  average, but still have the average subtracted).

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

A data frame containing player TORP ratings.
