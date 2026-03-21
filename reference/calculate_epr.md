# Calculate EPR (Expected Possession Rating)

Calculates EPR ratings for players based on their EPV credit
contributions, with exponential decay weighting and Bayesian shrinkage.

## Usage

``` r
calculate_epr(
  season_val = get_afl_season(type = "current"),
  round_val = get_afl_week(type = "next"),
  decay_recv = EPR_DECAY_RECV,
  decay_disp = EPR_DECAY_DISP,
  decay_spoil = EPR_DECAY_SPOIL,
  decay_hitout = EPR_DECAY_HITOUT,
  loading = EPR_LOADING_DEFAULT,
  prior_games_recv = EPR_PRIOR_GAMES_RECV,
  prior_games_disp = EPR_PRIOR_GAMES_DISP,
  plyr_tm_df = NULL,
  player_game_data = NULL,
  prior_games_spoil = EPR_PRIOR_GAMES_SPOIL,
  prior_games_hitout = EPR_PRIOR_GAMES_HITOUT,
  fixtures = NULL,
  skills = TRUE,
  prior_rate_recv = EPR_PRIOR_RATE_RECV,
  prior_rate_disp = EPR_PRIOR_RATE_DISP,
  prior_rate_spoil = EPR_PRIOR_RATE_SPOIL,
  prior_rate_hitout = EPR_PRIOR_RATE_HITOUT
)
```

## Arguments

- season_val:

  The season to calculate ratings for. Default is the current season.

- round_val:

  The round to calculate ratings for. Default is the next round.

- decay_recv:

  Decay factor (days) for receiving component. Default is
  `EPR_DECAY_RECV`.

- decay_disp:

  Decay factor (days) for disposal component. Default is
  `EPR_DECAY_DISP`.

- decay_spoil:

  Decay factor (days) for spoil component. Default is `EPR_DECAY_SPOIL`.

- decay_hitout:

  Decay factor (days) for hitout component. Default is
  `EPR_DECAY_HITOUT`.

- loading:

  The loading factor for EPR calculations. Default is
  `EPR_LOADING_DEFAULT`.

- prior_games_recv:

  The number of prior games to consider for receiving. Default is
  `EPR_PRIOR_GAMES_RECV`.

- prior_games_disp:

  The number of prior games to consider for disposal. Default is
  `EPR_PRIOR_GAMES_DISP`.

- plyr_tm_df:

  Optional pre-loaded player team data. If NULL, will load
  automatically.

- player_game_data:

  Optional pre-loaded player game data. If NULL, will load
  automatically.

- prior_games_spoil:

  Prior games for spoil shrinkage. Default is `EPR_PRIOR_GAMES_SPOIL`.

- prior_games_hitout:

  Prior games for hitout shrinkage. Default is `EPR_PRIOR_GAMES_HITOUT`.

- fixtures:

  Optional pre-loaded fixtures data. If NULL, will load automatically.

- skills:

  Controls TOG-weighted average adjustment. When active, EPR components
  are re-centered to "above TOG-weighted average" by subtracting the
  weighted mean of each component (weighted by pred_tog =
  `squad_selection_rating * cond_tog_rating`). Accepts:

  - `TRUE` (default): auto-loads stat ratings via
    `get_player_stat_ratings(current = FALSE)`.

  - A data.frame with `player_id`, `cond_tog_rating`, and
    `squad_selection_rating` columns.

  - `FALSE` or `NULL`: skip adjustment.

  Players not in `skills` default to weight 0 (excluded from the
  average, but still have the average subtracted).

- prior_rate_recv:

  Prior rate for receiving shrinkage target. Default is
  `EPR_PRIOR_RATE_RECV`.

- prior_rate_disp:

  Prior rate for disposal shrinkage target. Default is
  `EPR_PRIOR_RATE_DISP`.

- prior_rate_spoil:

  Prior rate for spoil shrinkage target. Default is
  `EPR_PRIOR_RATE_SPOIL`.

- prior_rate_hitout:

  Prior rate for hitout shrinkage target. Default is
  `EPR_PRIOR_RATE_HITOUT`.

## Value

A data frame containing player EPR ratings.
