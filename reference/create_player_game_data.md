# Create Player Game Data

Transforms raw play-by-play data and player stats into processed
per-game player performance data used by the TORP ratings pipeline.

## Usage

``` r
create_player_game_data(
  pbp_data = NULL,
  player_stats = NULL,
  teams = NULL,
  decay = RATING_DECAY_DEFAULT_DAYS,
  credit_params = NULL
)
```

## Arguments

- pbp_data:

  Play-by-play data from
  [`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md).
  If NULL, loads all available.

- player_stats:

  Raw player stats from
  [`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md).
  If NULL, loads all available.

- teams:

  Team lineup data from
  [`load_teams()`](https://peteowen1.github.io/torp/reference/load_teams.md).
  If NULL, loads all available.

- decay:

  Decay factor for time-weighting games. Default is
  `RATING_DECAY_DEFAULT_DAYS` (486).

- credit_params:

  Named list of credit assignment parameters. If NULL, uses
  [`default_credit_params()`](https://peteowen1.github.io/torp/reference/default_credit_params.md).

## Value

A data.table with one row per player per match, containing: identifiers
(`player_id`, `match_id`, `season`, `round`, `player_name`, `team`,
`opponent`, `listed_position`, `position`, `team_id`, `utc_start_time`),
position-adjusted TORP credits (`total_credits_adj`, `recv_credits_adj`,
`disp_credits_adj`, `spoil_credits_adj`, `hitout_credits_adj`), raw TORP
credits (`total_credits`, `recv_credits`, `disp_credits`,
`spoil_credits`, `hitout_credits`), and key box-score stats.

## Details

Computes disposal points, reception points, spoil/tackle points, and
hitout points for each player-game combination.
