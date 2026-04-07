# Apply additive EPV opponent adjustment to player game data

For each player-game, computes opponent-quality-adjusted EPV columns
(`_oadj` suffix) alongside the existing position-adjusted `_adj`
columns. The adjustment is the gap between league-average EPV conceded
and the specific opponent's EPV conceded, distributed by player TOG
share.

## Usage

``` r
adjust_epv_for_opponents(
  player_game_data,
  lambda_decay = EPV_OPP_LAMBDA_DECAY,
  prior_games = EPV_OPP_PRIOR_GAMES
)
```

## Arguments

- player_game_data:

  data.table of player game data with epv_adj, recv_epv_adj,
  disp_epv_adj, spoil_epv_adj, hitout_epv_adj columns.

- lambda_decay:

  Decay rate per day for opponent defensive profiles.

- prior_games:

  Pseudo-games at league average for shrinkage.

## Value

The input data with additional `_oadj` columns appended:
`recv_epv_oadj`, `disp_epv_oadj`, `spoil_epv_oadj`, `hitout_epv_oadj`,
`contest_epv_oadj` (if present), `epv_oadj`. Original columns unchanged.

## Details

The full adjustment chain visible in output:

- `recv_epv` — raw credit from PBP

- `recv_epv_adj` — position-centered (per-80-min)

- `recv_epv_oadj` — position + opponent adjusted
