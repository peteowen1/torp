# Explain a Player's EPR Calculation (Per-Game Trace)

Traces the exact per-game inputs to the EPR calculation: shows each
game's `recv_epv_adj`, `disp_epv_adj`, etc. (position-adjusted
per-80-min rates), the decay weight for each game, and how Bayesian
shrinkage produces the final EPR components.

## Usage

``` r
explain_epr(
  player,
  season_val = get_afl_season(type = "current"),
  round_val = get_afl_week(type = "next"),
  top_n = 15
)
```

## Arguments

- player:

  Character player name (partial match OK) or player_id string.

- season_val:

  Season year. Default is current season.

- round_val:

  Round number. Default is next round.

- top_n:

  Number of most recent games to show. Default 15.

## Value

Invisibly returns a list with `game_trace` (per-game data.table) and
`shrinkage` (named list of component calculations).
