# Explain a Player's TORP Rating Decomposition

Diagnostic function showing why a player has a given TORP, EPR, and PSR.
Breaks down EPR into recv/disp/spoil/hitout components with shrinkage
diagnostics, and PSR into per-stat contributions. Useful for
understanding divergences between EPR and PSR.

## Usage

``` r
explain_player_rating(
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

  Integer. Number of top PSR contributors to show. Default 15.

## Value

Invisibly returns a list with `epr_breakdown`, `psr_breakdown`,
`game_log`, and `shrinkage_info` components.
