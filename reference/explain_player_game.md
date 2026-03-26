# Explain a Player's Game Value (EPV + PSV Breakdown)

Shows both the possession-value (EPV) and stat-value (PSV) decomposition
for a player's game performance. EPV comes from play-by-play chain data;
PSV comes from applying glmnet coefficients to box-score stats.

## Usage

``` r
explain_player_game(
  player_id,
  match_id,
  player_stats = NULL,
  season = NULL,
  per80 = TRUE
)
```

## Arguments

- player_id:

  Player ID string (e.g. `"CD_I1001024"`).

- match_id:

  Match ID string (e.g. `"CD_M20260140102"`).

- player_stats:

  Optional pre-loaded player stats data.table. If NULL, loads from API
  via
  [`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md).

- season:

  Season year (used if `player_stats` is NULL).

- per80:

  Logical. If TRUE, show per-80-minute contributions. Default TRUE.

## Value

Invisibly returns a list with `epv` (EPV summary data.table) and `psv`
(PSV per-stat breakdown data.table).
