# Apply PSR coefficients to raw per-game stats

Computes a game-level PSR by applying the glmnet coefficients directly
to each player's box-score stats from a single game. Rate stats
(efficiency percentages) are computed from the raw counts.

## Usage

``` r
.add_game_psr(player_epv)
```

## Arguments

- player_epv:

  A data.table from
  [`create_player_game_data()`](https://peteowen1.github.io/torp/reference/create_player_game_data.md).

## Value

The input data.table with `game_psr`, `game_osr`, `game_dsr` added.
