# Compute PSR inline from raw data (fallback when pre-computed not available)

Loads player_game_data and player_stats, prepares stat rating data,
estimates ratings for the target round, then applies PSR coefficients.

## Usage

``` r
.compute_psr_inline(season_val, round_val)
```

## Arguments

- season_val:

  Season year.

- round_val:

  Round number.

## Value

A data.table with PSR columns, or NULL on failure.
