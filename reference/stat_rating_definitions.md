# Stat definitions for player stat rating estimation

Returns a data.frame describing every stat to estimate. Each row
specifies how to extract the raw value from player data and whether it's
a rate stat (Gamma-Poisson, scaled by TOG) or an efficiency stat
(Beta-Binomial).

## Usage

``` r
stat_rating_definitions()

skill_stat_definitions()
```

## Value

A data.frame with columns:

- stat_name:

  Short name used in output columns

- type:

  "rate" or "efficiency"

- source_col:

  Column name in player_game_data / player_stats for the raw count (rate
  stats)

- category:

  Grouping for display purposes

- success_col:

  Column or expression for successes (efficiency stats only)

- attempts_col:

  Column or expression for attempts (efficiency stats only)
