# Adjust player stat ratings for opponent quality

For each rate stat, computes an opponent adjustment factor based on how
much of that stat each opponent concedes relative to league average.
Players who faced tougher schedules get their ratings boosted.

## Usage

``` r
adjust_stat_ratings_for_opponents(
  stat_ratings,
  stat_rating_data,
  ref_date,
  lambda_decay = OPP_ADJ_LAMBDA_DECAY,
  cap = OPP_ADJ_FACTOR_CAP,
  stat_defs = NULL
)
```

## Arguments

- stat_ratings:

  data.table of player stat ratings from
  [`estimate_player_stat_ratings()`](https://peteowen1.github.io/torp/reference/estimate_player_stat_ratings.md),
  with `{stat}_rating` columns.

- stat_rating_data:

  data.table from
  [`.prepare_stat_rating_data()`](https://peteowen1.github.io/torp/reference/dot-prepare_stat_rating_data.md).
  Must include `team`, `opponent`, and `match_id` columns.

- ref_date:

  Date. Only matches before this date contribute to opponent defensive
  profiles.

- lambda_decay:

  Numeric. Decay rate per day for opponent profile recency weighting.
  Default uses `OPP_ADJ_LAMBDA_DECAY`.

- cap:

  Numeric vector of length 2: floor and ceiling for adjustment factors.
  Default uses `OPP_ADJ_FACTOR_CAP`.

- stat_defs:

  Output of
  [`stat_rating_definitions()`](https://peteowen1.github.io/torp/reference/stat_rating_definitions.md).
  If NULL, uses default.

## Value

The input `stat_ratings` with additional `{stat}_adj_rating` columns for
each rate stat. Original columns are unchanged.
