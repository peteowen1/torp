# Adjust per-game player stats for opponent quality

For each player-game and each rate stat, computes an opponent adjustment
factor based on decay-weighted team defensive profiles. Creates
`{stat}_oadj` columns alongside the originals.

## Usage

``` r
adjust_stats_for_opponents(
  player_stats,
  lambda_decay = OPP_ADJ_LAMBDA_DECAY,
  prior_games = 5,
  cap = OPP_ADJ_FACTOR_CAP,
  stat_defs = NULL,
  rolling = FALSE
)
```

## Arguments

- player_stats:

  data.table of player stats with per-game stat counts. Must have
  `team`, `opponent`, `match_id`, and either `match_date_rating` or
  `utc_start_time` columns. If `team`/`opponent` are missing, derives
  them from `home_team_name`, `away_team_name`, `team_status`.

- lambda_decay:

  Decay rate per day for opponent profiles.

- prior_games:

  Pseudo-games at league average for shrinkage.

- cap:

  Floor/ceiling for adjustment factors.

- stat_defs:

  Output of
  [`stat_rating_definitions()`](https://peteowen1.github.io/torp/reference/stat_rating_definitions.md).
  If NULL, uses default.

- rolling:

  Logical. If TRUE, computes causal rolling profiles (each game uses
  only prior games for its opponent profile). If FALSE (default),
  computes a single snapshot profile at the latest date.

## Value

The input data with additional `{stat}_oadj` columns for each rate stat.
