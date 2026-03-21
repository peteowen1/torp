# Get a player's stat rating profile with percentile ranks

Resolves a player by name (partial match OK), estimates stat ratings for
all players, then returns the target player's row with within-position
percentile ranks appended.

## Usage

``` r
player_stat_rating_profile(
  player_name,
  ref_date = Sys.Date(),
  seasons = TRUE,
  params = NULL,
  skills = NULL
)

player_skill_profile(
  player_name,
  ref_date = Sys.Date(),
  seasons = TRUE,
  params = NULL,
  skills = NULL
)
```

## Arguments

- player_name:

  A character string of the player's name (partial OK).

- ref_date:

  Date to estimate stat ratings as of. Default is today.

- seasons:

  Seasons to include. Numeric vector or TRUE for all.

- params:

  Hyperparameters from
  [`default_stat_rating_params()`](https://peteowen1.github.io/torp/reference/default_stat_rating_params.md).

- skills:

  Optional pre-computed stat ratings data (e.g. from
  `load_player_stat_ratings(TRUE)`). If provided, skips the expensive
  data loading and estimation steps. If NULL (default), computes from
  scratch.

## Value

A list of class `torp_stat_rating_profile` with elements:

- player_info:

  Player ID, name, team, position.

- skills:

  Data.frame of stat rating estimates with percentile ranks.

- ref_date:

  Reference date used.
