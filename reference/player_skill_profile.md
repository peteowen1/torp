# Get a player's skill profile with percentile ranks

Resolves a player by name (partial match OK), estimates skills for all
players, then returns the target player's row with within-position
percentile ranks appended.

## Usage

``` r
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

  Date to estimate skills as of. Default is today.

- seasons:

  Seasons to include. Numeric vector or TRUE for all.

- params:

  Hyperparameters from
  [`default_skill_params()`](https://peteowen1.github.io/torp/reference/default_skill_params.md).

- skills:

  Optional pre-computed skills data (e.g. from
  `load_player_skills(TRUE)`). If provided, skips the expensive data
  loading and estimation steps. If NULL (default), computes from
  scratch.

## Value

A list of class `torp_skill_profile` with elements:

- player_info:

  Player ID, name, team, position.

- skills:

  Data.frame of skill estimates with percentile ranks.

- ref_date:

  Reference date used.
