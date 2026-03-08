# Load Player Skills Data

Loads pre-computed Bayesian player skill estimates from the [torpdata
repository](https://github.com/peteowen1/torpdata). Skills are per-stat
estimates with credible intervals, produced by
[`estimate_player_skills()`](https://peteowen1.github.io/torp/reference/estimate_player_skills.md).

## Usage

``` r
load_player_skills(
  seasons = get_afl_season(),
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons —
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- use_disk_cache:

  Logical. If `TRUE`, uses persistent disk cache for faster repeated
  loads. Default is `FALSE`.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing player skill estimates with columns including
`player_id`, `player_name`, `pos_group`, `n_games`, `wt_games`,
`ref_date`, and `{stat}_skill`, `{stat}_lower`, `{stat}_upper` for each
estimated stat.

## See also

[`estimate_player_skills()`](https://peteowen1.github.io/torp/reference/estimate_player_skills.md),
[`player_skill_profile()`](https://peteowen1.github.io/torp/reference/player_skill_profile.md),
[`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_player_skills(2024)
})
} # }
```
