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
# \donttest{
try({ # prevents cran errors
  load_player_skills(2024)
})
#> # A tibble: 19,594 × 107
#>    player_id   player_name     pos_group n_games wt_games ref_date   goals_skill
#>    <chr>       <chr>           <chr>       <int>    <dbl> <date>           <dbl>
#>  1 CD_I990609  Charlie Cameron FWD            75     23.0 2024-03-07      2.57  
#>  2 CD_I293871  Jack Crisp      MID            73     22.7 2024-03-07      0.493 
#>  3 CD_I1002401 Brandon Starce… DEF            74     22.7 2024-03-07      0.0680
#>  4 CD_I996059  Harris Andrews  DEF            73     22.6 2024-03-07      0.0380
#>  5 CD_I293535  Lachie Neale    MID            68     22.3 2024-03-07      0.275 
#>  6 CD_I1008089 Isaac Quaynor   DEF            70     22.2 2024-03-07      0.0365
#>  7 CD_I1000978 Hugh McCluggage MID            73     22.2 2024-03-07      0.768 
#>  8 CD_I1005054 Josh Daicos     MID            68     22.2 2024-03-07      0.675 
#>  9 CD_I298210  Christian Petr… MID            74     22.1 2024-03-07      1.16  
#> 10 CD_I296420  Alex Neal-Bull… FWD            73     21.9 2024-03-07      0.905 
#> # ℹ 19,584 more rows
#> # ℹ 100 more variables: goals_lower <dbl>, goals_upper <dbl>,
#> #   behinds_skill <dbl>, behinds_lower <dbl>, behinds_upper <dbl>,
#> #   shots_at_goal_skill <dbl>, shots_at_goal_lower <dbl>,
#> #   shots_at_goal_upper <dbl>, score_involvements_skill <dbl>,
#> #   score_involvements_lower <dbl>, score_involvements_upper <dbl>,
#> #   goal_assists_skill <dbl>, goal_assists_lower <dbl>, …
# }
```
