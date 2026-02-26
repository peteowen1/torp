# Load EP/WP Chart Data

Loads a lightweight subset of play-by-play data optimised for charting
Expected Points (EP) and Win Probability (WP) over a match. Contains
every play but only ~25 columns instead of the full 150+ available from
[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md).

## Usage

``` r
load_ep_wp_charts(
  seasons = get_afl_season(),
  rounds = TRUE,
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons —
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- rounds:

  A numeric vector associated with given AFL round — defaults to all
  rounds. If set to `TRUE`, returns all available rounds in the given
  season range.

- use_disk_cache:

  Logical. If `TRUE`, uses persistent disk cache for faster repeated
  loads. Default is `FALSE`.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing EP/WP chart data with columns including
`match_id`, `season`, `round_number`, `period`, `total_seconds`,
`home_team_team_name`, `away_team_team_name`, `team`, `exp_pts`,
`delta_epv`, `wp`, `wpa`, `description`, `player_name`, `play_type`,
`shot_row`, and `points_shot`.

## See also

[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md),
[`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_ep_wp_charts(2024)
})
#> # A tibble: 324,958 × 25
#>    match_id        season round_number period period_seconds total_seconds
#>    <chr>            <dbl>        <int>  <int>          <int>         <dbl>
#>  1 CD_M20240140001   2024            0      1              0             0
#>  2 CD_M20240140002   2024            0      1              0             0
#>  3 CD_M20240140004   2024            0      1              0             0
#>  4 CD_M20240140102   2024            1      1              0             0
#>  5 CD_M20240140103   2024            1      1              0             0
#>  6 CD_M20240140106   2024            1      1              0             0
#>  7 CD_M20240140107   2024            1      1              0             0
#>  8 CD_M20240140108   2024            1      1              0             0
#>  9 CD_M20240140201   2024            2      1              0             0
#> 10 CD_M20240140202   2024            2      1              0             0
#> # ℹ 324,948 more rows
#> # ℹ 19 more variables: display_order <int>, home_team_team_name <chr>,
#> #   away_team_team_name <chr>, team <chr>, home <dbl>, pos_team_points <int>,
#> #   opp_team_points <int>, points_diff <int>, home_points <int>,
#> #   away_points <int>, exp_pts <dbl>, delta_epv <dbl>, wp <dbl>, wpa <dbl>,
#> #   description <chr>, player_name <chr>, play_type <fct>, shot_row <int>,
#> #   points_shot <dbl>
# }
```
