# Load Play By Play Data

Loads play by play seasons from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_pbp(
  seasons = get_afl_season(),
  rounds = TRUE,
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- rounds:

  A numeric vector associated with given AFL round - defaults to all
  rounds. If set to `TRUE`, returns all available rounds in the given
  season range.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing play by play data.

## See also

[`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md),
[`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md),
[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md),
[`clean_pbp()`](https://peteowen1.github.io/torp/reference/clean_pbp.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_pbp(2021:2022)
})
#> Downloading 2 files in parallel...
#> # A tibble: 640,911 × 174
#>    display_order description   period_seconds player_id team_id disposal
#>            <int> <chr>                  <int> <chr>     <chr>   <chr>   
#>  1             1 Centre Bounce              0 NA        NA      NA      
#>  2             1 Centre Bounce              0 NA        NA      NA      
#>  3             1 Centre Bounce              0 NA        NA      NA      
#>  4             1 Centre Bounce              0 NA        NA      NA      
#>  5             1 Centre Bounce              0 NA        NA      NA      
#>  6             1 Centre Bounce              0 NA        NA      NA      
#>  7             1 Centre Bounce              0 NA        NA      NA      
#>  8             1 Centre Bounce              0 NA        NA      NA      
#>  9             1 Centre Bounce              0 NA        NA      NA      
#> 10             1 Centre Bounce              0 NA        NA      NA      
#> # ℹ 640,901 more rows
#> # ℹ 168 more variables: shot_at_goal <lgl>, behind_info <chr>, x <dbl>,
#> #   y <dbl>, final_state <chr>, initial_state <chr>, period <int>,
#> #   chain_number <int>, match_id <chr>, venue_width <int>, venue_length <int>,
#> #   home_team_direction_qtr1 <chr>, status <chr>, utc_start_time <chr>,
#> #   home_team_id <chr>, away_team_id <chr>, competition_id <chr>,
#> #   round_number <int>, round_id <chr>, venue_local_start_time <lgl>, …
# }
```
