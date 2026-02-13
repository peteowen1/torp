# Load Player Game Data

Loads processed player game data from the [torpdata
repository](https://github.com/peteowen1/torpdata). This data contains
per-game performance metrics (disposal points, reception points, spoil
points, hitout points) adjusted by position, as used by the TORP ratings
pipeline.

## Usage

``` r
load_player_game_data(seasons = get_afl_season())
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

## Value

A data frame containing player game performance data.

## See also

[`create_player_game_data()`](https://peteowen1.github.io/torp/reference/create_player_game_data.md),
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md),
[`calculate_torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_player_game_data(2024)
})
#> # A tibble: 9,859 × 56
#>    player_id   match_id  season round plyr_nm tm    opp   pos   position team_id
#>    <chr>       <chr>      <dbl> <dbl> <chr>   <chr> <chr> <chr> <chr>    <chr>  
#>  1 CD_I1000061 CD_M2024…   2024     1 Callum… Nort… GWS … KEY_… CHF      CD_T100
#>  2 CD_I1000061 CD_M2024…   2024     2 Callum… Nort… Frem… KEY_… INT      CD_T100
#>  3 CD_I1000061 CD_M2024…   2024     3 Callum… Nort… Carl… KEY_… INT      CD_T100
#>  4 CD_I1000068 CD_M2024…   2024     1 Chris … Adel… Gold… KEY_… FPL      CD_T10 
#>  5 CD_I1000068 CD_M2024…   2024     2 Chris … Adel… Geel… KEY_… INT      CD_T10 
#>  6 CD_I1000068 CD_M2024…   2024     3 Chris … Adel… Frem… KEY_… FPL      CD_T10 
#>  7 CD_I1000068 CD_M2024…   2024     4 Chris … Adel… Melb… KEY_… INT      CD_T10 
#>  8 CD_I1000068 CD_M2024…   2024    11 Chris … Adel… West… KEY_… FPL      CD_T10 
#>  9 CD_I1000068 CD_M2024…   2024    12 Chris … Adel… Hawt… KEY_… FPL      CD_T10 
#> 10 CD_I1000068 CD_M2024…   2024    13 Chris … Adel… Rich… KEY_… FF       CD_T10 
#> # ℹ 9,849 more rows
#> # ℹ 46 more variables: utc_start_time <chr>, tot_p_adj <dbl>,
#> #   recv_pts_adj <dbl>, disp_pts_adj <dbl>, spoil_pts_adj <dbl>,
#> #   hitout_pts_adj <dbl>, tot_p <dbl>, recv_pts <dbl>, disp_pts <dbl>,
#> #   spoil_pts <dbl>, hitout_pts <dbl>, disp <dbl>, recvs <int>,
#> #   extended_stats_spoils <dbl>, tackles <dbl>,
#> #   extended_stats_pressure_acts <dbl>, …
# }
```
