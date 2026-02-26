# Load Player Season Ratings Data

Loads pre-computed season-total TORP ratings from the [torpdata
repository](https://github.com/peteowen1/torpdata). This is the output
of
[`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md)
— season totals and PPG leaderboards per player.

## Usage

``` r
load_player_season_ratings(
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

A data frame containing season-total player ratings with columns
including `season`, `player_id`, `player_name`, `team`, `position`,
`games`, `season_points`, `season_recv`, `season_disp`, `season_spoil`,
`season_hitout`, and `ppg`.

## See also

[`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md),
[`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md),
[`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_player_season_ratings(2024)
})
#> # A tibble: 658 × 13
#>    season player_name       player_id team_id team  position games season_points
#>     <dbl> <chr>             <chr>     <chr>   <chr> <chr>    <int>         <dbl>
#>  1   2024 Marcus Bontempel… CD_I2973… CD_T140 West… MIDFIEL…    23          312.
#>  2   2024 Jeremy Cameron    CD_I2938… CD_T70  Geel… KEY_FOR…    24          304 
#>  3   2024 Hayden Young      CD_I1009… CD_T60  Frem… MIDFIEL…    23          302.
#>  4   2024 Hayden McLean     CD_I1003… CD_T160 Sydn… KEY_FOR…    26          299.
#>  5   2024 Dylan Moore       CD_I1006… CD_T80  Hawt… MEDIUM_…    25          294.
#>  6   2024 Errol Gulden      CD_I1013… CD_T160 Sydn… MIDFIEL…    26          284.
#>  7   2024 Nick Daicos       CD_I1023… CD_T40  Coll… MIDFIEL…    23          282.
#>  8   2024 Kai Lohmann       CD_I1014… CD_T20  Bris… MEDIUM_…    27          276.
#>  9   2024 Harris Andrews    CD_I9960… CD_T20  Bris… KEY_DEF…    26          272.
#> 10   2024 Isaac Heeney      CD_I2985… CD_T160 Sydn… MIDFIEL…    24          272.
#> # ℹ 648 more rows
#> # ℹ 5 more variables: season_recv <dbl>, season_disp <dbl>, season_spoil <dbl>,
#> #   season_hitout <dbl>, ppg <dbl>
# }
```
