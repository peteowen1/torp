# Load Player Game Ratings Data

Loads pre-computed per-game TORP ratings from the [torpdata
repository](https://github.com/peteowen1/torpdata). This is the output
of
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md)
— a per-game TORP breakdown for every player, ready for leaderboards and
analysis.

## Usage

``` r
load_player_game_ratings(
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

A data frame containing per-game player ratings with columns including
`season`, `round`, `match_id`, `player_id`, `player_name`, `team`,
`total_points`, `recv_points`, `disp_points`, `spoil_points`, and
`hitout_points`.

## See also

[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md),
[`load_player_season_ratings()`](https://peteowen1.github.io/torp/reference/load_player_season_ratings.md),
[`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_player_game_ratings(2024)
})
#> # A tibble: 9,859 × 14
#>    season round player_name    position        team_id  team  opp   total_points
#>     <dbl> <dbl> <chr>          <chr>           <chr>    <chr> <chr>        <dbl>
#>  1   2024    23 Brent Daniels  MEDIUM_FORWARD  CD_T1010 GWS … Frem…         44.9
#>  2   2024    14 Joel Amartey   KEY_FORWARD     CD_T160  Sydn… Adel…         41.6
#>  3   2024    19 Ben Keays      MEDIUM_FORWARD  CD_T10   Adel… Esse…         41.4
#>  4   2024    24 Jeremy Cameron KEY_FORWARD     CD_T70   Geel… West…         38.5
#>  5   2024    11 Jarrod Witts   RUCK            CD_T1000 Gold… Carl…         36.8
#>  6   2024    19 Tim Membrey    KEY_FORWARD     CD_T130  St K… West…         36.4
#>  7   2024     9 Ed Richards    MIDFIELDER      CD_T140  West… Rich…         36.2
#>  8   2024    17 Lachie Neale   MIDFIELDER      CD_T20   Bris… Adel…         32.8
#>  9   2024    24 Sam Flanders   MEDIUM_DEFENDER CD_T1000 Gold… Rich…         32.4
#> 10   2024    24 Jack Bowes     MIDFIELDER      CD_T70   Geel… West…         32  
#> # ℹ 9,849 more rows
#> # ℹ 6 more variables: recv_points <dbl>, disp_points <dbl>, spoil_points <dbl>,
#> #   hitout_points <dbl>, player_id <chr>, match_id <chr>
# }
```
