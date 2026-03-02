# Load Team Ratings Data

Loads pre-computed team-level TORP aggregates from the [torpdata
repository](https://github.com/peteowen1/torpdata). Aggregates are the
sum of TORP ratings for each team's top-21 players (filtered to TORP \>
0) per round, with subcategory breakdowns.

## Usage

``` r
load_team_ratings(columns = NULL)
```

## Arguments

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing team-level ratings with columns including
`season`, `round`, `team`, `team_torp`, `team_recv`, `team_disp`,
`team_spoil`, `team_hitout`, `top_player`, `top_torp`, and `n_players`.

## See also

[`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md),
[`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_team_ratings()
})
#> # A tibble: 2,520 × 11
#>    season round team        team_torp team_recv team_disp team_spoil team_hitout
#>     <dbl> <int> <chr>           <dbl>     <dbl>     <dbl>      <dbl>       <dbl>
#>  1   2021     2 Essendon         32.2      7.76      6.8       20.9         0.51
#>  2   2021     2 Port Adela…      30.7      9.51      9.26      13.9         0.97
#>  3   2021     2 St Kilda         29.6      7.51      4.7       17.9         1.41
#>  4   2021     2 Western Bu…      28.8      9.45      8.56      14.4         0.27
#>  5   2021     2 Richmond         27.9      8.86      7.79      15.4         0.85
#>  6   2021     2 GWS GIANTS       23.9      8.26      5.17      13.6         1.7 
#>  7   2021     2 Sydney Swa…      23.8      9.91      8.44       8.03        0.42
#>  8   2021     2 Hawthorn         23.6      9.95      7.17      10.3         1.09
#>  9   2021     2 Gold Coast…      21.3      6.09      8.11      10.4         0.59
#> 10   2021     2 North Melb…      20.8      6.71      3.72      12.9         0.17
#> # ℹ 2,510 more rows
#> # ℹ 3 more variables: top_player <chr>, top_torp <dbl>, n_players <int>
# }
```
