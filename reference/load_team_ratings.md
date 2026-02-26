# Load Team Ratings Data

Loads pre-computed team-level TORP aggregates from the [torpdata
repository](https://github.com/peteowen1/torpdata). This data summarises
per-round team ratings derived from individual player TORP ratings.

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
`season`, `round`, `team`, `team_torp`, `team_attack`, `team_defence`,
`top_player`, `top_torp`, and `n_players`.

## See also

[`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md),
[`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_team_ratings()
})
#> # A tibble: 2,520 × 9
#>    season round team      team_torp team_attack team_defence top_player top_torp
#>     <dbl> <int> <chr>         <dbl>       <dbl>        <dbl> <chr>         <dbl>
#>  1   2021     2 Essendon       1.37        0.54         0.83 Devon Smi…     3.27
#>  2   2021     2 Western …      1.31        0.68         0.63 Laitham V…     4.22
#>  3   2021     2 Port Ade…      1.28        0.71         0.57 Travis Bo…     3.4 
#>  4   2021     2 St Kilda       1.21        0.41         0.8  Jack Stee…     3.63
#>  5   2021     2 Richmond       1.13        0.66         0.47 Jack Riew…     4.1 
#>  6   2021     2 GWS GIAN…      1.06        0.43         0.63 Jacob Hop…     3.02
#>  7   2021     2 Hawthorn       1           0.68         0.32 Sam Frost      2.47
#>  8   2021     2 Sydney S…      0.97        0.74         0.23 Errol Gul…     3.69
#>  9   2021     2 West Coa…      0.89        0.67         0.22 Tim Kelly      3.25
#> 10   2021     2 Melbourne      0.86        0.5          0.36 Tom McDon…     2.93
#> # ℹ 2,510 more rows
#> # ℹ 1 more variable: n_players <int>
# }
```
