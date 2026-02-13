# Load Expected Goals (xG) Data

Loads xg data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_xg(seasons = get_afl_season())
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

## Value

A data frame containing xG data.

## See also

[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md),
[`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md),
[`calculate_match_xgs()`](https://peteowen1.github.io/torp/reference/calculate_match_xgs.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_xg(2021:2022)
})
#> Loading 1/2 files...
#> # A tibble: 414 × 15
#>    match_id     home_team home_shots_score home_xscore home_sG home_sB away_team
#>    <chr>        <chr>                <dbl>       <dbl>   <dbl>   <dbl> <chr>    
#>  1 CD_M2021014… Richmond               105       114.       15      15 Carlton  
#>  2 CD_M2021014… Collingw…               53        60.6       7      11 Western …
#>  3 CD_M2021014… Melbourne               79        97.1      11      13 Fremantle
#>  4 CD_M2021014… Adelaide…              104        89.9      15      14 Geelong …
#>  5 CD_M2021014… Essendon                91        90.8      13      13 Hawthorn 
#>  6 CD_M2021014… Brisbane…               94        65.2      14      10 Sydney S…
#>  7 CD_M2021014… North Me…               65        82.7       9      11 Port Ade…
#>  8 CD_M2021014… GWS GIAN…               78        87.9      11      12 St Kilda 
#>  9 CD_M2021014… West Coa…               83        97.2      12      11 Gold Coa…
#> 10 CD_M2021014… Carlton                 85        75.6      13       7 Collingw…
#> # ℹ 404 more rows
#> # ℹ 8 more variables: away_shots_score <dbl>, away_xscore <dbl>, away_sG <dbl>,
#> #   away_sB <dbl>, score_diff <dbl>, xscore_diff <dbl>, total_points <dbl>,
#> #   total_xpoints <dbl>
# }
```
