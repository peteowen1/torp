# Load Player Stats Data

Loads player stats data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_player_stats(seasons = get_afl_season())
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

## Value

A data frame containing player stats data.

## See also

[`load_player_details()`](https://peteowen1.github.io/torp/reference/load_player_details.md),
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md),
[`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_player_stats(2021:2022)
})
#> Loading 1/2 files...
#> # A tibble: 19,044 × 85
#>    provider_id     utc_start_time       round_name round_round_number venue_name
#>    <chr>           <chr>                <chr>                   <int> <chr>     
#>  1 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#>  2 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#>  3 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#>  4 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#>  5 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#>  6 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#>  7 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#>  8 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#>  9 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#> 10 CD_M20210140101 2021-03-18T08:25:00… Round 1                     1 MCG       
#> # ℹ 19,034 more rows
#> # ℹ 80 more variables: home_team_name <chr>, home_team_club_name <chr>,
#> #   away_team_name <chr>, away_team_club_name <chr>,
#> #   player_jumper_number <int>, player_photo_url <chr>,
#> #   player_player_position <chr>, player_player_player_player_id <chr>,
#> #   player_player_player_player_jumper_number <int>,
#> #   player_player_player_given_name <chr>, …
# }
```
