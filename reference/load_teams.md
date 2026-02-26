# Load AFL Team and Lineup Data

Loads AFL team roster and lineup data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_teams(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing AFL team and player lineup data.

## See also

[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md),
[`load_results()`](https://peteowen1.github.io/torp/reference/load_results.md),
[`load_player_details()`](https://peteowen1.github.io/torp/reference/load_player_details.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_teams(2021:2022)
})
#> Downloading 2 files in parallel...
#> # A tibble: 19,044 × 22
#>    providerId      utcStartTime           status compSeason.shortName round.name
#>    <chr>           <chr>                  <chr>  <chr>                <chr>     
#>  1 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#>  2 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#>  3 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#>  4 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#>  5 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#>  6 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#>  7 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#>  8 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#>  9 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#> 10 CD_M20210140101 2021-03-18T08:25:00.0… CONCL… Premiership          Round 1   
#> # ℹ 19,034 more rows
#> # ℹ 17 more variables: round.roundNumber <int>, venue.name <chr>,
#> #   teamAbbr <chr>, teamName <chr>, teamNickname <chr>, teamId <chr>,
#> #   position <chr>, player.playerId <chr>, player.captain <lgl>,
#> #   player.playerJumperNumber <int>, player.playerName.givenName <chr>,
#> #   player.playerName.surname <chr>, teamStatus <chr>, teamType <chr>,
#> #   lateChanges <chr>, season <dbl>, row_id <chr>
# }
```
