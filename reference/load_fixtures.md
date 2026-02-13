# Load AFL Fixture Data

Loads AFL fixture and schedule data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_fixtures(
  seasons = NULL,
  all = FALSE,
  use_cache = TRUE,
  cache_ttl = 3600,
  verbose = FALSE
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- all:

  Logical. If TRUE, loads all available fixture data from 2018 onwards.

- use_cache:

  Logical. If TRUE (default), uses cached data when available to speed
  up repeated calls.

- cache_ttl:

  Numeric. Time-to-live for cached data in seconds. Default is 3600 (1
  hour).

- verbose:

  Logical. If TRUE, prints cache hit/miss information.

## Value

A data frame containing AFL fixture and schedule data.

## See also

[`load_results()`](https://peteowen1.github.io/torp/reference/load_results.md),
[`load_teams()`](https://peteowen1.github.io/torp/reference/load_teams.md),
[`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_fixtures(2021:2022)

  # Load all fixtures with caching disabled
  load_fixtures(all = TRUE, use_cache = FALSE)

  # Load with verbose cache information
  load_fixtures(all = TRUE, verbose = TRUE)
})
#> Loading 1/2 files...
#> Loading 1/9 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2019.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2019.parquet>
#> - check internet connection
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2020.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2020.parquet>
#> - check internet connection
#> Cache MISS for fixtures, fetching data
#> Loading 1/9 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2019.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2019.parquet>
#> - check internet connection
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2020.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/fixtures-data/fixtures_2020.parquet>
#> - check internet connection
#> Stored fixtures in cache (1476 rows)
#> # A tibble: 1,476 × 61
#>       id providerId      utcStartTime status compSeason.id compSeason.providerId
#>    <int> <chr>           <chr>        <chr>          <int> <chr>                
#>  1  1475 CD_M20180140101 2018-03-22T… CONCL…            14 CD_S2018014          
#>  2  1707 CD_M20180140102 2018-03-23T… CONCL…            14 CD_S2018014          
#>  3  1476 CD_M20180140103 2018-03-24T… CONCL…            14 CD_S2018014          
#>  4  1477 CD_M20180140104 2018-03-24T… CONCL…            14 CD_S2018014          
#>  5  1474 CD_M20180140105 2018-03-24T… CONCL…            14 CD_S2018014          
#>  6  1483 CD_M20180140106 2018-03-24T… CONCL…            14 CD_S2018014          
#>  7  1490 CD_M20180140107 2018-03-25T… CONCL…            14 CD_S2018014          
#>  8  1482 CD_M20180140108 2018-03-25T… CONCL…            14 CD_S2018014          
#>  9  1488 CD_M20180140109 2018-03-25T… CONCL…            14 CD_S2018014          
#> 10  1534 CD_M20180140201 2018-03-29T… CONCL…            14 CD_S2018014          
#> # ℹ 1,466 more rows
#> # ℹ 55 more variables: compSeason.name <chr>, compSeason.shortName <chr>,
#> #   compSeason.currentRoundNumber <int>, round.id <int>,
#> #   round.providerId <chr>, round.abbreviation <chr>, round.name <chr>,
#> #   round.roundNumber <int>, round.byes <list<list<vctrs_unspecified>>>,
#> #   round.utcStartTime <chr>, round.utcEndTime <chr>, home.team.id <int>,
#> #   home.team.providerId <chr>, home.team.name <chr>, …
# }
```
