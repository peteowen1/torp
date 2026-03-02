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
  verbose = FALSE,
  columns = NULL,
  use_disk_cache = FALSE
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- all:

  Logical. If TRUE, loads all available fixture data from 2021 onwards.

- use_cache:

  Logical. If TRUE (default), uses cached data when available to speed
  up repeated calls.

- cache_ttl:

  Numeric. Time-to-live for cached data in seconds. Default is 3600 (1
  hour).

- verbose:

  Logical. If TRUE, prints cache hit/miss information.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk caching. Default is FALSE.

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
#> Downloading 2 files in parallel...
#> Downloading 6 files in parallel...
#> Cache MISS for fixtures, fetching data
#> Downloading 6 files in parallel...
#> Stored fixtures in cache (1269 rows)
#> # A tibble: 1,269 × 60
#>       id providerId      utcStartTime status compSeason.id compSeason.providerId
#>    <int> <chr>           <chr>        <chr>          <int> <chr>                
#>  1  2991 CD_M20210140101 2021-03-18T… CONCL…            34 CD_S2021014          
#>  2  2986 CD_M20210140102 2021-03-19T… CONCL…            34 CD_S2021014          
#>  3  2992 CD_M20210140103 2021-03-20T… CONCL…            34 CD_S2021014          
#>  4  2993 CD_M20210140104 2021-03-20T… CONCL…            34 CD_S2021014          
#>  5  2994 CD_M20210140105 2021-03-20T… CONCL…            34 CD_S2021014          
#>  6  2987 CD_M20210140106 2021-03-20T… CONCL…            34 CD_S2021014          
#>  7  2990 CD_M20210140107 2021-03-21T… CONCL…            34 CD_S2021014          
#>  8  2989 CD_M20210140108 2021-03-21T… CONCL…            34 CD_S2021014          
#>  9  2988 CD_M20210140109 2021-03-21T… CONCL…            34 CD_S2021014          
#> 10  2999 CD_M20210140201 2021-03-25T… CONCL…            34 CD_S2021014          
#> # ℹ 1,259 more rows
#> # ℹ 54 more variables: compSeason.name <chr>, compSeason.shortName <chr>,
#> #   compSeason.currentRoundNumber <int>, round.id <int>,
#> #   round.providerId <chr>, round.abbreviation <chr>, round.name <chr>,
#> #   round.roundNumber <int>, home.team.id <int>, home.team.providerId <chr>,
#> #   home.team.name <chr>, home.team.abbreviation <chr>,
#> #   home.team.nickname <chr>, home.team.teamType <chr>, …
# }
```
