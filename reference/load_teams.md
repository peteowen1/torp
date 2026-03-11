# Load AFL Team and Lineup Data

Loads AFL team roster and lineup data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_teams(
  seasons = get_afl_season(),
  use_disk_cache = TRUE,
  refresh = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- use_disk_cache:

  Logical. If TRUE (default), caches completed past seasons to disk so
  they load instantly on subsequent calls. Current season is always
  fetched fresh from the API.

- refresh:

  Logical. If TRUE, clears all caches and fetches fresh data from the
  API for all seasons. Default is FALSE.

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
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_teams(2021:2022)
})
} # }
```
