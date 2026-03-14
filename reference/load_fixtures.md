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

  Deprecated. Use `seasons = TRUE` instead (consistent with other
  `load_*()` functions).

- use_cache:

  Logical. If TRUE, uses in-memory cached data when available to speed
  up repeated calls. Default is TRUE.

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
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_fixtures(2021:2022)

  # Load all fixtures
  load_fixtures(seasons = TRUE)
})
} # }
```
