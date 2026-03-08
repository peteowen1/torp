# Load AFL Match Results Data

Loads AFL match results and scores from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_results(
  seasons = get_afl_season(),
  use_disk_cache = FALSE,
  columns = NULL
)
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

A data frame containing AFL match results and final scores.

## See also

[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md),
[`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md),
[`load_teams()`](https://peteowen1.github.io/torp/reference/load_teams.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_results(2021:2022)
})
} # }
```
