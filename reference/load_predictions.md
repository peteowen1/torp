# Load AFL Match Predictions Data

Loads AFL match predictions and probability data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_predictions(
  seasons = get_afl_season(),
  rounds = get_afl_week(),
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- rounds:

  A numeric vector associated with given AFL round - defaults to latest
  round. If set to `TRUE`, returns all available rounds in the given
  season range.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing AFL match predictions including win
probabilities and expected scores.

## See also

[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md),
[`load_results()`](https://peteowen1.github.io/torp/reference/load_results.md),
[`simulate_season()`](https://peteowen1.github.io/torp/reference/simulate_season.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_predictions(2021:2022)
})
} # }
```
