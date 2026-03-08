# Load Chains Data

Loads chains data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_chains(
  seasons = get_afl_season(),
  rounds = TRUE,
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

  A numeric vector associated with given AFL round - defaults to all
  rounds. If set to `TRUE`, returns all available rounds in the given
  season range.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing chains data.

## See also

[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md),
[`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md),
[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_chains(2021:2022)
})
} # }
```
