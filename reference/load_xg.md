# Load Expected Goals (xG) Data

Loads xg data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_xg(
  seasons = get_afl_season(),
  rounds = NULL,
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

  A numeric vector of round numbers to filter to. If `NULL` (default),
  returns all rounds.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing xG data.

## See also

[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md),
[`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md),
[`calculate_match_xgs()`](https://peteowen1.github.io/torp/reference/calculate_match_xgs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_xg(2021:2022)
  load_xg(2026, 2)
})
} # }
```
