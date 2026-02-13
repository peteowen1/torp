# Load Play By Play Data

Loads play by play seasons from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_pbp(seasons = get_afl_season(), rounds = get_afl_week())
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

## Value

A data frame containing play by play data.

## See also

[`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md),
[`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md),
[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md),
[`clean_pbp()`](https://peteowen1.github.io/torp/reference/clean_pbp.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_pbp(2021:2022)
})
#> Loading 1/2 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_2021_00.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_2021_00.parquet>
#> - check internet connection
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_2022_00.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/pbp-data/pbp_data_2022_00.parquet>
#> - check internet connection
#> # A tibble: 0 × 0
# }
```
