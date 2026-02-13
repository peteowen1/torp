# Load AFL Match Predictions Data

Loads AFL match predictions and probability data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_predictions(seasons = get_afl_season())
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

## Value

A data frame containing AFL match predictions including win
probabilities and expected scores.

## See also

[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md),
[`load_results()`](https://peteowen1.github.io/torp/reference/load_results.md),
[`simulate_season()`](https://peteowen1.github.io/torp/reference/simulate_season.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_predictions(2021:2022)
})
#> Loading 1/2 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/predictions/predictions_2021.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/predictions/predictions_2021.parquet>
#> - check internet connection
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/predictions/predictions_2022.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/predictions/predictions_2022.parquet>
#> - check internet connection
#> # A tibble: 0 × 0
# }
```
