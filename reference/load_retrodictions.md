# Load Retrodictions Data

Loads retrodictions from the [torpdata
repository](https://github.com/peteowen1/torpdata). Retrodictions are
the current model's predictions for all matches (completed and
upcoming), regenerated each pipeline run. Compare with
[`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md)
which returns locked pre-game predictions frozen at kickoff.

## Usage

``` r
load_retrodictions(
  seasons = get_afl_season(),
  rounds = get_afl_week(type = "next"),
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years - defaults to latest season. If set
  to `TRUE`, returns all available data since 2021.

- rounds:

  A numeric vector of round numbers - defaults to latest round. If set
  to `TRUE`, returns all available rounds.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing retrodictions with the same columns as
[`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md).

## See also

[`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md),
[`load_results()`](https://peteowen1.github.io/torp/reference/load_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_retrodictions(2026)
})
} # }
```
