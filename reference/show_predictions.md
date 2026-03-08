# Show TORP Match Predictions

Display a formatted summary of TORP match predictions for a given round,
including predicted margins, win probabilities, and actual results where
available.

## Usage

``` r
show_predictions(
  season = get_afl_season(),
  week = get_afl_week(),
  refresh = FALSE
)
```

## Arguments

- season:

  Season year (default: current via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md))

- week:

  Round number (default: current via
  [`get_afl_week()`](https://peteowen1.github.io/torp/reference/get_afl_week.md))

- refresh:

  If `TRUE`, run
  [`run_predictions_pipeline()`](https://peteowen1.github.io/torp/reference/run_predictions_pipeline.md)
  first

## Value

Predictions tibble with results joined (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
show_predictions()
show_predictions(2025, 10)
show_predictions(refresh = TRUE)
} # }
```
