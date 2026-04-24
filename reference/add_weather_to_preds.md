# Add weather to a predictions tibble

Joins historical or forecast weather onto an existing predictions tibble
by `match_id`. Adds a compact `weather` summary string (e.g.
`"18\u00B0C, wind 22kph, rain 2.4mm"`, or `"Indoor"` for roofed games),
positioned after `venue`. Optionally also returns raw weather columns.

## Usage

``` r
add_weather_to_preds(preds, raw_cols = FALSE)
```

## Arguments

- preds:

  Predictions tibble with `match_id`, `season`, `round` columns
  (typically the `$predictions` slot of
  [`run_predictions_pipeline()`](https://peteowen1.github.io/torp/reference/run_predictions_pipeline.md),
  or the output of
  [`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md)).

- raw_cols:

  If `TRUE`, also include raw `temp_avg`, `wind_avg`,
  `precipitation_total`, `humidity_avg`, and `is_roof` columns.

## Value

`preds` with a `weather` column (and optionally raw weather columns)
joined by `match_id`.

## Details

For completed matches, weather comes from the Open-Meteo archive via the
torpdata `weather-data` release. For upcoming matches, the Open-Meteo
forecast API is queried per venue.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- run_predictions_pipeline()
result$predictions |>
  dplyr::filter(season == 2026, round == 6) |>
  add_weather_to_preds() |>
  print()
} # }
```
