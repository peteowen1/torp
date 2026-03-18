# Load Weather Data

Downloads historical match weather data from the torpdata GitHub
release. Returns a tibble with match-level weather aggregates (temp,
wind, humidity, precipitation) derived from Open-Meteo archive data.

## Usage

``` r
load_weather()
```

## Value

A tibble with columns: match_id, temp_avg, wind_avg, humidity_avg,
precipitation_total, is_roof, and additional metadata.

## See also

[`save_to_release()`](https://peteowen1.github.io/torp/reference/save_to_release.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_weather()
})
} # }
```
