# Load weather data for match modelling

Loads historical weather from parquet and optionally fetches forecast
for upcoming matches via Open-Meteo API.

## Usage

``` r
.load_match_weather(
  fixtures,
  all_grounds,
  target_weeks = NULL,
  season = NULL,
  weather_path = NULL
)
```

## Arguments

- fixtures:

  Raw fixtures (for identifying upcoming matches)

- all_grounds:

  Stadium reference data (for geocoding)

- target_weeks:

  Numeric vector of target round numbers

- season:

  Target season

- weather_path:

  Path to historical weather parquet file

## Value

Tibble with match_id, temp_avg, precipitation_total, wind_avg,
humidity_avg, is_roof
