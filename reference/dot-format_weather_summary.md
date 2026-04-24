# Format weather values into a compact human-readable summary

Format weather values into a compact human-readable summary

## Usage

``` r
.format_weather_summary(temp_avg, wind_avg, precipitation_total, is_roof)
```

## Arguments

- temp_avg:

  Numeric vector of avg temperature (degrees C)

- wind_avg:

  Numeric vector of avg wind speed (kph)

- precipitation_total:

  Numeric vector of total precipitation (mm)

- is_roof:

  Logical vector indicating roofed venues

## Value

Character vector of weather summary strings
