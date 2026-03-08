# Build complete match model dataset end-to-end

Convenience wrapper chaining all internal .build\_\* functions. Loads
data, builds fixtures, ratings, features, weather, and model dataset.

## Usage

``` r
build_team_mdl_df(season = NULL, target_weeks = NULL, weather_path = NULL)
```

## Arguments

- season:

  Season to build for (default: current via get_afl_season())

- target_weeks:

  Numeric vector of target round numbers (used for weather forecasting)

- weather_path:

  Path to historical weather parquet (default:
  data-raw/weather_data.parquet)

## Value

Complete team_mdl_df ready for GAM training
