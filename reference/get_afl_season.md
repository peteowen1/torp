# Get AFL Season

A helper function to choose the most recent season available for a given
dataset. Currently simplified to return the current calendar year -
future enhancement should implement proper AFL season detection based on
fixture data.

## Usage

``` r
get_afl_season(type = "current")
```

## Arguments

- type:

  A character string or logical: "current" (default) returns the current
  season year, "next" returns the upcoming season, and `TRUE` returns
  all seasons (2021 to current year), consistent with `load_*()`
  functions.

## Value

An integer vector of AFL season year(s).

## Examples

``` r
# Get the current AFL season
get_afl_season("current")
#> [1] 2026

# Get the next AFL season
get_afl_season("next")
#> [1] 2027

# Get all available seasons
get_afl_season(TRUE)
#> [1] 2021 2022 2023 2024 2025 2026
```
