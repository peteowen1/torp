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

  A character string: "current" returns the current season, "next"
  returns the upcoming season.

## Value

An integer representing the AFL season year.

## Examples

``` r
# Get the current AFL season
get_afl_season("current")
#> [1] 2026

# Get the next AFL season
get_afl_season("next")
#> [1] 2027
```
