# Fetch AFL Fixtures

Fetches fixture data for a season from the AFL API in a single HTTP
call. Includes scores for completed games (same data the results
endpoint returns).

## Usage

``` r
get_afl_fixtures(season = NULL)
```

## Arguments

- season:

  Numeric year, or `TRUE` for all available seasons (default: current
  season via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md))

## Value

A tibble of fixture data

## Examples

``` r
if (FALSE) { # \dontrun{
fixtures <- get_afl_fixtures()
fixtures <- get_afl_fixtures(2025)
} # }
```
