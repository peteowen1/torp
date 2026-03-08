# Fetch AFL Results

Returns completed match results for a season. Zero additional HTTP calls
— derives results from fixture data (which includes scores).

## Usage

``` r
get_afl_results(season = NULL)
```

## Arguments

- season:

  Numeric year (default: current season via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md))

## Value

A tibble of completed match data (fixture schema, filtered to concluded
games)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- get_afl_results(2025)
} # }
```
