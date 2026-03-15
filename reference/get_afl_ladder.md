# Fetch AFL Ladder (Current Standings)

Computes the current season ladder from concluded match results. Uses
[`get_afl_results()`](https://peteowen1.github.io/torp/reference/get_afl_results.md)
and
[`calculate_ladder()`](https://peteowen1.github.io/torp/reference/calculate_ladder.md)
internally — no external dependencies.

## Usage

``` r
get_afl_ladder(season = NULL)
```

## Arguments

- season:

  Numeric year (default: current season via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md))

## Value

A data.table with columns: team, played, wins, draws, losses,
points_for, points_against, percentage, ladder_points, rank.

## Examples

``` r
if (FALSE) { # \dontrun{
ladder <- get_afl_ladder(2026)
} # }
```
