# Plot season simulation results

Visualizes the output of
[`simulate_afl_season()`](https://peteowen1.github.io/torp/reference/simulate_afl_season.md)
as a ladder probability chart, position heatmap, or finals probability
chart.

## Usage

``` r
plot_simulation(
  sim_results,
  type = c("ladder", "position", "finals"),
  teams = NULL
)
```

## Arguments

- sim_results:

  A `torp_sim_results` object from
  [`simulate_afl_season()`](https://peteowen1.github.io/torp/reference/simulate_afl_season.md).

- type:

  One of `"ladder"` (default), `"position"`, or `"finals"`.

- teams:

  Optional character vector of team names to include. NULL for all.

## Value

A ggplot2 object.
