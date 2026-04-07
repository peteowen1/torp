# Plot match simulation results

Visualizes the output of
[`simulate_match_mc()`](https://peteowen1.github.io/torp/reference/simulate_match_mc.md)
as score distributions, win probability fan charts, margin density, or
quarter breakdowns.

## Usage

``` r
plot_match_simulation(
  match_sim,
  type = c("scores", "wp_trajectory", "margin", "quarters")
)
```

## Arguments

- match_sim:

  A `torp_match_sim` object from
  [`simulate_match_mc()`](https://peteowen1.github.io/torp/reference/simulate_match_mc.md).

- type:

  One of `"scores"`, `"wp_trajectory"`, `"margin"`, or `"quarters"`.

## Value

A ggplot2 object.
