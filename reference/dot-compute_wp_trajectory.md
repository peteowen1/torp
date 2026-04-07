# Compute win probability trajectory from scoring events

For each simulation, computes WP at regular time intervals based on
cumulative score margin and time remaining. Aggregates across
simulations to produce percentile bands.

## Usage

``` r
.compute_wp_trajectory(events_dt, pred_total, n_sims)
```

## Arguments

- events_dt:

  data.table of scoring events with cum_home, cum_away columns.

- pred_total:

  Predicted combined total score.

- n_sims:

  Number of simulations.

## Value

data.table with time_pct and WP percentile columns.
