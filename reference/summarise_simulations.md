# Summarise simulation results

Aggregates ladder and finals results across all simulations into a
single team-level summary table.

## Usage

``` r
summarise_simulations(sim_results)
```

## Arguments

- sim_results:

  An object of class `"torp_sim_results"`.

## Value

A data.table with one row per team, ordered by average wins descending.
