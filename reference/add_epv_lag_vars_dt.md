# Add EPV lagged variables and speed (data.table, by reference)

Computes all lag/lead variables and derived speed metrics. Batches shift
operations into a single := call per group traversal.

## Usage

``` r
add_epv_lag_vars_dt(dt, grp)
```

## Arguments

- dt:

  A data.table to modify by reference.

- grp:

  Character vector of grouping columns.

## Value

Invisible NULL (modifies dt by reference).
