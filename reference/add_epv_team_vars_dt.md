# Add EPV team variables (data.table, by reference)

Computes team_id_mdl, home, points, mirror, and coordinate transform.
All shift operations for the mirror calculation are batched into a
single := call for performance.

## Usage

``` r
add_epv_team_vars_dt(dt, grp)
```

## Arguments

- dt:

  A data.table to modify by reference.

- grp:

  Character vector of grouping columns.

## Value

Invisible NULL (modifies dt by reference).
