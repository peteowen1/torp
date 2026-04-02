# Add team_id_mdl and goal boundaries (Pass 1.5)

Computes team_id_mdl (the possessing team for each row, handling
throw-ins) and goal boundary markers needed for coordinate fixing.
Extracted from add_quarter_vars_dt so it runs before
fix_chain_coordinates_dt.

## Usage

``` r
add_team_id_mdl_dt(dt)
```

## Arguments

- dt:

  A data.table to modify

## Value

Invisible NULL (modifies dt by reference)
