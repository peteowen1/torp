# Calculate Mirror Values

Determines whether field coordinates should be mirrored based on
throw-in patterns and team possession changes. This function handles
complex field position adjustments for AFL analytics by breaking down
the logic into understandable conditions.

## Usage

``` r
calculate_mirror(throw_in, team_id_mdl, x)
```

## Arguments

- throw_in:

  A vector indicating if the play is a throw-in.

- team_id_mdl:

  A vector of team IDs for modeling.

- x:

  A vector of x-coordinates.

## Value

A vector of mirror values (-1 or 1).
