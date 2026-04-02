# Fix chain coordinate jumps (Pass 1.6)

Converts coordinates to pitch-relative (home-team) space, fixes
unreasonable jumps around throw-ins and ambiguous possession events,
then converts back to possession-team perspective.

## Usage

``` r
fix_chain_coordinates_dt(dt)
```

## Arguments

- dt:

  A data.table to modify

## Value

Invisible NULL (modifies dt by reference)
