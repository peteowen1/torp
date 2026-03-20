# Apply PSR coefficients to a data.table of player stats

Apply PSR coefficients to a data.table of player stats

## Usage

``` r
.apply_coefs(dt, coefs)
```

## Arguments

- dt:

  A data.table with stat columns.

- coefs:

  A data.frame with stat_name, beta, sd columns.

## Value

A numeric vector of PSR values (one per row).
