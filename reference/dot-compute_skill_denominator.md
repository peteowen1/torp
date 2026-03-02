# Compute denominator vector from a column spec string

Supports "col1+col2" for summing multiple columns.

## Usage

``` r
.compute_skill_denominator(dt, denom_spec)
```

## Arguments

- dt:

  A data.table.

- denom_spec:

  A string like "col" or "col1+col2".

## Value

Numeric vector of denominators.
