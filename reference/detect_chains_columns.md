# Detect chains column naming convention

Chains data is normalised to snake_case at load/fetch time via
[`.normalise_chains_columns()`](https://peteowen1.github.io/torp/reference/dot-normalise_chains_columns.md).
This function handles both conventions for backward compatibility with
any un-normalised data.

## Usage

``` r
detect_chains_columns(dt)
```

## Arguments

- dt:

  A data.table of chains data

## Value

Named list mapping logical names to actual column names
