# Convert NA factor levels to a named level

Lightweight replacement for forcats::fct_na_value_to_level(). Converts a
vector to factor and replaces NA values with the specified level.

## Usage

``` r
fct_na_to_level(x, level = "(Missing)")
```

## Arguments

- x:

  A vector (character or factor).

- level:

  The level name to use for NA values.

## Value

A factor with NAs replaced by the specified level.
