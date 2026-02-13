# Fill NA values in character vectors

A helper function to fill NA values in character vectors using LOCF or
NOCB. This provides similar functionality to zoo::na.locf0 but is
optimized for use within dplyr pipelines.

## Usage

``` r
nafill_char(x, type = "locf")
```

## Arguments

- x:

  A character vector with potential NA values.

- type:

  Fill type: "locf" (last observation carried forward) or "nocb" (next
  observation carried backward).

## Value

A character vector with NA values filled.
