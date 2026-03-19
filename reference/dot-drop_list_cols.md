# Drop list-columns from a data.frame

Removes any columns that are lists (e.g. nested JSON structs) which
cannot be serialized to parquet or used in model matrices.

## Usage

``` r
.drop_list_cols(df)
```

## Arguments

- df:

  A data.frame.

## Value

The data.frame with list-columns removed.
