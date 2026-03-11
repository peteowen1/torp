# Normalise Fixture Column Names

Maps dot-notation columns from the AFL API
(`jsonlite::fromJSON(flatten=TRUE)`) and CFS results schema to canonical
snake_case names. Works on both new API output and old parquet files.

## Usage

``` r
.normalise_fixture_columns(df)
```

## Arguments

- df:

  A data.frame, tibble, or data.table of fixture data.

## Value

The input with normalised column names, modified by reference.
