# Build fixture dataframe with temporal features

Pivots home/away, adds date/time features with timezone-safe conversion.

## Usage

``` r
.build_fixtures_df(fixtures)
```

## Arguments

- fixtures:

  Raw fixtures from load_fixtures()

## Value

Pivoted fixture dataframe with temporal columns
