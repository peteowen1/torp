# Normalise Stat Rating Column Names

Maps old `*_skill` column names (and their `_lower`/`_upper` credible
interval variants) to the new `*_rating` suffix convention. Runs
dynamically from
[`stat_rating_definitions()`](https://peteowen1.github.io/torp/reference/stat_rating_definitions.md)
so new stats are automatically covered.

## Usage

``` r
.normalise_stat_rating_columns(df)
```

## Arguments

- df:

  A data.frame, tibble, or data.table containing stat rating data.

## Value

The input with normalised column names, modified by reference.
