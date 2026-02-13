# Create dummy columns for categorical variables

Lightweight replacement for fastDummies::dummy_cols(). Creates binary
0/1 columns for each level of the specified factor/character columns.

## Usage

``` r
torp_dummy_cols(df, select_columns, remove_first_dummy = FALSE)
```

## Arguments

- df:

  A data frame.

- select_columns:

  Character vector of column names to create dummies for.

- remove_first_dummy:

  Logical; if TRUE, omits the first level (useful for regression).

## Value

The data frame with added dummy columns.
