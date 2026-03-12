# Normalise Player Details Column Names

Maps camelCase columns from squad/player details data to canonical
snake_case.

## Usage

``` r
.normalise_player_details_columns(df)
```

## Arguments

- df:

  A data.frame, tibble, or data.table of player details data.

## Value

The input with normalised column names, modified by reference.
