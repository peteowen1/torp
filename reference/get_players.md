# Get Players

This function is intended for internal use and may be unexported in a
future release. Retrieves player data either from the API or from a
local database.

## Usage

``` r
get_players(use_api = FALSE)
```

## Arguments

- use_api:

  Logical, whether to use the API (TRUE) or local database (FALSE,
  default).

## Value

A dataframe containing player data.
