# Get Players

Retrieves player data either from the API or from a local database.

## Usage

``` r
get_players(season = TRUE, use_api = FALSE)
```

## Arguments

- season:

  Numeric season(s) to load. Defaults to all seasons (TRUE).

- use_api:

  Logical, whether to use the API (TRUE) or local database (FALSE,
  default).

## Value

A dataframe containing player data.
