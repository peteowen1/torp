# Clean Play-by-Play Data

This function cleans and processes play-by-play data for AFL matches.

## Usage

``` r
clean_pbp(df)
```

## Arguments

- df:

  A dataframe containing raw play-by-play data.

## Value

A cleaned and processed dataframe with additional variables.

## Examples

``` r
if (FALSE) { # \dontrun{
chains <- load_chains(2024, rounds = 1)
cleaned_data <- clean_pbp(chains)
} # }
```
