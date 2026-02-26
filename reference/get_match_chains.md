# Get Match Chains

Retrieves match chain data for a given season and round.

## Usage

``` r
get_match_chains(season = get_afl_season(), round = NA)
```

## Arguments

- season:

  The AFL season year (numeric).

- round:

  The round number (numeric). If NA, retrieves data for all rounds in
  the season.

## Value

A dataframe containing match chain data.

## Examples

``` r
if (FALSE) { # \dontrun{
chains <- get_match_chains(2022, 1)
} # }
```
