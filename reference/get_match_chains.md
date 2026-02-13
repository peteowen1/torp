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
# \donttest{
chains <- get_match_chains(2022, 1)
#> Scraping match chains...
#> Loading 1/6 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_details-data/player_details_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_details-data/player_details_2026.parquet>
#> - check internet connection
#> Success!
# }
```
