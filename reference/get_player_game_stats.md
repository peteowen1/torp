# Fetch Player Game Stats from AFL API

Fetches live player statistics for one or more matches directly from the
AFL API. Returns normalised snake_case columns.

## Usage

``` r
get_player_game_stats(match_ids)
```

## Arguments

- match_ids:

  Character vector of match IDs (e.g. `"CD_M20260140405"`).

## Value

A data.table of player stats with normalised column names.

## Examples

``` r
if (FALSE) { # \dontrun{
stats <- get_player_game_stats("CD_M20260140405")
stats <- get_player_game_stats(c("CD_M20260140401", "CD_M20260140402"))
} # }
```
