# Scrape AFL Weekly Injury List

Scrapes the current injury list from afl.com.au. The page contains 18
tables (one per team) in alphabetical team order. Each table's last row
contains an "Updated: ..." date stamp. Returns player names, injury
descriptions, estimated return timelines, team names, and the per-team
updated date.

## Usage

``` r
scrape_injuries(timeout = 30)
```

## Arguments

- timeout:

  Numeric timeout in seconds for the HTTP request.

## Value

A data.frame with columns: `player`, `team`, `injury`,
`estimated_return`, `updated`, `player_norm`.
