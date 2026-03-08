# Scrape AFL Weekly Injury List

Scrapes the current injury list from afl.com.au. Returns player names,
injury descriptions, and estimated return timelines.

## Usage

``` r
scrape_injuries(timeout = 30)
```

## Arguments

- timeout:

  Numeric timeout in seconds for the HTTP request.

## Value

A data.frame with columns: `player`, `injury`, `estimated_return`,
`player_norm`.
