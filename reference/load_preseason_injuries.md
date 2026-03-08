# Load Preseason Injury List

Reads a curated CSV of preseason injuries from `inst/extdata/`. These
capture long-term injuries known before the season starts (e.g., ACL
reconstructions, stress fractures) that won't appear on the weekly AFL
injury list until teams are required to report.

## Usage

``` r
load_preseason_injuries(season)
```

## Arguments

- season:

  Numeric season year (e.g. 2026).

## Value

A data.frame with columns: `player`, `team`, `injury`,
`estimated_return`, `player_norm`. Returns an empty data.frame (with
correct columns) if no file exists for the requested season.
