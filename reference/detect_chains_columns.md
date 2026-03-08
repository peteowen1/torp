# Detect chains column naming convention

Chains data uses camelCase (matchId, displayOrder, teamId) while PBP
data after clean_pbp() uses snake_case (match_id, display_order,
team_id). This detects which convention is present.

## Usage

``` r
detect_chains_columns(dt)
```

## Arguments

- dt:

  A data.table of chains data

## Value

Named list mapping logical names to actual column names
