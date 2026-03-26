# Normalise team name values to canonical forms

Converts team name columns to canonical full names (via
`torp_replace_teams`) and abbreviation columns to canonical
abbreviations (via `torp_team_abbr`). Idempotent — already-canonical
values pass through unchanged.

## Usage

``` r
.normalise_team_values(df)
```

## Arguments

- df:

  A data.frame, tibble, or data.table.

## Value

The input, modified in place. Returns invisibly.
