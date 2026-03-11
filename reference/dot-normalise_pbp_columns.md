# Normalise PBP column names across API schema versions

The AFL API changed its response schema for 2026+. The old CFS schema
uses names like `homeTeamScore.totalScore` (cleaned to
`home_team_score_total_score`), while the new schema uses
`home.score.totalScore` (cleaned to `home_score_total_score`). Both are
normalised to the canonical name `home_score` via `PBP_COL_MAP`.

## Usage

``` r
.normalise_pbp_columns(dt)
```

## Arguments

- dt:

  A data.table with cleaned (snake_case) column names.

## Value

Invisible NULL (renames columns by reference).
