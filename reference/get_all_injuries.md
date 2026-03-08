# Get All Injuries (Weekly + Preseason)

Combines the live AFL weekly injury list with the curated preseason
injury list. Weekly entries take precedence when a player appears in
both sources (deduplicated by normalized name).

## Usage

``` r
get_all_injuries(season, scrape = TRUE)
```

## Arguments

- season:

  Numeric season year (e.g. 2026).

- scrape:

  Logical; if TRUE (default), scrapes the live AFL injury list. Set to
  FALSE for pre-season use or testing when the AFL page is empty.

## Value

A data.frame with columns: `player`, `injury`, `estimated_return`,
`player_norm`, `source`. The `source` column is `"weekly"` or
`"preseason"`.
