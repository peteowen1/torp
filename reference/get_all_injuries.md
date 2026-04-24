# Get All Injuries (Weekly + Preseason)

Combines the live AFL weekly injury list with the curated preseason
injury list. Weekly entries take precedence when a player appears in
both sources (deduplicated by normalized name). Preseason entries for
players who have already played a senior game this season are dropped —
otherwise stale "TBC" entries linger indefinitely and exclude healthy
players from team ratings (e.g. a preseason concussion listing for
someone who has played every round since).

## Usage

``` r
get_all_injuries(season, scrape = TRUE, drop_played_preseason = TRUE)
```

## Arguments

- season:

  Numeric season year (e.g. 2026).

- scrape:

  Logical; if TRUE (default), scrapes the live AFL injury list. Set to
  FALSE for pre-season use or testing when the AFL page is empty.

- drop_played_preseason:

  Logical; if TRUE (default), drops preseason entries for players who
  appear in this season's
  [`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md).
  Set to FALSE to inspect the raw merged list.

## Value

A data.frame with columns: `player`, `team`, `injury`,
`estimated_return`, `updated`, `player_norm`, `source`. The `source`
column is `"weekly"` or `"preseason"`. `updated` is the per-team date
from the AFL injury page (Date class; NA for preseason entries).
