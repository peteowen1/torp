# Read per-season disk cache for CFS data

Only caches past seasons (before current year). Current season data may
contain provisional lineups or live stats, so it is always fetched fresh
from the API.

## Usage

``` r
.read_season_disk_cache(prefix, season, current_year)
```

## Arguments

- prefix:

  Cache prefix (e.g. "teams", "player_stats")

- season:

  Season year

- current_year:

  Current calendar year

## Value

Data frame if cache hit, NULL if miss
