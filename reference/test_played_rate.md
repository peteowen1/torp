# TEST Listing Played-Rate

For each completed round, finds players listed as "Test" on the weekly
injury list at the latest scrape before the round's first match, and
checks whether they were named in the selected 22 (non-EMERG/SUB).
Useful for validating whether the model's TEST-as-TBC assumption is too
harsh.

## Usage

``` r
test_played_rate(season = NULL, round = NULL)
```

## Arguments

- season:

  Season year. Defaults to current via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md).

- round:

  Optional round number (or vector) to filter to. If `NULL`, analyses
  all rounds with injury history and lineup data available.

## Value

A tibble with one row per TEST listing: `round`, `player`, `team`,
`injury`, `scraped_at`, `played` (logical).

## Details

Requires accumulated injury snapshots from
[`save_injury_data()`](https://peteowen1.github.io/torp/reference/save_injury_data.md)
– returns an empty tibble if the release predates `scraped_at` tracking
or if no TEST listings have been captured yet.
