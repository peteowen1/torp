# Get player season stats

Aggregates per-game box-score stats from
[`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md)
into season totals or per-80-minute averages. Useful for seeing stat
leaders.

## Usage

``` r
player_season_stats(
  season_val = get_afl_season(),
  per80 = FALSE,
  sort_by = "disposals"
)
```

## Arguments

- season_val:

  Season year(s). Default is current season.

- per80:

  Logical. If `TRUE`, return per-80-minute averages. Default `FALSE`
  (season totals).

- sort_by:

  Column name to sort by (descending). Default `"disposals"`.

## Value

A data.table with season-aggregated stats.
