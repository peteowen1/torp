# Get latest lineup player IDs per team

Loads lineup data for the current season and extracts each team's most
recent match lineup. Maps abbreviation-style team names to canonical
names.

## Usage

``` r
.get_latest_lineup_ids()
```

## Value

A list with elements:

- players:

  data.table with `player_id` and `team` (canonical name)

- match_info:

  data.table with `team` and `match_id` for each team's latest match

Returns NULL if lineup data is unavailable.
