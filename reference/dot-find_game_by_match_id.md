# Find game metadata for a match ID

Extracts the round number from the match ID structure
(`CD_M{year}{comp}{round}{game}`) and fetches that round's fixtures
directly.

## Usage

``` r
.find_game_by_match_id(season, match_id)
```

## Arguments

- season:

  Numeric season year.

- match_id:

  Match ID string.

## Value

A single-row data.frame of game metadata, or empty data.frame.
