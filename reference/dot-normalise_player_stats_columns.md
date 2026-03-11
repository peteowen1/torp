# Normalise Player Stats Column Names

Maps both old (2021-2025) and new (2026+ v2 API) player stats schemas to
clean canonical names. Handles:

- v2 prefix stripping: `stats_goals` -\> `goals`

- Player column renames: `player_player_player_player_id` -\>
  `player_id`

- Match column renames: `provider_id` -\> `match_id`

- Round column: `round_round_number` -\> `round_number`

- Position column: `player_player_position` -\> `position`

- Extended stats prefix: `extended_stats_spoils` -\> `spoils`

- Clearance columns: `clearances_total_clearances` -\> `clearances`

- Missing `season` column: derived from `utc_start_time`

## Usage

``` r
.normalise_player_stats_columns(dt)
```

## Arguments

- dt:

  A data.table or data.frame of player stats.

## Value

The input (coerced to data.table) with normalised column names, modified
by reference.
