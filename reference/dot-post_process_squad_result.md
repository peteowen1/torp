# Post-process raw squad API response into clean player details

Shared helper used by both get_afl_player_details() (single season) and
.fetch_all_player_details() (multi-season). Handles name construction,
age calculation, prefix stripping, ID renaming, and column
normalisation.

## Usage

``` r
.post_process_squad_result(result, season_col = NULL, scalar_season = NULL)
```

## Arguments

- result:

  Data frame from list_rbind of .parse_squad_json() results.

- season_col:

  Character column name containing per-row season values, or NULL if a
  scalar season should be used.

- scalar_season:

  Single season value (used when season_col is NULL).

## Value

A tibble with normalised player details.
