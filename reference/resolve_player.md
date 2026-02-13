# Resolve a player name to a unique player ID

Internal helper that takes a player name string (partial match OK) and
resolves it to a single player ID using fuzzy matching via
[`norm_name()`](https://peteowen1.github.io/torp/reference/norm_name.md).

## Usage

``` r
resolve_player(player_name, seasons = TRUE)
```

## Arguments

- player_name:

  A character string of the player's name (full or partial).

- seasons:

  Seasons to search across. Passed to
  [`load_player_details()`](https://peteowen1.github.io/torp/reference/load_player_details.md).

## Value

A list with elements `player_id`, `player_name`, `team`, `position`.
