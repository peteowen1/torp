# Get player skill estimates from pre-computed data

A fast convenience function that looks up skill estimates from the
pre-computed data stored in torpdata releases. When called with a player
name, returns one row for that player. When called without arguments,
returns the latest snapshot for every player.

## Usage

``` r
get_player_skills(
  player_name = NULL,
  ref_date = NULL,
  seasons = TRUE,
  current = TRUE
)
```

## Arguments

- player_name:

  A character string of the player's name (partial OK). If NULL
  (default), returns all players.

- ref_date:

  Optional date to filter to the latest snapshot at or before this date.
  If NULL, uses the latest available snapshot.

- seasons:

  Seasons to include. Numeric vector or TRUE for all.

- current:

  If TRUE (default), only return players on a current team (i.e. those
  with a skill estimate in the latest season). Set FALSE to include all
  historical players.

## Value

A data.table of skill estimates — one row per player.

## See also

[`player_skill_profile()`](https://peteowen1.github.io/torp/reference/player_skill_profile.md)
for full profile with percentile ranks,
[`load_player_skills()`](https://peteowen1.github.io/torp/reference/load_player_skills.md)
to load raw pre-computed data.
