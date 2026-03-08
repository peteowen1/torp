# Prepare data for skill estimation

Joins player game data with player stats to produce a single table with
all required columns for the skill estimation pipeline.

## Usage

``` r
prepare_skill_data(
  player_game_data,
  player_stats,
  rosters = NULL,
  fixtures = NULL
)
```

## Arguments

- player_game_data:

  Player game data from `load_player_game_data(TRUE)`.

- player_stats:

  Player stats from `load_player_stats(TRUE)`.

- rosters:

  Optional roster data. If NULL, loads from torpdata.

- fixtures:

  Optional fixture data. If NULL, loads from torpdata.

## Value

A data.table with one row per player-match containing: identifiers
(player_id, match_id, player_name, season, round, team),
match_date_skill (Date), tog (time on ground as fraction), position, and
all stat columns referenced by
[`skill_stat_definitions()`](https://peteowen1.github.io/torp/reference/skill_stat_definitions.md).
