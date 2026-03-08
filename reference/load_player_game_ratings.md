# Load Player Game Ratings Data

Loads pre-computed per-game TORP ratings from the [torpdata
repository](https://github.com/peteowen1/torpdata). This is the output
of
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md)
— a per-game TORP breakdown for every player, ready for leaderboards and
analysis.

## Usage

``` r
load_player_game_ratings(
  seasons = get_afl_season(),
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons —
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- use_disk_cache:

  Logical. If `TRUE`, uses persistent disk cache for faster repeated
  loads. Default is `FALSE`.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing per-game player ratings with columns including
`season`, `round`, `match_id`, `player_id`, `player_name`, `team`,
`total_points`, `recv_points`, `disp_points`, `spoil_points`, and
`hitout_points`.

## See also

[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md),
[`load_player_season_ratings()`](https://peteowen1.github.io/torp/reference/load_player_season_ratings.md),
[`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_player_game_ratings(2024)
})
} # }
```
