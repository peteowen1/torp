# Load Player Season Ratings Data

Loads pre-computed season-total TORP ratings from the [torpdata
repository](https://github.com/peteowen1/torpdata). This is the output
of
[`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md)
— season totals and PPG leaderboards per player.

## Usage

``` r
load_player_season_ratings(
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

A data frame containing season-total player ratings with columns
including `season`, `player_id`, `player_name`, `team`, `position`,
`games`, `season_points`, `season_recv`, `season_disp`, `season_spoil`,
`season_hitout`, and `ppg`.

## See also

[`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md),
[`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md),
[`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_player_season_ratings(2024)
})
} # }
```
