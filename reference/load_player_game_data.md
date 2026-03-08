# Load Player Game Data

Loads processed player game data from the [torpdata
repository](https://github.com/peteowen1/torpdata). This data contains
per-game performance metrics (disposal points, reception points, spoil
points, hitout points) adjusted by position, as used by the TORP ratings
pipeline.

## Usage

``` r
load_player_game_data(
  seasons = get_afl_season(),
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing player game performance data.

## See also

[`create_player_game_data()`](https://peteowen1.github.io/torp/reference/create_player_game_data.md),
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md),
[`calculate_torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_player_game_data(2024)
})
} # }
```
