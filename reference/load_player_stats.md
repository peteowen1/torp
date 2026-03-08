# Load Player Stats Data

Loads player stats data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_player_stats(
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

A data frame containing player stats data.

## See also

[`load_player_details()`](https://peteowen1.github.io/torp/reference/load_player_details.md),
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md),
[`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_player_stats(2021:2022)
})
} # }
```
