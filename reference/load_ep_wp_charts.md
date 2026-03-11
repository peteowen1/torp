# Load EP/WP Chart Data

Loads a lightweight subset of play-by-play data optimised for charting
Expected Points (EP) and Win Probability (WP) over a match. Contains
every play but only ~25 columns instead of the full 150+ available from
[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md).

## Usage

``` r
load_ep_wp_charts(
  seasons = get_afl_season(),
  rounds = TRUE,
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons —
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- rounds:

  A numeric vector associated with given AFL round — defaults to all
  rounds. If set to `TRUE`, returns all available rounds in the given
  season range.

- use_disk_cache:

  Logical. If `TRUE`, uses persistent disk cache for faster repeated
  loads. Default is `FALSE`.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing EP/WP chart data with columns including
`match_id`, `season`, `round_number`, `period`, `total_seconds`,
`home_team_name`, `away_team_name`, `team`, `exp_pts`, `delta_epv`,
`wp`, `wpa`, `description`, `player_name`, `play_type`, `shot_row`, and
`points_shot`.

## See also

[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md),
[`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_ep_wp_charts(2024)
})
} # }
```
