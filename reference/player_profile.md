# Get a Player Profile

Combines raw stats, TORP season ratings, and current TORP rating for a
player into a single object. Accepts partial name matches (e.g.
"Heeney").

## Usage

``` r
player_profile(player_name, seasons = TRUE)
```

## Arguments

- player_name:

  A character string of the player's name (full or partial).

- seasons:

  Seasons to include. Numeric vector of years, or `TRUE` for all
  available.

## Value

A list of class `torp_player_profile` with elements:

- player_info:

  1-row tibble with player_id, name, team, position.

- yearly_stats:

  Per-season aggregated raw stats from
  [`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md).

- torp_season:

  Per-season TORP ratings from
  [`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md).

- current_torp:

  Current TORP rating from
  [`calculate_torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md).

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  player_profile("Heeney")
  player_profile("Heeney", seasons = 2022:2024)
})
} # }
```
