# Load Player Skill Ratings (PSR)

Loads pre-computed PSR (Player Skill Ratings) from the [torpdata
repository](https://github.com/peteowen1/torpdata). PSR represents each
player's predicted contribution to match margin based on their skill
profile.

## Usage

``` r
load_psr(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL)
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

A data frame containing PSR data with columns including `player_id`,
`player_name`, `season`, `round`, `pos_group`, `psr_raw`, and `psr`.

## See also

[`calculate_psr()`](https://peteowen1.github.io/torp/reference/calculate_psr.md),
[`load_player_stat_ratings()`](https://peteowen1.github.io/torp/reference/load_player_stat_ratings.md),
[`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_psr(2024)
  load_psr(TRUE)  # all seasons
})
} # }
```
