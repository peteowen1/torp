# Load AFL Player Details Data

Loads AFL player biographical and details data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_player_details(
  seasons = get_afl_season(),
  use_disk_cache = TRUE,
  refresh = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- use_disk_cache:

  Logical. If TRUE (default), caches completed past seasons to disk so
  they load instantly on subsequent calls. Current season is always
  fetched fresh from the API.

- refresh:

  Logical. If TRUE, clears all caches and fetches fresh data from the
  API for all seasons. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing AFL player biographical details including names,
ages, and team affiliations.

## See also

[`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md),
[`calculate_epr()`](https://peteowen1.github.io/torp/reference/calculate_epr.md),
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_player_details(2021:2022)
})
} # }
```
