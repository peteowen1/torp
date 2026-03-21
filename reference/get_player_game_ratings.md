# Get Player Game Ratings (Live)

Scrapes chain data for a match (or full round) and produces per-player
EPV ratings in the same format as
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md).
Designed to be run immediately after a game finishes, using live API
data rather than stored releases.

## Usage

``` r
get_player_game_ratings(match = NULL, season = get_afl_season(), round = NULL)
```

## Arguments

- match:

  Input: either a match ID string (e.g. `"CD_M20260140201"`) or a
  pre-scraped chains data.frame from
  [`get_match_chains()`](https://peteowen1.github.io/torp/reference/get_match_chains.md).
  If `NULL` (default), uses `season` and `round` to fetch chains.

- season:

  Numeric season year (default: current season via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md)).
  Only used when `match` is `NULL`.

- round:

  Numeric round number. Only used when `match` is `NULL`.

## Value

A tibble with the same columns as
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md):
identifiers, TOG-weighted centered EPV components, and position-adjusted
EPV per-80 metrics.

## Examples

``` r
if (FALSE) { # \dontrun{
# From match ID (scrapes everything automatically)
result <- get_player_game_ratings("CD_M20260140201")

# From season and round (all matches in the round)
result <- get_player_game_ratings(round = 2)
result <- get_player_game_ratings(season = 2025, round = 14)

# From pre-scraped chains
chains <- get_match_chains(2026, 2)
result <- get_player_game_ratings(chains)
} # }
```
