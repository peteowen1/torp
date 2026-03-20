# Get Player Match Ratings

Scrapes chain data for a match (or full round) and produces per-player
EPV credit and game-level PSR ratings. Designed to be run immediately
after a game finishes, using live API data rather than stored releases.

## Usage

``` r
get_player_match_ratings(match = NULL, season = get_afl_season(), round = NULL)
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

A data.table with one row per player per match, containing: identifiers
(player_id, match_id, player_name, team, opponent, position), EPV
components (epv, recv_epv, disp_epv, spoil_epv, hitout_epv, epv_adj),
game PSR (game_psr, game_osr, game_dsr), and key box-score stats.

## Examples

``` r
if (FALSE) { # \dontrun{
# From match ID (scrapes everything automatically)
result <- get_player_match_ratings("CD_M20260140201")

# From season and round (all matches in the round)
result <- get_player_match_ratings(round = 2)
result <- get_player_match_ratings(season = 2025, round = 14)

# From pre-scraped chains
chains <- get_match_chains(2026, 2)
result <- get_player_match_ratings(chains)
} # }
```
