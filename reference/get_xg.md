# Get Live xG for a Match or Round

Scrapes chain data from the AFL API, runs the shot model pipeline, and
computes match-level xG summaries. Useful for matches that just finished
before the daily torpdata pipeline has run.

## Usage

``` r
get_xg(
  match = NULL,
  season = get_afl_season(),
  round = NULL,
  quarter = 1:4,
  detail = FALSE
)
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

- quarter:

  Numeric vector of quarters to include (default: `1:4`).

- detail:

  Logical. If `TRUE`, returns the full play-by-play data (filtered to
  shots) instead of match-level summaries. Useful for debugging xG
  values. Default is `FALSE`.

## Value

When `detail = FALSE` (default), a tibble with one row per match. When
`detail = TRUE`, the full shot-level PBP with columns like `xscore`,
`goal_prob`, `behind_prob`, `points_shot`, `shot_row`.

## Examples

``` r
if (FALSE) { # \dontrun{
# From match ID
get_xg("CD_M20260140201")

# From season and round (all matches in the round)
get_xg(round = 2)
get_xg(season = 2025, round = 14)

# Debug: return shot-level data
get_xg(round = 2, detail = TRUE)

# From pre-scraped chains
chains <- get_match_chains(2026, 2)
get_xg(chains)
} # }
```
