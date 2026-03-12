# Get a Team Profile

Combines team TORP rating, season record, and top players into a single
object. Accepts partial name matches, abbreviations, or full names (e.g.
"Sydney", "SYD", "Sydney Swans").

## Usage

``` r
team_profile(team_name, seasons = get_afl_season(), top_n = 10)
```

## Arguments

- team_name:

  A character string of the team's name (partial OK).

- seasons:

  Seasons to include for the record. Numeric vector of years, or `TRUE`
  for all available. Default is the current season.

- top_n:

  Number of top players to include (by current TORP). Default 10.

## Value

A list of class `torp_team_profile` with elements:

- team_info:

  1-row data.frame with name, full name, abbreviation.

- team_rating:

  Current TORP team rating (1-row data.frame, or empty).

- season_record:

  Per-season W/L/D record and percentage.

- top_players:

  Top players by current TORP rating.

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  team_profile("Sydney")
  team_profile("SYD", seasons = 2022:2025)
})
} # }
```
