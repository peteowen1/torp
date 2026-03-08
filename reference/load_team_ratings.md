# Load Team Ratings Data

Loads pre-computed team-level TORP aggregates from the [torpdata
repository](https://github.com/peteowen1/torpdata). Aggregates are the
sum of TORP ratings for each team's top-21 players (filtered to TORP \>
0) per round, with subcategory breakdowns. Player-level ratings are
already centered relative to average, so team sums are naturally
relative to 0.

## Usage

``` r
load_team_ratings(columns = NULL)
```

## Arguments

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing team-level ratings with columns including
`season`, `round`, `team`, `team_torp`, `team_recv`, `team_disp`,
`team_spoil`, `team_hitout`, `top_player`, `top_torp`, and `n_players`.

## See also

[`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md),
[`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  load_team_ratings()
})
} # }
```
