# Get a Team Stat Rating Profile

Aggregates player stat ratings for a team and computes league-wide
percentile ranks across all 18 teams. The team equivalent of
[`player_stat_rating_profile()`](https://peteowen1.github.io/torp/reference/player_stat_rating_profile.md).

## Usage

``` r
team_stat_rating_profile(team_name, top_n = 22)

team_skill_profile(team_name, top_n = 22)
```

## Arguments

- team_name:

  A character string of the team's name (partial OK).

- top_n:

  Maximum number of players per team to include. Default 22.

## Value

A list of class `torp_team_stat_rating_profile` with elements:

- team_info:

  1-row data.frame with team name, full name, abbreviation.

- stat_ratings:

  Data.frame of stat rating estimates with league percentile ranks.

- n_players:

  Number of players included in the aggregation.

## See also

[`get_team_stat_ratings()`](https://peteowen1.github.io/torp/reference/get_team_stat_ratings.md),
[`player_stat_rating_profile()`](https://peteowen1.github.io/torp/reference/player_stat_rating_profile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  team_stat_rating_profile("Sydney")
  team_stat_rating_profile("Geelong")
})
} # }
```
