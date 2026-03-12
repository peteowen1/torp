# Get a Team Skill Profile

Aggregates player skills for a team and computes league-wide percentile
ranks across all 18 teams. The team equivalent of
[`player_skill_profile()`](https://peteowen1.github.io/torp/reference/player_skill_profile.md).

## Usage

``` r
team_skill_profile(team_name, top_n = 22)
```

## Arguments

- team_name:

  A character string of the team's name (partial OK).

- top_n:

  Maximum number of players per team to include. Default 22.

## Value

A list of class `torp_team_skill_profile` with elements:

- team_info:

  1-row data.frame with team name, full name, abbreviation.

- skills:

  Data.frame of skill estimates with league percentile ranks.

- n_players:

  Number of players included in the aggregation.

## See also

[`get_team_skills()`](https://peteowen1.github.io/torp/reference/get_team_skills.md),
[`player_skill_profile()`](https://peteowen1.github.io/torp/reference/player_skill_profile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  team_skill_profile("Sydney")
  team_skill_profile("Geelong")
})
} # }
```
