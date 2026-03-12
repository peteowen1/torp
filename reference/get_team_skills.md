# Get Team Skills

Aggregates player-level skill estimates to team level. For each team,
sums and averages the Bayesian skill estimates of its current players.

## Usage

``` r
get_team_skills(team_name = NULL, top_n = 22)
```

## Arguments

- team_name:

  Optional team name (partial OK). If NULL (default), returns all teams.

- top_n:

  Maximum number of players per team to include (ordered by total
  skill). Default 22 (a full team).

## Value

A data.table with one row per team, containing `team`, `n_players`, and
for each stat: `{stat}_sum` and `{stat}_mean`.

## See also

[`get_player_skills()`](https://peteowen1.github.io/torp/reference/get_player_skills.md),
[`aggregate_team_skills()`](https://peteowen1.github.io/torp/reference/aggregate_team_skills.md),
[`team_skill_profile()`](https://peteowen1.github.io/torp/reference/team_skill_profile.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({ # prevents cran errors
  get_team_skills()
  get_team_skills("Sydney")
})
} # }
```
