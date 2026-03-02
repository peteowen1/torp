# Aggregate player skills to team level

For each team in a lineup, sums and averages the skill estimates of the
players in that team. Used to create team-level features for match
prediction models.

## Usage

``` r
aggregate_team_skills(skills, team_lineups, top_n = 22)
```

## Arguments

- skills:

  A data.table from
  [`estimate_player_skills()`](https://peteowen1.github.io/torp/reference/estimate_player_skills.md).

- team_lineups:

  A data.table with columns `match_id`, `team`, and `player_id`
  identifying the lineup for each match-team combination.

- top_n:

  Maximum number of players per team to include. Default 22.

## Value

A data.table with one row per match-team, containing `match_id`, `team`,
and for each stat: `{stat}_team_sum` and `{stat}_team_mean`.
