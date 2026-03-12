# Resolve lineup match info for a team

Looks up the match_id from the lineup attribute and joins with fixture
data to get the opponent, round, and local datetime.

## Usage

``` r
.resolve_lineup_info(team_skills, team_name)
```

## Arguments

- team_skills:

  The result of
  [`get_team_skills()`](https://peteowen1.github.io/torp/reference/get_team_skills.md)
  (carries match_info attribute).

- team_name:

  Canonical team name.

## Value

A list with `match_id`, `opponent`, `round`, `datetime`, or NULL if
unavailable.
