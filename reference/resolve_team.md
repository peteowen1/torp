# Resolve a team name to its canonical form

Internal helper that takes a team name string (partial match OK) and
resolves it to a canonical AFL team name using
[AFL_TEAM_ALIASES](https://peteowen1.github.io/torp/reference/AFL_TEAM_ALIASES.md).

## Usage

``` r
resolve_team(team_name)
```

## Arguments

- team_name:

  A character string of the team's name (full, short, or abbreviation).

## Value

A list with elements `name`, `full`, `abbr`.
