# Simulate a season of games

This function simulates a season of games based on team ratings and
fixture data.

`sim_season()` is deprecated; use `simulate_season()` instead.

## Usage

``` r
simulate_season(
  sim_teams,
  sim_games,
  return_teams = FALSE,
  injury_sd = SIM_INJURY_SD
)

sim_season(sim_teams, sim_games)
```

## Arguments

- sim_teams:

  A data frame containing team ratings.

- sim_games:

  A data frame containing fixture data.

- return_teams:

  Logical. If TRUE, return a list with both games and updated team
  ratings. Default FALSE returns just the games data.table for backward
  compatibility.

- injury_sd:

  Standard deviation for injury impact on team ratings. Default is
  `SIM_INJURY_SD`.

## Value

A data.table of simulated game results (default), or a list with `games`
and `teams` elements when `return_teams = TRUE`.
