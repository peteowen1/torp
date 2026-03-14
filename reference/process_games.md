# Process games for a single round

**\[deprecated\]**

Thin wrapper around
[`process_games_dt()`](https://peteowen1.github.io/torp/reference/process_games_dt.md)
that converts results back to data.frames. Use
[`process_games_dt()`](https://peteowen1.github.io/torp/reference/process_games_dt.md)
directly for better performance.

## Usage

``` r
process_games(sim_teams, sim_games, round_num, injury_sd = SIM_INJURY_SD)
```

## Arguments

- sim_teams:

  A data frame containing team ratings.

- sim_games:

  A data frame containing fixture data.

- round_num:

  The round number to process.

- injury_sd:

  Standard deviation for injury impact.

## Value

A list containing updated sim_teams and sim_games data frames.
