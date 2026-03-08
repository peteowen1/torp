# Process games for a single round (data.table optimized)

Internal data.table implementation for processing games.

## Usage

``` r
process_games_dt(sim_teams, sim_games, round_num, injury_sd = SIM_INJURY_SD)
```

## Arguments

- sim_teams:

  A data.table containing team ratings.

- sim_games:

  A data.table containing fixture data.

- round_num:

  The round number to process.

## Value

A list containing updated sim_teams and sim_games data.tables.
