# Process games for a single round

This function processes games for a single round of the season. Uses
optimized data.table implementation internally.

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

## Value

A list containing updated sim_teams and sim_games data frames.
