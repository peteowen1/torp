# Process games for a single round (legacy dplyr version)

Original dplyr-based implementation for backwards compatibility testing.

## Usage

``` r
process_games_dplyr(sim_teams, sim_games, round_num)
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
