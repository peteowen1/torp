# Compute rolling team EPV defensive profiles

For each team at each match date, computes the decay-weighted average
total EPV that team concedes per game. Profiles are shrunk toward the
league mean via pseudo-games.

## Usage

``` r
.compute_rolling_epv_profiles(pgd_dt, lambda_decay, prior_games)
```

## Arguments

- pgd_dt:

  data.table of player_game_data with match_id, team, opponent,
  utc_start_time, epv_adj, time_on_ground_percentage columns.

- lambda_decay:

  Numeric. Decay rate per day for defensive profile recency weighting.

- prior_games:

  Numeric. Pseudo-games at league average to add for shrinkage (handles
  early-season small samples).

## Value

data.table with columns: match_id, team (the defending team),
epv_allowed (decay-weighted avg EPV conceded), league_avg_epv,
epv_opp_adj (additive adjustment to apply to opponents).
