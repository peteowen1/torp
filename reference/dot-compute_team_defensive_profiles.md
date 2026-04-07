# Compute team defensive profiles (stats conceded per game)

For each team and each rate stat, computes the decay-weighted average of
that stat conceded per game. Team profiles are shrunk toward the league
mean to handle small sample sizes.

## Usage

``` r
.compute_team_defensive_profiles(
  dt_played,
  ref_date,
  lambda_decay,
  rate_sources,
  cap,
  prior_games = 5
)
```

## Arguments

- dt_played:

  data.table of played matches (filtered to before ref_date).

- ref_date:

  Date. Reference date for decay computation.

- lambda_decay:

  Numeric. Decay rate per day.

- rate_sources:

  Named character vector: stat_name -\> source_col.

- cap:

  Numeric vector of length 2: floor/ceiling for adj_factor.

- prior_games:

  Numeric. Pseudo-games at league average for shrinkage.

## Value

data.table with one row per team, containing `{stat}_adj_factor`.
