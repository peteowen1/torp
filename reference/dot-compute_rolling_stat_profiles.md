# Compute rolling causal team defensive profiles for stats

For each match, computes each team's decay-weighted defensive profile
using only games played strictly before that match (causal). This
ensures a 2021 game is adjusted by the opponent's 2021 profile, not
their 2026 one.

## Usage

``` r
.compute_rolling_stat_profiles(
  dt,
  lambda_decay,
  rate_sources,
  cap,
  prior_games = 5
)
```

## Arguments

- dt:

  data.table of player stats with team, opponent, match_id,
  match_date_rating, and stat columns.

- lambda_decay:

  Numeric. Decay rate per day.

- rate_sources:

  Named character vector: stat_name -\> source_col.

- cap:

  Numeric vector of length 2: floor/ceiling for adj_factor.

- prior_games:

  Numeric. Pseudo-games at league average for shrinkage.

## Value

data.table with columns: match_id, opponent, and `{stat}_adj_factor` for
each rate stat. One row per (match_id, opponent) pair.

## Details

Mirrors the approach in
[`.compute_rolling_epv_profiles()`](https://peteowen1.github.io/torp/reference/dot-compute_rolling_epv_profiles.md).
