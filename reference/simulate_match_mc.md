# Monte Carlo match simulation

Simulates a single AFL match N times, producing score distributions,
quarter-by-quarter breakdowns, individual scoring events, and a win
probability fan chart showing how WP could evolve during the match.

## Usage

``` r
simulate_match_mc(
  home_team,
  away_team,
  season = get_afl_season(),
  n_sims = MATCH_SIM_DEFAULT_N,
  team_ratings = NULL,
  predictions = NULL,
  home_advantage = SIM_HOME_ADVANTAGE,
  seed = NULL
)
```

## Arguments

- home_team:

  Character. Home team name (matched via AFL_TEAM_ALIASES).

- away_team:

  Character. Away team name.

- season:

  Numeric. Season year (default: current via get_afl_season()).

- n_sims:

  Integer. Number of Monte Carlo simulations (default 10,000).

- team_ratings:

  Optional data frame with `team` and `torp` columns. If NULL, loaded
  via
  [`load_team_ratings()`](https://peteowen1.github.io/torp/reference/load_team_ratings.md).

- predictions:

  Optional data frame of match predictions with `home_team`,
  `away_team`, `pred_xtotal`, `pred_margin`, `pred_win`. If NULL, loaded
  via
  [`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md).

- home_advantage:

  Numeric. Home ground advantage in points.

- seed:

  Optional integer seed for reproducibility.

## Value

A `torp_match_sim` S3 object with components:

- home_team:

  Home team name

- away_team:

  Away team name

- n_sims:

  Number of simulations run

- estimate:

  Pre-game expected margin (home perspective)

- pred_total:

  Pre-game expected combined score

- pre_game_wp:

  Pre-game win probability

- scores:

  data.table: sim_id, home_score, away_score, margin

- quarters:

  data.table: sim_id, quarter, home_qtr_score, away_qtr_score

- events:

  data.table: sim_id, quarter, event_num, time_pct, team, score_type,
  score_value, cum_home, cum_away

- wp_trajectory:

  data.table: time_pct, wp_mean, wp_p10, wp_p25, wp_median, wp_p75,
  wp_p90

- summary:

  List of summary statistics
