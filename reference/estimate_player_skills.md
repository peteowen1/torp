# Estimate player skills using Bayesian conjugate priors

For each player, estimates "true skill" at a reference date using all
prior matches. Uses conjugate Bayesian updating with exponential time
decay.

## Usage

``` r
estimate_player_skills(
  skill_data,
  ref_date = NULL,
  params = NULL,
  stat_defs = NULL,
  compute_ci = TRUE
)
```

## Arguments

- skill_data:

  A data.table from
  [`prepare_skill_data()`](https://peteowen1.github.io/torp/reference/prepare_skill_data.md).

- ref_date:

  Date to estimate skills as of. Only matches before this date are used.
  If NULL, includes all available matches (sets ref_date to one day
  after the latest match in the data).

- params:

  Named list of hyperparameters from
  [`default_skill_params()`](https://peteowen1.github.io/torp/reference/default_skill_params.md).

- stat_defs:

  Output of
  [`skill_stat_definitions()`](https://peteowen1.github.io/torp/reference/skill_stat_definitions.md).
  If NULL, uses default.

- compute_ci:

  Logical. If TRUE (default), compute credible intervals
  (`_lower`/`_upper` columns) using qgamma/qbeta. Set to FALSE to skip
  interval computation for faster batch processing.

## Value

A data.table with one row per player containing: `player_id`,
`player_name`, `pos_group`, `n_games`, `wt_games`, `ref_date`, and for
each stat: `{stat}_skill`, `{stat}_lower`, `{stat}_upper`.

## Details

**Rate stats (per-game):** Gamma-Poisson model. Raw event counts are
decay-weighted; the Gamma prior is centered on the position mean with
strength controlled by `prior_games`. Posterior mean gives the estimated
per-game rate (full-TOG adjusted).

**Efficiency stats (proportions):** Beta-Binomial model. Successes and
attempts are decay-weighted; the Beta prior is centered on the position
mean with strength controlled by `prior_attempts`.
