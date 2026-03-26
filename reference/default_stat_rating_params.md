# Default hyperparameters for stat rating estimation

Returns optimized defaults for the Bayesian stat rating estimation
pipeline. Per-category lambda and prior values come from
`data-raw/06-stat-ratings/02_optimize_skill_params.R`. The global
`lambda_rate` and `prior_games` are used as fallbacks for any category
not in `category_params`.

## Usage

``` r
default_stat_rating_params()

default_skill_params()
```

## Value

A named list with elements:

- lambda_rate:

  Fallback decay rate for rate stats (per day).

- lambda_efficiency:

  Decay rate for efficiency stats (per day).

- prior_games:

  Fallback prior pseudo-games for Gamma-Poisson rate stats.

- prior_attempts:

  Prior pseudo-attempts for Beta-Binomial efficiency stats.

- min_games:

  Minimum weighted games to appear in output.

- credible_level:

  Width of credible interval (e.g. 0.80 for 80 pct).

- category_params:

  Per-category lambda and prior_strength overrides.
