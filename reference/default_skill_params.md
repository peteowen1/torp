# Default hyperparameters for skill estimation

Returns sensible defaults for the Bayesian skill estimation pipeline.
These can be overridden by optimized values from
`data-raw/06-skills/02_optimize_params.R`.

## Usage

``` r
default_skill_params()
```

## Value

A named list with elements:

- lambda_rate:

  Exponential decay rate for rate stats (per day). Default 0.0019 gives
  about 365-day half-life.

- lambda_efficiency:

  Decay rate for efficiency stats. Default 0.0013 gives about 533-day
  half-life.

- prior_games:

  Prior pseudo-games for Gamma-Poisson rate stats.

- prior_attempts:

  Prior pseudo-attempts for Beta-Binomial efficiency stats.

- min_games:

  Minimum weighted games to appear in output.

- credible_level:

  Width of credible interval (e.g. 0.80 for 80 pct).
