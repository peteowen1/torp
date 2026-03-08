# Optimized per-stat hyperparameters

Baked-in results from `data-raw/06-skills/02_optimize_skill_params.R`.
Rate stats optimized via TOG-weighted MSE, efficiency stats via
attempt-weighted log-loss. Each entry has `lambda` (decay per day) and
`prior_strength`.

## Usage

``` r
.skill_stat_params()
```
