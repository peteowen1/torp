# Extract player shooting skill from the shot model

Extracts per-player random effects from the shot GAM model, representing
each player's shooting ability relative to average (controlling for shot
location, play type, and position). Positive values = better than
average conversion.

## Usage

``` r
extract_player_xg_skill(shot_model = NULL)
```

## Arguments

- shot_model:

  Optional fitted shot GAM model. If NULL, loads from torpmodels via
  [`load_model_with_fallback()`](https://peteowen1.github.io/torp/reference/load_model_with_fallback.md).

## Value

A data.table with columns `player_id`, `player_name`, `xg_skill` (random
effect coefficient), `xg_skill_se` (standard error), and `n_shots`
(number of shots in training data, if available). Sorted by `xg_skill`
descending. Returns NULL if extraction fails.
