# Prepare simulation data for a season

Loads fixtures, team ratings, and (optionally) predictions, then
separates played from unplayed games and formats everything for the
simulation loop.

## Usage

``` r
prepare_sim_data(
  season,
  team_ratings = NULL,
  fixtures = NULL,
  predictions = NULL,
  injuries = NULL,
  team_residuals = NULL,
  models = NULL
)
```

## Arguments

- season:

  Numeric season year (e.g. 2026).

- team_ratings:

  Optional data.table/data.frame with columns `team` and `torp`. If
  NULL, loads via
  [`load_team_ratings()`](https://peteowen1.github.io/torp/reference/load_team_ratings.md).

- fixtures:

  Optional fixture data.frame (AFL API format). If NULL, loads via
  [`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md).

- predictions:

  Optional predictions data.frame with `pred_xtotal`. If NULL, attempts
  to load via
  [`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md).

- injuries:

  Optional injury data.frame from
  [`get_all_injuries()`](https://peteowen1.github.io/torp/reference/get_all_injuries.md).
  When provided, team ratings are built from player-level TORP with
  injured players excluded and a lighter discount
  ([INJURY_KNOWN_DISCOUNT](https://peteowen1.github.io/torp/reference/INJURY_KNOWN_DISCOUNT.md))
  applied.

- team_residuals:

  Optional data.frame with columns `team`, `residual_mean`,
  `residual_se`. Team-level quality residuals from the match GAM random
  effects, capturing systematic over/under-performance not explained by
  player TORP. If `"auto"`, attempts to extract from the match GAM
  model.

- models:

  Optional named list of GAM models from
  `run_predictions_pipeline()$models`. Passed to
  [`.extract_team_residuals()`](https://peteowen1.github.io/torp/reference/dot-extract_team_residuals.md)
  to use fresh models instead of the stored torpmodels version.

## Value

A list with elements `sim_teams`, `sim_games`, `played_games`.
