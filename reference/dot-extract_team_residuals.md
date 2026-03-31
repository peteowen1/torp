# Extract team-level quality residuals from match GAM

Extracts the cross-season `team_name.x` random effects from the
`xscore_diff` GAM. These capture systematic team over/under-performance
not explained by player TORP ratings (e.g. coaching, team system). Team
names are standardised via
[`torp_replace_teams()`](https://peteowen1.github.io/torp/reference/torp_replace_teams.md)
to ensure consistent merging with simulation team names.

## Usage

``` r
.extract_team_residuals(models = NULL)
```

## Arguments

- models:

  Optional named list of GAM models (as returned by
  `run_predictions_pipeline()$models`). When provided, uses the
  `xscore_diff` model directly. Otherwise loads from torpmodels.

## Value

A data.table with columns `team`, `residual_mean`, `residual_se`, or
NULL if extraction fails.
