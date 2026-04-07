# Extract team-level quality residuals from match GAM

Extracts both cross-season `team_name` and within-season
`team_name_season` random effects from the `xscore_diff` GAM and
combines them. These capture systematic team over/under-performance not
explained by player TORP ratings (e.g. coaching, team system,
season-specific form). Team names are standardised via
[`torp_replace_teams()`](https://peteowen1.github.io/torp/reference/torp_replace_teams.md)
to ensure consistent merging with simulation team names.

## Usage

``` r
.extract_team_residuals(models = NULL, season = NULL)
```

## Arguments

- models:

  Optional named list of GAM models (as returned by
  `run_predictions_pipeline()$models`). When provided, uses the
  `xscore_diff` model directly. Otherwise loads from torpmodels.

- season:

  Integer season year used to filter `team_name_season` effects to the
  relevant season. Defaults to the current year.

## Value

A data.table with columns `team`, `residual_mean`, `residual_se`, or
NULL if extraction fails.
