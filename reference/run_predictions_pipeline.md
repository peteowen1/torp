# Run weekly match predictions pipeline

Builds team_mdl_df with injury-adjusted ratings, trains 5 sequential
GAMs, generates predictions for target weeks, and uploads to torpdata
releases.

## Usage

``` r
run_predictions_pipeline(week = NULL, weeks = NULL, season = NULL)
```

## Arguments

- week:

  Single target week (auto-detected if NULL)

- weeks:

  Vector of weeks, or "all" for all fixture weeks

- season:

  Season year (default: current via get_afl_season())

## Value

A list (invisibly) with:

- predictions:

  All match predictions across all seasons (season, round, providerId,
  home_team, away_team, pred_margin, pred_win, margin, etc.)

- models:

  Named list of 5 GAM models: total_xpoints, xscore_diff, conv_diff,
  score_diff, win
