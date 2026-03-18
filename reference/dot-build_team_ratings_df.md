# Build team-level ratings from lineups

Joins lineups to EPR ratings, imputes missing with priors, applies TOG
weighting, generates all position columns, and aggregates to team level.

## Usage

``` r
.build_team_ratings_df(teams, torp_df, psr_df = NULL)
```

## Arguments

- teams:

  Raw teams/lineups from load_teams()

- torp_df:

  EPR ratings from load_torp_ratings()

## Value

Team-level aggregated ratings with position columns
