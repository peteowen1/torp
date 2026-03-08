# Build match-level features (home ground, familiarity, distance, days rest)

Build match-level features (home ground, familiarity, distance, days
rest)

## Usage

``` r
.build_match_features(fix_df, team_rt_df, all_grounds)
```

## Arguments

- fix_df:

  Pivoted fixture df from .build_fixtures_df()

- team_rt_df:

  Team ratings df from .build_team_ratings_df()

- all_grounds:

  Stadium reference data

## Value

Fixture df enriched with log_dist, familiarity, days_rest, and team
ratings
