# Build the full match model dataset

Self-joins for opponent features, computes diffs, joins
results/xG/weather, adds log-transformed weather, and applies time-decay
weights.

## Usage

``` r
.build_team_mdl_df(
  team_rt_fix_df,
  results,
  xg_df,
  weather_df,
  weight_anchor_date
)
```

## Arguments

- team_rt_fix_df:

  Combined fixture + team ratings from .build_match_features()

- results:

  Match results from load_results()

- xg_df:

  Expected goals data from load_xg()

- weather_df:

  Weather data from .load_match_weather()

- weight_anchor_date:

  Date for time-decay weight anchoring

## Value

Complete model dataset ready for GAM training
