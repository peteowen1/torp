# Calculate Player Attribution for a Match

Measures how much each player's ratings contribute to the match
prediction by comparing full prediction vs prediction with that player's
ratings set to league average (zero-ablation).

## Usage

``` r
calculate_player_attribution(
  match_features,
  predict_fn,
  player_rating_cols,
  baseline_value = 0
)
```

## Arguments

- match_features:

  Data frame. One row of match features including player-level rating
  aggregations (e.g., from `aggregate_lineup_ratings()`).

- predict_fn:

  Function. Takes a feature data frame and returns a prediction (e.g.,
  win probability or expected goals).

- player_rating_cols:

  Character vector. Column names containing player-aggregated ratings to
  ablate.

- baseline_value:

  Numeric. Value to use when ablating (default 0, representing league
  average for centered ratings).

## Value

Data frame with columns:

- `feature`: Rating column name

- `full_pred`: Prediction with all features

- `ablated_pred`: Prediction with this feature zeroed

- `contribution`: `full_pred - ablated_pred` (positive = helps team)

- `pct_contribution`: Contribution as percentage of total

## Examples

``` r
if (FALSE) { # \dontrun{
features <- data.frame(
  home_panna_sum = 5.2, away_panna_sum = 3.1,
  home_offense_mean = 0.8, away_offense_mean = 0.5,
  elo_diff = 50
)
predict_fn <- function(x) 1 / (1 + exp(-x$elo_diff / 100))
rating_cols <- c("home_panna_sum", "away_panna_sum",
                 "home_offense_mean", "away_offense_mean")
attr <- calculate_player_attribution(features, predict_fn, rating_cols)
} # }
```
