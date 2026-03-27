# Batch Player Attribution Across Matches

Runs zero-ablation attribution for multiple matches and aggregates
player-level contributions.

## Usage

``` r
batch_player_attribution(
  match_features_list,
  predict_fn,
  player_rating_cols,
  baseline_value = 0
)
```

## Arguments

- match_features_list:

  List of single-row data frames (one per match).

- predict_fn:

  Function. Prediction function.

- player_rating_cols:

  Character vector. Rating columns to ablate.

- baseline_value:

  Numeric. Ablation baseline. Default 0.

## Value

Data frame with per-feature average contributions across matches
