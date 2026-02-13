# Score-Only Baseline Model for Win Probability

This function is intended for internal use and may be unexported in a
future release. Simple logistic regression using only score difference

## Usage

``` r
predict_wp_score_only(data, pred_data = NULL)
```

## Arguments

- data:

  Data with 'points_diff' column (if only one parameter provided, uses
  internal training)

- pred_data:

  Optional - data to make predictions on

## Value

Vector of win probability predictions
