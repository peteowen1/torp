# Create Grouped Cross-Validation Folds

This function is intended for internal use and may be unexported in a
future release. Creates cross-validation folds ensuring matches stay
together to prevent data leakage

## Usage

``` r
create_grouped_cv_folds(data, group_var = "match_id", k = 5, seed = 42)
```

## Arguments

- data:

  A dataframe containing model data

- group_var:

  Character string specifying the grouping variable (default:
  "match_id")

- k:

  Number of folds (default: 5)

- seed:

  Random seed for reproducibility

## Value

List of fold indices
