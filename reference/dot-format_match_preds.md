# Run weekly match predictions pipeline

Average home/away rows into one match-level prediction

## Usage

``` r
.format_match_preds(df)
```

## Arguments

- df:

  Long-form team_mdl_df with both home and away rows per match

## Value

One-row-per-match tibble with averaged predictions from the home team
perspective

## Details

Flips away-team predictions to the home-team perspective, then averages
home and (flipped) away predictions per match. Expects columns from
[`.train_match_gams()`](https://peteowen1.github.io/torp/reference/dot-train_match_gams.md)
output (team_mdl_df).
