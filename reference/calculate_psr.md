# Calculate Player Skill Ratings (PSR)

Computes PSR for each player-round by applying glmnet coefficients to
individual player stat rating values. PSR represents each player's
predicted contribution to match margin based on their stat rating
profile.

## Usage

``` r
calculate_psr(skills, coef_df, center = TRUE)
```

## Arguments

- skills:

  A data.table/data.frame from
  [`load_player_stat_ratings()`](https://peteowen1.github.io/torp/reference/load_player_stat_ratings.md),
  containing `player_id`, `player_name`, `season`, `round`, `pos_group`,
  and `*_rating` columns.

- coef_df:

  A data.frame with columns `stat_name` and `beta`, as produced by the
  PSR training script. If an `sd` column is present, each stat rating is
  divided by its SD before multiplying by beta (i.e. the coefficients
  are on the standardized scale).

- center:

  Logical. If TRUE (default), subtract the league mean so PSR =
  contribution above average player.

## Value

A data.table with columns: `player_id`, `player_name`, `season`,
`round`, `pos_group`, `psr_raw`, `psr`.
