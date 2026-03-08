# Match Injuries to a Ratings Data Frame

Joins injury data to any data frame containing a `player_name` column
(e.g., TORP ratings). Matching uses normalized names for robustness.

## Usage

``` r
match_injuries(ratings_df, injuries_df)
```

## Arguments

- ratings_df:

  A data.frame with a `player_name` column.

- injuries_df:

  A data.frame from
  [`get_all_injuries()`](https://peteowen1.github.io/torp/reference/get_all_injuries.md)
  or
  [`scrape_injuries()`](https://peteowen1.github.io/torp/reference/scrape_injuries.md)
  with a `player_norm` column.

## Value

The input `ratings_df` with `injury` and `estimated_return` columns
added. Healthy players have NA values.
