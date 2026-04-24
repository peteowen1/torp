# Format match predictions for the blog / R2 parquet

Single source of truth for the `predictions.parquet` schema consumed by
the inthegame-blog `afl/matches.qmd` page. Called by both the CI
pipeline (`torpdata/scripts/build_blog_data.R`) and the local
convenience pusher (`torp/data-raw/02-models/push_predictions_to_r2.R`)
so schema drift between the two producers is impossible.

## Usage

``` r
format_predictions_blog(preds, xg = NULL)
```

## Arguments

- preds:

  Data frame with the core blog columns: `season`, `round`, `home_team`,
  `away_team`, `home_epr`, `away_epr`, `pred_margin`, `home_win_prob`,
  `pred_total`, `actual_margin`, `start_time`, `venue`.

- xg:

  Optional lookup with columns `season`, `round`, `home_team`,
  `away_team`, `xscore_home`, `xscore_away`. Use
  [`xg_to_blog_lookup()`](https://peteowen1.github.io/torp/reference/xg_to_blog_lookup.md)
  to build this from
  [`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md)
  or [`get_xg()`](https://peteowen1.github.io/torp/reference/get_xg.md)
  output. If `NULL`, xscore columns are written as `NA_real_` so the
  blog reader never hits `undefined`.

## Value

Tibble with exactly the columns in `PREDICTIONS_BLOG_COLS`, in canonical
order. Fails loudly if input is missing any required column.
