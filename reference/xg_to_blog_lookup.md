# Convert xG release / live data into a blog-ready xscore lookup

Takes the match-level output of
[`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md) or
[`get_xg()`](https://peteowen1.github.io/torp/reference/get_xg.md) and
reshapes it into the
`(season, round, home_team, away_team, xscore_home, xscore_away)` shape
that
[`format_predictions_blog()`](https://peteowen1.github.io/torp/reference/format_predictions_blog.md)
expects as its `xg` argument.

## Usage

``` r
xg_to_blog_lookup(xg, season_val)
```

## Arguments

- xg:

  Output of
  [`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md)
  (single season) or
  [`get_xg()`](https://peteowen1.github.io/torp/reference/get_xg.md)
  (single round). Must contain `match_id`, `home_team`, `away_team`,
  `home_xscore`, `away_xscore`.

- season_val:

  Integer season to stamp onto every row —
  [`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md)
  does not emit a `season` column and match_id parsing alone would hide
  schema bugs if the caller mixes seasons by accident.

## Value

Tibble with canonical lookup columns, or `NULL` if `xg` is empty.

## Details

Team names are normalised via
[`torp_replace_teams()`](https://peteowen1.github.io/torp/reference/torp_replace_teams.md)
so that live
[`get_xg()`](https://peteowen1.github.io/torp/reference/get_xg.md)
output (raw `home_team_name`) joins cleanly against predictions that
already went through normalisation.
