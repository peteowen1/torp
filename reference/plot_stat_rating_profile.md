# Plot stat rating profile as bar chart or radar

Visualizes a player's stat rating percentile ranks grouped by category.

## Usage

``` r
plot_stat_rating_profile(
  x,
  type = c("bar", "radar"),
  categories = NULL,
  top_n = 15,
  comparison = c("position", "league")
)
```

## Arguments

- x:

  A `torp_stat_rating_profile` object from
  [`player_stat_rating_profile()`](https://peteowen1.github.io/torp/reference/player_stat_rating_profile.md).

- type:

  One of `"bar"` (default) or `"radar"`.

- categories:

  Optional character vector of categories to include (e.g.
  `c("scoring", "disposal")`). NULL for all.

- top_n:

  For bar chart, number of stats to show (sorted by percentile).
  Default 15. Use NULL for all.

- comparison:

  One of `"position"` (default, uses `pos_pct`) or `"league"` (uses
  `league_pct`).

## Value

A ggplot2 object.
