# Plot EP/WP game flow chart

Visualizes the expected points or win probability trajectory of a single
AFL match. The most common visualization in sports analytics.

## Usage

``` r
plot_ep_wp(
  season = get_afl_season(),
  round = NULL,
  match_id = NULL,
  metric = c("wp", "ep"),
  show_plays = TRUE,
  home_color = NULL,
  away_color = NULL,
  data = NULL
)
```

## Arguments

- season:

  Season year (default: current season).

- round:

  Round number. If NULL and `match_id` is NULL, uses latest round.

- match_id:

  Optional specific match ID. If NULL, the round must contain exactly
  one match or an error is raised with available matches.

- metric:

  One of `"wp"` (win probability, default) or `"ep"` (expected points).

- show_plays:

  Logical. If TRUE (default), overlay key play markers (goals).

- home_color:

  Override hex colour for home team.

- away_color:

  Override hex colour for away team.

- data:

  Optional pre-loaded EP/WP chart data. If NULL, loads via
  [`load_ep_wp_charts()`](https://peteowen1.github.io/torp/reference/load_ep_wp_charts.md).

## Value

A ggplot2 object.
