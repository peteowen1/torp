# Plot multi-player rating comparison

Overlays per-game rating trends for 2-5 players on a single chart,
showing rolling averages for easy comparison.

## Usage

``` r
plot_player_comparison(
  player_names,
  seasons = TRUE,
  metric = c("torp_value", "epv", "psv"),
  rolling = 10,
  show_points = TRUE,
  data = NULL
)
```

## Arguments

- player_names:

  Character vector of 2-5 player names (partial match OK).

- seasons:

  Seasons to include. Numeric vector or TRUE for all. Default TRUE.

- metric:

  One of `"torp_value"` (default), `"epv"`, `"psv"`.

- rolling:

  Rolling average window (number of games). Default 10.

- show_points:

  Logical. If TRUE (default), show individual game points in addition to
  the rolling average line.

- data:

  Optional pre-loaded data from
  [`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md).

## Value

A ggplot2 object.
