# Plot player rating trend over time

Shows per-game TORP (or component) ratings as points with a rolling
average smoothed line. Useful for tracking player form and development.

## Usage

``` r
plot_player_rating(
  player_name,
  seasons = TRUE,
  metric = c("torp_value", "epv", "psv"),
  rolling = 5,
  show_season_avg = TRUE,
  data = NULL
)
```

## Arguments

- player_name:

  Player name (partial match OK, resolved via
  [`resolve_player()`](https://peteowen1.github.io/torp/reference/resolve_player.md)).

- seasons:

  Seasons to include. Numeric vector or TRUE for all. Default TRUE.

- metric:

  One of `"torp_value"` (default), `"epv"`, `"psv"`. The per-game value
  column to plot.

- rolling:

  Rolling average window (number of games). Default 5.

- show_season_avg:

  Logical. If TRUE (default), show horizontal segments for each season's
  average.

- data:

  Optional pre-loaded data from
  [`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md).

## Value

A ggplot2 object.
