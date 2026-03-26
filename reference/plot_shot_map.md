# Plot shot map for a match or team

Displays shot locations on a half-field layout with colour indicating
expected goal probability (xG). Shots are positioned by their field
coordinates (`goal_x` for distance to goal, `y` for lateral position).

## Usage

``` r
plot_shot_map(
  season = get_afl_season(),
  round = NULL,
  match_id = NULL,
  team = NULL,
  show_outcome = TRUE,
  data = NULL
)
```

## Arguments

- season:

  Season year (default: current season).

- round:

  Round number. Required if `match_id` is NULL.

- match_id:

  Optional specific match ID. If NULL, uses `season` + `round`.

- team:

  Optional team name to filter to (shows only that team's shots).

- show_outcome:

  Logical. If TRUE (default), shapes indicate actual outcome
  (goal/behind/miss). If FALSE, all shots shown as circles.

- data:

  Optional pre-loaded PBP data with shot columns (`goal_x`, `y`,
  `goal_prob`, `shot_row`, `points_shot`). If NULL, loads via
  [`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md).

## Value

A ggplot2 object.
