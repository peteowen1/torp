# Plot team ratings comparison

Displays team-level TORP ratings as a horizontal bar chart with team
colours. Automatically uses the latest round from
[`load_team_ratings()`](https://peteowen1.github.io/torp/reference/load_team_ratings.md)
and maps column names (`team_epr` -\> `epr`, etc.).

## Usage

``` r
plot_team_ratings(
  team_ratings = NULL,
  metric = c("epr", "recv", "disp", "spoil", "hitout"),
  season = get_afl_season()
)
```

## Arguments

- team_ratings:

  Optional data frame of team ratings. If NULL (default), loads via
  [`load_team_ratings()`](https://peteowen1.github.io/torp/reference/load_team_ratings.md)
  and filters to the latest round.

- metric:

  One of `"epr"` (default), `"recv"`, `"disp"`, `"spoil"`, or
  `"hitout"`. Mapped to `team_epr`, `team_recv`, etc.

- season:

  Season year for title. Default: current season.

## Value

A ggplot2 object.
