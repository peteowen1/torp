# Visualization Guide

## Overview

torp includes 7 visualization functions that produce ggplot2 objects.
All plots use
[`theme_torp()`](https://peteowen1.github.io/torp/reference/theme_torp.md)
for consistent styling and `AFL_TEAM_COLORS` for team-specific
colouring. Since they return standard ggplot objects, you can further
customize with `+`.

``` r
library(torp)
```

## Game Flow Charts

The most common sports analytics visualization — shows how a game
unfolded play-by-play.

### Win Probability

``` r
# Win probability chart for a specific match
plot_ep_wp(2025, round = 1, match_id = "CD_M20250140101")

# Auto-selects match if only one in the round
plot_ep_wp(2025, round = 1)
```

### Expected Points

``` r
plot_ep_wp(2025, round = 1, match_id = "CD_M20250140101", metric = "ep")
```

## Team Ratings

Compare all 18 teams by their EPR rating (or component: recv, disp,
spoil, hitout).

``` r
# Overall team EPR
plot_team_ratings()

# Disposal-specific EPR
plot_team_ratings(metric = "disp")
```

## Shot Maps

Visualize shot locations on a half-field layout with expected goal
probability as colour.

``` r
# All shots in a round
plot_shot_map(2025, round = 1)

# Filter to a specific team
plot_shot_map(2025, round = 1, team = "Geelong")
```

Shapes indicate actual outcomes (circle = goal, triangle = behind, X =
miss) when `show_outcome = TRUE` (the default).

## Player Rating Trends

Track a player’s per-game TORP value over time with a rolling average.

``` r
# Default: torp_value with 5-game rolling average
plot_player_rating("Nick Daicos")

# EPV component, 10-game rolling
plot_player_rating("Nick Daicos", metric = "epv", rolling = 10)
```

## Player Comparison

Overlay 2-5 players on a single chart to compare form.

``` r
plot_player_comparison(
  c("Nick Daicos", "Marcus Bontempelli", "Lachie Neale"),
  rolling = 10
)
```

Lines are coloured by team colour when players are from different teams,
or by a distinct palette when team colours collide.

## Stat Rating Profiles

Visualize a player’s statistical strengths and weaknesses as percentile
ranks. See
[`vignette("stat-ratings")`](https://peteowen1.github.io/torp/articles/stat-ratings.md)
for details on how stat ratings are computed.

### Bar Chart (default)

``` r
profile <- player_stat_rating_profile("Nick Daicos")
plot_stat_rating_profile(profile)

# Filter to specific categories
plot_stat_rating_profile(profile, categories = c("disposal", "scoring"))
```

### Radar Chart

``` r
plot_stat_rating_profile(profile, type = "radar")
```

## Season Simulation

Visualize the output of
[`simulate_afl_season()`](https://peteowen1.github.io/torp/reference/simulate_afl_season.md).

### Ladder Probabilities

``` r
sim <- simulate_afl_season(n_sims = 1000)
plot_simulation(sim, type = "ladder")
```

### Position Heatmap

``` r
plot_simulation(sim, type = "position")
```

### Finals Probabilities

``` r
plot_simulation(sim, type = "finals")
```

## Customization

All functions return ggplot2 objects, so you can customize further:

``` r
plot_team_ratings() +
  ggplot2::labs(caption = "Data: torp package") +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
```

### Team Colours

Use the built-in colour constants and scales in your own plots:

``` r
# Named vectors
AFL_TEAM_COLORS["Adelaide Crows"]   # "#002B5C"
AFL_TEAM_COLORS2["Adelaide Crows"]  # "#E21937" (secondary)

# ggplot2 scales
ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, colour = team)) +
  team_color_scale()  # or team_fill_scale() for fill
```
