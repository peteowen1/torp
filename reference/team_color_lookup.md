# Look up a team colour with NA-safe fallback

Look up a team colour with NA-safe fallback

## Usage

``` r
team_color_lookup(team, default = "#808080", colors = AFL_TEAM_COLORS)
```

## Arguments

- team:

  Canonical team name.

- default:

  Fallback hex colour if team not found.

- colors:

  Named colour vector (default `AFL_TEAM_COLORS`).

## Value

A hex colour string.
