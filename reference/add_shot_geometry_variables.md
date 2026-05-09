# Add Shot Geometry Variables

Computes side_b, side_c, angle, and distance to the attacking goal.
Internally folds `goal_x` to the near-goal distance: a player at x = -30
on a 165m field has goal_x = halfLen - x = 112.5 (signed), but the
actual distance to the goal they're attacking is 52.5m. Pre-fold, shots
from negative-x reported distance to the FAR goal — visible on the blog
as e.g. a 126m behind that should have been 40m. The fold uses
venue_length when available; tests / legacy callers without venue_length
fall through to the raw goal_x (which assumes coords are already
pre-mirrored to be positive).

## Usage

``` r
add_shot_geometry_variables(df, goal_width)
```

## Arguments

- df:

  A dataframe containing shot data.

- goal_width:

  The width of the goal.

## Value

A dataframe with additional shot geometry variables.
