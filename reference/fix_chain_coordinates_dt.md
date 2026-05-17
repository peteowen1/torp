# Fix chain coordinate jumps (Pass 1.6)

Converts coordinates to pitch-relative (home-team) space, fixes
unreasonable jumps around throw-ins and ambiguous possession events,
then converts back to possession-team perspective.

## Usage

``` r
fix_chain_coordinates_dt(dt)
```

## Arguments

- dt:

  A data.table to modify

## Value

Invisible NULL (modifies dt by reference)

## Details

Known limitation: the iterative neighbour-based sign-flip routine in
steps C-F can cascade from a wrong anchor row when there's API noise
(duplicate / oscillating chain rows), wrongly flipping a chain of
correctly-positioned rows to match an outlier predecessor. Empirically
affects ~7% of shot_at_goal rows.
`R/clean_features.R::add_shot_geometry_variables` folds the resulting
bad `goal_x` to a near-goal distance as a downstream band-aid for
user-facing display, but the underlying `x` is still wrong for those
rows (consumed by EP/WP/shot models). A proper fix needs rework of the
correction algorithm AND retraining of the EP, WP, and shot models.
Tracked in internal project notes
(`project_clean_pbp_sign_flip_cascade`) and the corresponding GitHub
issue.
