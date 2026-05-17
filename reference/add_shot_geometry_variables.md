# Add Shot Geometry Variables

Computes shot `angle` and `distance` to the attacking goal (plus
intermediate trig legs `side_b` / `side_c`).

## Usage

``` r
add_shot_geometry_variables(df, goal_width)
```

## Arguments

- df:

  A dataframe containing shot data. Pass `venue_length` to enable the
  near-goal fold (recommended for any production caller). Without it,
  `goal_x` is used as-is, which assumes the caller has already mirrored
  coordinates to be positive – only suitable for synthetic test
  fixtures.

- goal_width:

  The width of the goal.

## Value

A dataframe with additional shot geometry variables.

## Details

BAND-AID: when `venue_length` is in `names(df)`, `goal_x` is folded to
the near-goal distance via `pmin(goal_x, venue_length - goal_x)` before
the geometry calculation. This is NOT a coordinate-frame fix – it works
around an upstream issue where `clean_pbp::fix_chain_coordinates_dt`
mis-flips a small fraction of shot_at_goal rows from positive to
negative `x` (the iterative neighbour-based sign-flip cascades from a
wrong anchor row when there's API noise in the chain). On 2026 R1 chain
data the mis-flip rate on shots was ~7% post-`clean_pbp` vs ~0.2% in raw
chains. Without the fold, those rows render as e.g. 126m behinds when
the truth is ~40m. The fold only corrects user-facing `distance` /
`angle`; the underlying `x` and `goal_x` columns remain wrong for those
rows, which is fine because the EP/WP/shot models were trained on the
same buggy `x` and are internally consistent with it. The full
investigation and path to a proper upstream fix (model retraining
required) is tracked in the internal
`project_clean_pbp_sign_flip_cascade` notes; root-cause file is
`R/clean_pbp.R::fix_chain_coordinates_dt`.

Side effect: genuine long-range shots from own half (real `x < 0` cases
per CLAUDE.md's team-relative coords; ~0.2% of shots in measured data)
also get folded to near-goal distance. Acceptable trade-off vs the
mis-flip case.
