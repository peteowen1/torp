# Add contest variables (Pass 2b)

Collapses aerial contest information from adjacent rows onto the
preceding Kick row. Contest target rows ("Contest Target", "Kick Inside
50 Result") and their outcome rows (Spoil, Mark, etc.) will be filtered
out downstream by
[`clean_model_data_epv_dt()`](https://peteowen1.github.io/torp/reference/clean_model_data_epv_dt.md),
but their information is preserved on the Kick row via these columns.

## Usage

``` r
add_contest_vars_dt(dt)
```

## Arguments

- dt:

  A data.table to modify (sorted by match_id, display_order)

## Value

Invisible NULL (modifies dt by reference)
