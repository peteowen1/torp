# Estimate player skills for multiple reference dates efficiently

Internal function that eliminates redundant work when estimating skills
across many dates: date conversion and avail_only setup are done once,
and the data is sorted so each iteration filters an ascending subset.

## Usage

``` r
.estimate_skills_batch(
  skill_data,
  ref_dates,
  params = NULL,
  stat_defs = NULL,
  compute_ci = FALSE
)
```

## Arguments

- skill_data:

  A data.table from
  [`prepare_skill_data()`](https://peteowen1.github.io/torp/reference/prepare_skill_data.md).

- ref_dates:

  Date vector of reference dates.

- params:

  Named list of hyperparameters from
  [`default_skill_params()`](https://peteowen1.github.io/torp/reference/default_skill_params.md).

- stat_defs:

  Output of
  [`skill_stat_definitions()`](https://peteowen1.github.io/torp/reference/skill_stat_definitions.md).
  If NULL, uses default.

- compute_ci:

  Logical. If FALSE (default), skip credible interval computation.

## Value

Named list of data.tables (keyed by ref_date as character), one per
date.
