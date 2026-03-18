# Load PSR Coefficient Files and Compute Components

Convenience wrapper that loads the margin, offensive, and defensive
coefficient CSVs from `inst/extdata` (or a fallback path) and calls
[`calculate_psr_components`](https://peteowen1.github.io/torp/reference/calculate_psr_components.md).

## Usage

``` r
.compute_psr_from_skills(skills, psr_coef_path = NULL, center = TRUE)
```

## Arguments

- skills:

  A data.table/data.frame from
  [`load_player_skills()`](https://peteowen1.github.io/torp/reference/load_player_skills.md),
  containing `player_id`, `player_name`, `season`, `round`, `pos_group`,
  and `*_skill` columns.

- psr_coef_path:

  Path to the margin PSR coefficient CSV. If NULL, searches
  `inst/extdata/psr_v2_coefficients.csv`.

- center:

  Logical. If TRUE (default), subtract the league mean so PSR =
  contribution above average player.

## Value

A data.table with `psr`, `osr`, `dsr` columns, or the result of
[`calculate_psr()`](https://peteowen1.github.io/torp/reference/calculate_psr.md)
(without osr/dsr) if the offensive/defensive coefficient files are not
found.
