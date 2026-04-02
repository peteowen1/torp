# Extract random effects from a GAM model

Extracts coefficient values and standard errors for a random effect
smooth term from an mgcv GAM/BAM model. Recovers actual factor level
names from the model's stored training data (`model$model`).

## Usage

``` r
extract_gam_random_effects(model, variable)
```

## Arguments

- model:

  A fitted GAM/BAM model from mgcv.

- variable:

  Character string matching the variable name of the random effect
  smooth (matched via `grepl` against `s$vn`).

## Value

A data.table with columns `level`, `coefficient`, `se`, or NULL if the
smooth is not found.
