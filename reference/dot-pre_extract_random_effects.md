# Pre-extract all random effects from a GAM before stripping

Extracts coefficients and SEs for every `bs = "re"` smooth in the model,
storing them as a named list of data.tables keyed by variable name. This
allows
[`.strip_gam()`](https://peteowen1.github.io/torp/reference/dot-strip_gam.md)
to safely remove `$Vp` and `$model` while preserving the random effect
information needed by
[`extract_gam_random_effects()`](https://peteowen1.github.io/torp/reference/extract_gam_random_effects.md).

## Usage

``` r
.pre_extract_random_effects(model)
```

## Arguments

- model:

  A fitted gam/bam object

## Value

Named list of data.tables (one per random effect variable), or empty
list if extraction fails
