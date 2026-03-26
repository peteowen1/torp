# Normalise skills column names for backwards compatibility

Remaps legacy `_skill` suffixed columns to canonical `_rating` names.
When `strict = TRUE` (user-supplied data), aborts on missing columns.
When `strict = FALSE` (auto-loaded), warns and returns NULL.

## Usage

``` r
.normalise_skills_columns(skills, strict = FALSE)
```

## Arguments

- skills:

  A data.frame or NULL.

- strict:

  Logical. If TRUE, abort on missing required columns.

## Value

The normalised data.frame, or NULL if unusable.
