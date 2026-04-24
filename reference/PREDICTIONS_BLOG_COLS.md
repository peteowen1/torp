# Canonical blog predictions schema

Column order matters less than presence — OJS reads by name — but
keeping a canonical order makes `preds_blog[, PREDICTIONS_BLOG_COLS]`
style checks trivial and parquet schemas diff-clean across runs.

## Usage

``` r
PREDICTIONS_BLOG_COLS
```

## Format

An object of class `character` of length 14.
