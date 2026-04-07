# Strip a GAM model to prediction-only components

Removes large components (model frame, residuals, fitted values, etc.)
that are not needed for predict.gam(). Typically shrinks models 5-10x.

## Usage

``` r
.strip_gam(model)
```

## Arguments

- model:

  A fitted gam/bam object

## Value

The same model with bulky diagnostic components removed
