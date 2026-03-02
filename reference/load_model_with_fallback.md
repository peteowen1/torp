# Load Model from torpmodels

Loads a model from the torpmodels package with in-memory caching.
Install torpmodels via
`devtools::install_github("peteowen1/torpmodels")`.

## Usage

``` r
load_model_with_fallback(model_name)
```

## Arguments

- model_name:

  Short model name: "ep", "wp", "shot", "match_gams", or "xgb_win"

## Value

The loaded model object.
