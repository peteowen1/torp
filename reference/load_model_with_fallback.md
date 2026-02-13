# Load Model with Fallback

Attempts to load a model from torpmodels package first, then falls back
to package data if torpmodels is not available. Models are cached in
memory to avoid repeated loading.

## Usage

``` r
load_model_with_fallback(model_name)
```

## Arguments

- model_name:

  Short model name: "ep", "wp", "shot", or "xgb_win"

## Value

The loaded model object, or NULL if not available
