# Resolve path to PSR coefficient CSV

Checks inst/extdata first, then falls back to
data-raw/cache-stat-ratings/.

## Usage

``` r
.find_psr_coef_path(coef_file = "psr_coefficients.csv")
```

## Arguments

- coef_file:

  Filename (default "psr_coefficients.csv")

## Value

Absolute path to the CSV, or "" if not found
