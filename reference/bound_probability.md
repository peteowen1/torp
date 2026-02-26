# Bound a probability to avoid log(0) and numeric edge cases

Bound a probability to avoid log(0) and numeric edge cases

## Usage

``` r
bound_probability(p, lower = 0.001, upper = 0.999)
```

## Arguments

- p:

  Numeric vector of probabilities

- lower:

  Lower bound (default 0.001)

- upper:

  Upper bound (default 0.999)

## Value

Numeric vector bounded to \[lower, upper\]
