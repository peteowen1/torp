# Vectorized Harmonic Mean of Two Numeric Vectors

Internal function. Computes the row-wise harmonic mean of two numeric
vectors. The harmonic mean is particularly useful in AFL analytics for
averaging rates and proportions, giving less weight to extreme values
than the arithmetic mean.

## Usage

``` r
harmonic_mean(x, y)
```

## Arguments

- x:

  Numeric vector (e.g. home_shots).

- y:

  Numeric vector (e.g. away_shots).

## Value

A numeric vector of harmonic means. Returns NA for pairs where either
value is 0.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate harmonic mean of shot attempts
home_shots <- c(10, 15, 20)
away_shots <- c(12, 18, 25)
harmonic_mean(home_shots, away_shots)

# Returns NA when one value is zero
harmonic_mean(c(10, 0, 20), c(15, 10, 25))
} # }
```
