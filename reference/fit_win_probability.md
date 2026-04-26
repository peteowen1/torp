# Fit AFL Win Probability Model

Fits a logistic regression model for live win probability prediction.
The model estimates P(home_win \| margin, game_progress) using
historical AFL data with quarter-by-quarter scores.

## Usage

``` r
fit_win_probability(seasons = 2010:2025, output_path = NULL, seed = 20250101L)
```

## Arguments

- seasons:

  Numeric vector of seasons to include (default: 2010-2025)

- output_path:

  Path to write the JSON coefficients file (NULL = return only)

- seed:

  Integer seed used to make the synthetic quarter-break noise
  reproducible. The fitted coefficients are exported to JSON for browser
  inference; without a fixed seed each retraining produces a different
  model. Scoped via
  [`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  so the caller's RNG stream is not affected.

## Value

A list with: coefficients (named list), model (glm object), data
(training data)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- fit_win_probability()
# Coefficients for browser JS:
result$coefficients
} # }
```
