# Bayesian shrinkage formula

Computes the posterior mean for a single EPR component:
`(loading * sum_val + prior_games * prior_rate) / (wt_gms + prior_games)`.

## Usage

``` r
.bayesian_shrink(sum_val, wt_gms, loading, prior_games, prior_rate)
```

## Arguments

- sum_val:

  Numeric vector of weighted sums.

- wt_gms:

  Numeric vector of weighted games.

- loading:

  Loading factor.

- prior_games:

  Prior games shrinkage strength.

- prior_rate:

  Prior rate (shrinkage target).

## Value

Numeric vector of posterior means.
