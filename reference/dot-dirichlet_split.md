# Split totals into quarters via Dirichlet distribution

Uses the gamma-distribution trick: draw k independent Gamma(alpha_k, 1)
samples, normalize to get Dirichlet fractions, then distribute total
score.

## Usage

``` r
.dirichlet_split(totals, alpha, n_sims)
```

## Arguments

- totals:

  Numeric vector of length n_sims with total scores.

- alpha:

  Numeric vector of Dirichlet concentration parameters (length 4).

- n_sims:

  Integer number of simulations.

## Value

Integer matrix (n_sims x 4) of quarter scores.
