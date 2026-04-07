# Simulate scoring sequences for N matches

For each simulation: draw margin and total, split into quarters via
Dirichlet, then simulate individual scoring events (goals/behinds)
within each quarter using calibrated Poisson shot counts and Bernoulli
outcomes.

## Usage

``` r
.sim_scoring_sequence(estimate, pred_total, home_conv, away_conv, n_sims)
```
