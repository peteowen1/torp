# Multiplier applied to team-quality residual SE before per-sim sampling. Widens season-long team uncertainty drawn from the xscore_diff GAM random effects. Values \> 1 make the ladder and finals distribution less confident (e.g. fewer 80%+ Premier probabilities, wider Top-N bands). Calibrated empirically — the raw GAM SE under-states true team uncertainty because random effects are shrunk toward the league mean.

Multiplier applied to team-quality residual SE before per-sim sampling.
Widens season-long team uncertainty drawn from the xscore_diff GAM
random effects. Values \> 1 make the ladder and finals distribution less
confident (e.g. fewer 80%+ Premier probabilities, wider Top-N bands).
Calibrated empirically — the raw GAM SE under-states true team
uncertainty because random effects are shrunk toward the league mean.

## Usage

``` r
SIM_RESIDUAL_SE_MULT
```

## Format

An object of class `numeric` of length 1.
