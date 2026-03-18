# Calculate TORP (Total Over Replacement Predictive-value)

Blends EPR (Expected Possession Rating) with PSR (Player Skill Rating)
to produce the final TORP rating. TORP = w \* epr + (1 - w) \* psr.

## Usage

``` r
calculate_torp(epr_df, psr_df, epr_weight = TORP_EPR_WEIGHT)
```

## Arguments

- epr_df:

  Data frame with EPR ratings (must contain `player_id`, `epr`).

- psr_df:

  Data frame with PSR ratings (must contain `player_id`, `psr`).

- epr_weight:

  Weight for EPR in the blend. Default is `TORP_EPR_WEIGHT` (0.5).

## Value

A data frame with all EPR columns plus `psr` and `torp` (blended).
