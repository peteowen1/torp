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

## Details

When both `epr_df` and `psr_df` carry `season` and `round` columns,
PSR/OSR/DSR are joined per `(player_id, season, round)` so each
historical rating row receives the PSR as it stood at that point in
time. Rows with no matching historical PSR (e.g. the current round in
live computation, or seasons outside the PSR history) fall back to the
player's latest available PSR snapshot. When either frame lacks
season/round, the latest-per-player snapshot is used for all rows.
