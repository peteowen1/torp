# Calculate PSR with Offensive/Defensive Decomposition

Computes the margin-based PSR (the best single predictor of match
outcomes), then decomposes it into offensive (OSR) and defensive (DSR)
components using separately trained coefficient models. The
decomposition uses an additive shift so that `osr + dsr = psr` exactly.

## Usage

``` r
calculate_psr_components(
  skills,
  coef_df,
  osr_coef_df,
  dsr_coef_df,
  center = TRUE
)
```

## Arguments

- skills:

  A data.table/data.frame from
  [`load_player_stat_ratings()`](https://peteowen1.github.io/torp/reference/load_player_stat_ratings.md),
  containing `player_id`, `player_name`, `season`, `round`, `pos_group`,
  and `*_rating` columns.

- coef_df:

  A data.frame with columns `stat_name` and `beta`, as produced by the
  PSR training script. If an `sd` column is present, each stat rating is
  divided by its SD before multiplying by beta (i.e. the coefficients
  are on the standardized scale).

- osr_coef_df:

  Coefficient data.frame for the offensive model (same format as
  `coef_df`: columns `stat_name`, `beta`, optionally `sd`).

- dsr_coef_df:

  Coefficient data.frame for the defensive model.

- center:

  Logical. If TRUE (default), subtract the league mean so PSR =
  contribution above average player.

## Value

A data.table with columns: `player_id`, `player_name`, `season`,
`round`, `pos_group`, `psr_raw`, `psr`, `osr`, `dsr`.

## Details

The margin-PSR is the gold-standard total rating. OSR and DSR come from
models trained on points-scored and points-conceded respectively. Since
these are trained independently, `raw_osr + raw_dsr != psr` in general.
We reconcile by distributing the residual evenly:

\$\$\delta = (psr - raw\\osr - raw\\dsr) / 2\$\$ \$\$osr = raw\\osr +
\delta\$\$ \$\$dsr = raw\\dsr + \delta\$\$
