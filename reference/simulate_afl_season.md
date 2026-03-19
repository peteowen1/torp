# Simulate a full AFL season

Runs multiple Monte Carlo simulations of the AFL regular season and
finals to estimate ladder positions, finals probabilities, and
premiership odds.

## Usage

``` r
simulate_afl_season(
  season,
  n_sims = SIM_DEFAULT_N,
  team_ratings = NULL,
  fixtures = NULL,
  predictions = NULL,
  injuries = NULL,
  seed = NULL,
  verbose = TRUE,
  keep_games = FALSE,
  n_cores = 1L
)
```

## Arguments

- season:

  Numeric season year (e.g. 2026).

- n_sims:

  Number of simulations to run (default
  [SIM_DEFAULT_N](https://peteowen1.github.io/torp/reference/SIM_DEFAULT_N.md)).

- team_ratings:

  Optional data.table with `team` and `torp` columns. If NULL, loads
  from torpdata.

- fixtures:

  Optional fixture data. If NULL, loads from torpdata.

- predictions:

  Optional predictions data with `pred_xtotal`.

- injuries:

  Injury data.frame from
  [`get_all_injuries()`](https://peteowen1.github.io/torp/reference/get_all_injuries.md).
  By default, fetched automatically from the AFL injury list. Pass
  `FALSE` to disable injury-aware simulation, or supply your own
  data.frame.

- seed:

  Optional random seed for reproducibility.

- verbose:

  Logical; if TRUE, shows a progress bar.

- keep_games:

  Logical; if TRUE, stores per-sim game results (memory intensive).
  Default FALSE.

- n_cores:

  Number of cores for parallel execution. Default 1 (sequential). Values
  \> 1 use
  [`parallel::parLapply()`](https://rdrr.io/r/parallel/clusterApply.html)
  with reproducible L'Ecuyer-CMRG random streams. Progress bars are not
  shown in parallel mode.

## Value

An S3 object of class `"torp_sim_results"` containing `season`,
`n_sims`, `ladders`, `finals`, `games` (if requested), and
`original_ratings`.

## Examples

``` r
if (FALSE) { # \dontrun{
try({
  results <- simulate_afl_season(2025, n_sims = 10, seed = 42)
  print(results)
})
} # }
```
