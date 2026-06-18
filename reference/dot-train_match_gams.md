# Train the 5-model sequential GAM pipeline

Trains total xPoints, xScore diff, conversion, score diff, and win
probability GAMs sequentially (each model's predictions feed the next).
Model 1 includes weather smooths (log_wind, log_precip, temp_avg,
humidity_avg).

## Usage

``` r
.train_match_gams(
  team_mdl_df,
  train_filter = NULL,
  nthreads = 4L,
  gamma_arg = 1.4
)
```

## Arguments

- team_mdl_df:

  Complete model dataset from .build_team_mdl_df()

- train_filter:

  Logical vector indicating training rows (NULL = all completed matches)

- nthreads:

  Number of threads for mgcv::bam() (default 4)

- gamma_arg:

  Smoothness penalty multiplier passed to every mgcv::bam() call
  (default 1.4). 1.0 = fREML's own choice; \>1 forces smoother fits.
  Tuned via rolling-eval sweep on 2025+2026 (n=306) where 1.4 improved
  MAE (−0.44), Brier (−0.003), and bits (+4.1 total) over baseline 1.0
  with no regressions on either season — see
  data-raw/debug/gamma_full_pipeline_rolling.R.

## Value

List with \$models (named list of 5 GAMs) and \$data (team_mdl_df with
predictions)
