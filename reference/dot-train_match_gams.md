# Train the 5-model sequential GAM pipeline

Trains total xPoints, xScore diff, conversion, score diff, and win
probability GAMs sequentially (each model's predictions feed the next).
Model 1 includes weather smooths (log_wind, log_precip, temp_avg,
humidity_avg).

## Usage

``` r
.train_match_gams(team_mdl_df, train_filter = NULL, nthreads = 4L)
```

## Arguments

- team_mdl_df:

  Complete model dataset from .build_team_mdl_df()

- train_filter:

  Logical vector indicating training rows (NULL = all completed matches)

- nthreads:

  Number of threads for mgcv::bam() (default 4)

## Value

List with \$models (named list of 5 GAMs) and \$data (team_mdl_df with
predictions)
