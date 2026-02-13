# Changelog

## torp 0.0.0.9002 (development)

### Code Quality

- Merged `logging_monitoring.R` and `safe_logging.R` into a single
  `logging.R` file for better organization.

- Fixed global state issues by replacing `<<-` operator with
  package-level environment for logging state.

- Improved AUC calculation efficiency using Mann-Whitney U-statistic
  (O(n log n) instead of O(n^2)).

- Fixed inefficient [`rbind()`](https://rdrr.io/r/base/cbind.html)
  patterns in
  [`compare_baseline_models()`](https://peteowen1.github.io/torp/reference/compare_baseline_models.md)
  and
  [`evaluate_baseline_models()`](https://peteowen1.github.io/torp/reference/evaluate_baseline_models.md)
  by using pre-allocated lists with
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).

- Fixed double fixture load in
  [`get_afl_week()`](https://peteowen1.github.io/torp/reference/get_afl_week.md) -
  now loads fixtures once and filters twice.

- Fixed
  [`mutate_all()`](https://dplyr.tidyverse.org/reference/mutate_all.html)
  performance issue in data validation using
  [`lapply()`](https://rdrr.io/r/base/lapply.html) for column-wise
  operations.

- Removed unused `match_id` parameter from
  [`match_xgs()`](https://peteowen1.github.io/torp/reference/calculate_match_xgs.md)
  function.

### New Features

- Added `R/constants.R` with centralized AFL and model constants:

- `AFL_GOAL_WIDTH`, `AFL_QUARTER_DURATION`, `AFL_TOTAL_GAME_SECONDS`

- `RATING_DECAY_DEFAULT_DAYS`, `SIM_NOISE_SD`, `SIM_WP_SCALING_FACTOR`

- Placeholder dashboard functions (`create_monitoring_dashboard_data()`,
  `get_model_health_status()`) now return informative empty structures
  instead of fake data.

### Documentation

- Added five vignettes: Getting Started, Player Ratings, Model Usage,
  Data Architecture, and Season Simulation.
- Added pkgdown site configuration with comprehensive reference
  sections.
- Improved README with lifecycle badge, ecosystem table, and torpmodels
  install instructions.

### Internal Changes

- Reduced exported functions - internalized helper functions:

  - `rds_from_url()`,
    [`file_reader()`](https://peteowen1.github.io/torp/reference/file_reader.md)
    (internal data loading helpers)
  - [`predict_wp_naive()`](https://peteowen1.github.io/torp/reference/predict_wp_naive.md),
    [`predict_wp_time_only()`](https://peteowen1.github.io/torp/reference/predict_wp_time_only.md)
    (baseline model internals)
  - [`log_prediction_event()`](https://peteowen1.github.io/torp/reference/log_prediction_event.md),
    [`log_data_quality()`](https://peteowen1.github.io/torp/reference/log_data_quality.md)
    (internal logging helpers)

- Moved manual test scripts to `tests/manual/` directory.

- Archived legacy data-raw scripts to `inst/extdata/archive/scripts/`.

- Enhanced test helper functions with additional mock data creators.

### Test Coverage

- Added tests for `helper_functions.R` internal functions.
- Added tests for `match_xg_functions.R` function signatures.
- Added tests for `sim-helpers.R` simulation functions.

------------------------------------------------------------------------

## torp 0.0.0.9001

- Initial development version.
- Core data loading functions for AFL play-by-play, chains, xG, and
  player stats.
- Expected points (EP) and win probability (WP) models.
- Player rating system (TORP).
- Match prediction framework.
