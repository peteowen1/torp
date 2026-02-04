# torp (development version)

## Code Quality Improvements

* Merged `logging_monitoring.R` and `safe_logging.R` into a single `logging.R` file for better organization.

* Fixed global state issues by replacing `<<-` operator with package-level environment for logging state.

* Improved AUC calculation efficiency using Mann-Whitney U-statistic (O(n log n) instead of O(n^2)).

* Fixed inefficient `rbind()` patterns in `compare_baseline_models()` and `evaluate_baseline_models()` by using pre-allocated lists with `dplyr::bind_rows()`.

* Fixed double fixture load in `get_afl_week()` - now loads fixtures once and filters twice.

* Fixed `mutate_all()` performance issue in data validation using `lapply()` for column-wise operations.

* Removed unused `match_id` parameter from `match_xgs()` function.

## New Features

* Added `R/constants.R` with centralized AFL and model constants:
 - `AFL_GOAL_WIDTH`, `AFL_QUARTER_DURATION`, `AFL_TOTAL_GAME_SECONDS`
 - `RATING_DECAY_DEFAULT_DAYS`, `SIM_NOISE_SD`, `SIM_WP_SCALING_FACTOR`

* Placeholder dashboard functions (`create_monitoring_dashboard_data()`, `get_model_health_status()`) now return informative empty structures instead of fake data.

## Documentation

* Added comprehensive "Getting Started" vignette with installation instructions, data loading examples, and configuration options.

## Internal Changes

* Reduced exported functions - internalized helper functions:
  - `rds_from_url()`, `file_reader()` (internal data loading helpers)
  - `predict_wp_naive()`, `predict_wp_time_only()` (baseline model internals)
  - `log_prediction_event()`, `log_data_quality()` (internal logging helpers)

* Moved manual test scripts to `tests/manual/` directory.

* Archived legacy data-raw scripts to `inst/extdata/archive/scripts/`.

* Enhanced test helper functions with additional mock data creators.

## Test Coverage

* Added tests for `helper_functions.R` internal functions.
* Added tests for `match_xg_functions.R` function signatures.
* Added tests for `sim-helpers.R` simulation functions.

---

# torp 0.0.0.9001

* Initial development version.
* Core data loading functions for AFL play-by-play, chains, xG, and player stats.
* Expected points (EP) and win probability (WP) models.
* Player rating system (TORP).
* Match prediction framework.
