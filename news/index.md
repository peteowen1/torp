# Changelog

## torp 0.0.0.9003 (development)

### Breaking Changes

- [`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md)
  and
  [`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md)
  now default `rounds = TRUE` (all rounds) instead of the current week.
  Callers that relied on getting only the latest round without
  specifying `rounds` will now receive the full season. Use
  `rounds = get_afl_week()` to restore the old behaviour.

- [`load_from_url()`](https://peteowen1.github.io/torp/reference/load_from_url.md)
  now defaults `use_disk_cache = FALSE` (previously `TRUE`). Pass
  `use_disk_cache = TRUE` explicitly if you want persistent disk
  caching.

- `load_fixtures(all = TRUE)` now starts from 2021 (previously 2018).
  Seasons before 2021 are outside
  [`validate_seasons()`](https://peteowen1.github.io/torp/reference/validate_seasons.md)
  range and were producing errors.

- `check_internet_connection()` has been removed. Use
  [`curl::has_internet()`](https://jeroen.r-universe.dev/curl/reference/nslookup.html)
  directly.

### New Features

- **Local-first data loading**: All `load_*()` functions now check
  `torpdata/data/` first, then disk cache, then download. Downloaded
  files are auto-saved locally.

- [`download_torp_data()`](https://peteowen1.github.io/torp/reference/download_torp_data.md)
  for bulk-downloading parquet files for offline access.

- **Parallel downloads**: Multi-URL loads use
  [`curl::multi_download()`](https://jeroen.r-universe.dev/curl/reference/multi_download.html)
  for faster batch fetching.

- **Negative cache (skip markers)**: 404 URLs are marked with `.skip`
  files to avoid repeated failed downloads. Use
  [`clear_skip_markers()`](https://peteowen1.github.io/torp/reference/clear_skip_markers.md)
  to retry.

- **Column selection**: All `load_*()` functions accept a `columns`
  parameter to read only specific columns.

- New load functions:
  [`load_ep_wp_charts()`](https://peteowen1.github.io/torp/reference/load_ep_wp_charts.md),
  [`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md),
  [`load_player_season_ratings()`](https://peteowen1.github.io/torp/reference/load_player_season_ratings.md),
  [`load_team_ratings()`](https://peteowen1.github.io/torp/reference/load_team_ratings.md).

- `CREDIT_POS_ADJ_QUANTILE` split into 4 per-dimension constants:
  `CREDIT_POS_ADJ_QUANTILE_RECV`, `_DISP`, `_SPOIL`, `_HITOUT`.

### Bug Fixes

- [`parquet_from_urls_parallel()`](https://peteowen1.github.io/torp/reference/parquet_from_urls_parallel.md)
  now warns instead of silently dropping data when column selection
  finds no matching columns.

- [`parquet_from_urls_parallel()`](https://peteowen1.github.io/torp/reference/parquet_from_urls_parallel.md)
  now errors (instead of just warning) when downloads completely fail
  and no local data is available.

- [`mark_download_skippable()`](https://peteowen1.github.io/torp/reference/mark_download_skippable.md)
  now logs write errors instead of silently swallowing them.

- [`read_local_parquet()`](https://peteowen1.github.io/torp/reference/read_local_parquet.md)
  now only deletes files on likely corruption errors, not transient
  failures (memory, locking).

- [`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md)
  and
  [`load_team_ratings()`](https://peteowen1.github.io/torp/reference/load_team_ratings.md)
  now warn when returning empty data.

- [`load_from_url()`](https://peteowen1.github.io/torp/reference/load_from_url.md)
  now warns when round filtering is requested but no round column exists
  in the data.

- Integration test data loading is now guarded against CRAN
  environments.

### Optimized Parameters

- Re-optimized rating constants: `RATING_DECAY_DEFAULT_DAYS` (486→511),
  `RATING_PRIOR_GAMES_RECV` (5.87→6.19), `RATING_PRIOR_GAMES_DISP`
  (7.14→7.11), `RATING_PRIOR_GAMES_HITOUT` (3→4.44).

- Re-optimized credit assignment constants for disposal, reception, and
  position adjustment.

------------------------------------------------------------------------

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
