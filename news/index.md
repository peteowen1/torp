# Changelog

## torp 1.3.2 (2026-04-18)

### Bug Fixes

- **Player position groups now derived from PBP playstyle, not lineup
  role** —
  [`.resolve_stat_rating_positions()`](https://peteowen1.github.io/torp/reference/dot-resolve_stat_rating_positions.md)
  prefers `position_group` (6-way PBP-derived classification) over
  `lineup_position` (20-way AFL API named role). Previously, players
  named in the forward pocket — e.g. tall forwards rotating through FPL
  — were classified as small/medium forwards based on their lineup, when
  their actual ball-contest behavior was key-position. Adds a
  teams-table fallback for fringe players who never registered a PBP
  `position_group`.

### Model Updates

- **Removed `bounces` from PSR feature set** — coefficient was
  non-causal (bouncers are ball carriers in transition, often correlated
  with losing teams rather than causing losses). Refit PSR/OSR/DSR
  coefficients on the reduced feature set. The defensive transition
  signal now concentrates correctly into `def_half_pressure_acts`.

### Internal

- [`.prepare_stat_rating_data()`](https://peteowen1.github.io/torp/reference/dot-prepare_stat_rating_data.md)
  and
  [`.resolve_stat_rating_positions()`](https://peteowen1.github.io/torp/reference/dot-resolve_stat_rating_positions.md)
  accept a new `teams` parameter for fallback `pos_group` assignment
  from `lineup_position` modal.

- PSR training script (`06_train_psr_model.R`) now filters leaderboard
  displays to `wt_80s >= 5` to suppress fringe low-sample players from
  top-N lists.

------------------------------------------------------------------------

## torp 1.3.0 (2026-04-02)

### New Features

- **Coordinate sign-flip correction** —
  [`fix_chain_coordinates_dt()`](https://peteowen1.github.io/torp/reference/fix_chain_coordinates_dt.md)
  detects and corrects AFL API sign-flipped x,y coordinates at
  possession changes. 8-step pipeline (throw-in fix, iterative
  sign-flip, both-neighbor confirmation, neighbor interpolation, paired
  flip) eliminates 99.7% of \>100m pitch-relative jumps. New constants
  `COORD_JUMP_THRESHOLD` (100m) and `COORD_FLIP_TOLERANCE` (70m).

- **Player xG skill extraction** —
  [`extract_player_xg_skill()`](https://peteowen1.github.io/torp/reference/extract_player_xg_skill.md)
  extracts per-player shooting ability from the shot GAM’s random
  effects. Returns player-level xG skill adjustments, standard errors,
  and shot counts.

- **Generic GAM random effect extractor** —
  [`extract_gam_random_effects()`](https://peteowen1.github.io/torp/reference/extract_gam_random_effects.md)
  extracts coefficients and SEs from any mgcv GAM random effect smooth,
  recovering actual factor level names from the model’s training data.

- **Team quality residuals in simulation** —
  [`simulate_afl_season()`](https://peteowen1.github.io/torp/reference/simulate_afl_season.md)
  now correctly displays team GAM residuals in the summary table (fixed
  factor level name extraction).

- **Win probability model fitting** —
  [`fit_win_probability()`](https://peteowen1.github.io/torp/reference/fit_win_probability.md)
  now exported for custom WP model training.

### Bug Fixes

- Fixed AFL API delivering coordinates in the wrong team’s frame for
  ~12% of PBP rows at possession changes (Spoils, Loose Ball Gets,
  Contested Marks, etc.).

- Fixed team residual extraction returning numeric indices instead of
  team names, causing all residuals to be zero in simulation output.

- Fixed parallel connection error in stat rating optimization
  (`02_optimize_stat_rating_params.R`) caused by stale file handles from
  prior pipeline phases exhausting R’s connection pool.

- Fixed non-ASCII character (`x` instead of `×`) in `win_probability.R`
  that caused R CMD check WARNING.

- Fixed integer truncation warnings in pitch-relative coordinate
  conversion by using
  [`as.double()`](https://rdrr.io/r/base/double.html).

### Model Updates

- Retrained EP model (128 rounds), WP model (50 rounds), shot GAM, and
  match prediction GAMs on coordinate-corrected data.

- Re-optimized all 56 stat rating hyperparameters with cleaner
  coordinate data.

### Documentation

- Added Coordinate System section to ARCHITECTURE.md documenting the
  8-step sign-flip fix pipeline with step-by-step table.

- Added Release Workflow section to CLAUDE.md (pre-PR checklist, version
  bumping, NEWS.md conventions).

------------------------------------------------------------------------

## torp 1.2.0 (2026-03-31)

### New Features

- **Player Stat Ratings system** — Bayesian estimation of 48 rate
  stats + 6 efficiency stats with positional priors and exponential
  decay. New exported functions:

  - [`estimate_player_stat_ratings()`](https://peteowen1.github.io/torp/reference/estimate_player_stat_ratings.md)
    — batch stat rating estimation
  - [`player_stat_rating_profile()`](https://peteowen1.github.io/torp/reference/player_stat_rating_profile.md)
    — per-player stat rating profiles with percentile ranks
  - [`get_player_stat_ratings()`](https://peteowen1.github.io/torp/reference/get_player_stat_ratings.md)
    — lookup stat ratings for a player
  - [`team_stat_rating_profile()`](https://peteowen1.github.io/torp/reference/team_stat_rating_profile.md)
    — team-aggregated stat rating profiles
  - [`get_team_stat_ratings()`](https://peteowen1.github.io/torp/reference/get_team_stat_ratings.md)
    — lookup team stat ratings
  - [`aggregate_team_stat_ratings()`](https://peteowen1.github.io/torp/reference/aggregate_team_stat_ratings.md)
    — aggregate player stat ratings to team level
  - [`stat_rating_definitions()`](https://peteowen1.github.io/torp/reference/stat_rating_definitions.md),
    [`default_stat_rating_params()`](https://peteowen1.github.io/torp/reference/default_stat_rating_params.md),
    [`stat_rating_position_map()`](https://peteowen1.github.io/torp/reference/stat_rating_position_map.md)
    — configuration helpers

- **Player Skill Rating (PSR)** — glmnet model mapping stat ratings to
  predicted margin contribution. New functions:
  [`calculate_psr()`](https://peteowen1.github.io/torp/reference/calculate_psr.md),
  [`calculate_psr_components()`](https://peteowen1.github.io/torp/reference/calculate_psr_components.md),
  [`calculate_psv()`](https://peteowen1.github.io/torp/reference/calculate_psv.md),
  [`calculate_psv_components()`](https://peteowen1.github.io/torp/reference/calculate_psv_components.md),
  [`psr_ratings()`](https://peteowen1.github.io/torp/reference/psr_ratings.md).

- **TORP blend** —
  [`torp_ratings()`](https://peteowen1.github.io/torp/reference/torp_ratings.md)
  now combines EPR (50%) + PSR (50%) for a complete player rating.
  Deprecates
  [`calculate_torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md).

- **Win Probability Added (WPA) credit** —
  [`create_wp_credit()`](https://peteowen1.github.io/torp/reference/create_wp_credit.md)
  allocates WPA between disposers and receivers.

- **Player attribution** —
  [`calculate_player_attribution()`](https://peteowen1.github.io/torp/reference/calculate_player_attribution.md)
  and
  [`batch_player_attribution()`](https://peteowen1.github.io/torp/reference/batch_player_attribution.md)
  for zero-ablation player impact measurement.

- **Network centrality** —
  [`calculate_player_centrality()`](https://peteowen1.github.io/torp/reference/calculate_player_centrality.md)
  for opponent quality adjustment.

- **Team profiles** —
  [`team_profile()`](https://peteowen1.github.io/torp/reference/team_profile.md),
  [`team_stat_rating_profile()`](https://peteowen1.github.io/torp/reference/team_stat_rating_profile.md),
  [`get_team_stat_ratings()`](https://peteowen1.github.io/torp/reference/get_team_stat_ratings.md)
  for team-level analysis.

- **Weather data loading** —
  [`load_weather()`](https://peteowen1.github.io/torp/reference/load_weather.md)
  for historical weather data from torpdata releases.

- **Injury scheduling** —
  [`build_injury_schedule()`](https://peteowen1.github.io/torp/reference/build_injury_schedule.md)
  and
  [`load_preseason_injuries()`](https://peteowen1.github.io/torp/reference/load_preseason_injuries.md)
  for simulation-aware injury management.

- **New load functions** —
  [`load_player_stat_ratings()`](https://peteowen1.github.io/torp/reference/load_player_stat_ratings.md),
  [`load_psr()`](https://peteowen1.github.io/torp/reference/load_psr.md),
  [`load_retrodictions()`](https://peteowen1.github.io/torp/reference/load_retrodictions.md).

### Documentation

- Updated README, vignettes, and pkgdown reference to use
  [`torp_ratings()`](https://peteowen1.github.io/torp/reference/torp_ratings.md)
  instead of deprecated
  [`calculate_torp()`](https://peteowen1.github.io/torp/reference/calculate_torp.md)
  /
  [`calculate_torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md).

- Expanded CLAUDE.md Key Files section to cover all 52 R files.

- Expanded ARCHITECTURE.md Code References table with 14 previously
  undocumented components.

- Added position codes and RAPM to ARCHITECTURE.md glossary.

- Added GitHub issue and PR templates.

------------------------------------------------------------------------

## torp 1.1.0 (2026-03-10)

### Breaking Changes

- **Standardised all column names to canonical `snake_case`** across the
  entire torp ecosystem. Old column names from multiple API schema
  versions (CFS camelCase, v2 dot-notation, ad-hoc abbreviations) are
  now normalised at load/fetch time via central column maps in
  `R/column_schema.R`. Key renames include:
  - Fixtures: `providerId` → `match_id`, `compSeason.year` → `season`,
    `round.roundNumber` → `round_number`, `home.score.totalScore` →
    `home_score`
  - PBP: `home_team_team_name` → `home_team_name`,
    `home_team_score_total_score` → `home_score`
  - Chains: `matchId` → `match_id`, `playerId` → `player_id`,
    `displayOrder` → `display_order`
  - Player stats: `extended_stats_spoils` → `spoils`,
    `clearances_total_clearances` → `clearances`
  - Player game data: `plyr_nm` → `player_name`, `tot_p` →
    `total_credits`, `recv_pts` → `recv_credits`, `tm` → `team`
- Old parquet files with legacy column names are normalised
  automatically at load time — no data regeneration required for
  backward compatibility.

### New Features

- **Central column schema infrastructure** (`R/column_schema.R`):
  per-data-type column maps (`FIXTURE_COL_MAP`, `PBP_COL_MAP`,
  `CHAINS_COL_MAP`, `PLAYER_STATS_COL_MAP`, `PLAYER_GAME_COL_MAP`,
  `TEAMS_COL_MAP`) and a generic
  [`.normalise_columns()`](https://peteowen1.github.io/torp/reference/dot-normalise_columns.md)
  function that remaps old names at load time.

### Bug Fixes

- Fixed dplyr data masking bug in
  [`filter_game_data()`](https://peteowen1.github.io/torp/reference/filter_game_data.md)
  where renaming column `tm` → `team` caused it to shadow the function
  parameter. Now uses `.env$` pronoun for disambiguation.

- Fixed
  [`get_afl_player_stats()`](https://peteowen1.github.io/torp/reference/get_afl_player_stats.md)
  returning `providerId` instead of `match_id` in output.

- Fixed
  [`detect_chains_columns()`](https://peteowen1.github.io/torp/reference/detect_chains_columns.md)
  silently returning wrong column name mappings when passed a plain
  data.frame (non-data.table) with camelCase columns.

------------------------------------------------------------------------

## torp 1.0.0 (2026-03-05)

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

- Unexported internal-use functions: `get_wp_model_info()`,
  [`check_wp_model_health()`](https://peteowen1.github.io/torp/reference/check_wp_model_health.md),
  [`harmonic_mean()`](https://peteowen1.github.io/torp/reference/harmonic_mean.md),
  [`norm_name()`](https://peteowen1.github.io/torp/reference/norm_name.md).
  These remain accessible via `torp:::`.

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

- Added `R/constants.R` with centralized AFL and model constants:

  - `AFL_GOAL_WIDTH`, `AFL_QUARTER_DURATION`, `AFL_TOTAL_GAME_SECONDS`
  - `RATING_DECAY_DEFAULT_DAYS`, `SIM_NOISE_SD`, `SIM_WP_SCALING_FACTOR`

- Placeholder dashboard functions (`create_monitoring_dashboard_data()`,
  `get_model_health_status()`) now return informative empty structures
  instead of fake data.

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

- Fixed double fixture load in
  [`get_afl_week()`](https://peteowen1.github.io/torp/reference/get_afl_week.md) -
  now loads fixtures once and filters twice.

### Optimized Parameters

- Re-optimized rating constants with per-component decay:
  `RATING_DECAY_RECV` (260), `RATING_DECAY_DISP` (700),
  `RATING_DECAY_SPOIL` (295), `RATING_DECAY_HITOUT` (700). Prior games:
  `RATING_PRIOR_GAMES_RECV` (12.56), `RATING_PRIOR_GAMES_DISP` (5.83),
  `RATING_PRIOR_GAMES_SPOIL` (3.00), `RATING_PRIOR_GAMES_HITOUT`
  (15.00).

- Re-optimized credit assignment constants for disposal, reception, and
  position adjustment.

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

- Fixed `mutate_all()` performance issue in data validation using
  [`lapply()`](https://rdrr.io/r/base/lapply.html) for column-wise
  operations.

- Removed unused `match_id` parameter from
  [`match_xgs()`](https://peteowen1.github.io/torp/reference/calculate_match_xgs.md)
  function.

- Replaced deprecated
  [`dplyr::group_by_all()`](https://dplyr.tidyverse.org/reference/group_by_all.html)
  with `dplyr::group_by(dplyr::across(dplyr::everything()))`.

### Documentation

- Added two vignettes: Getting Started and torp Reference Guide
  (consolidating ratings, models, data architecture, and simulation).

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
  - `get_wp_model_info()`,
    [`check_wp_model_health()`](https://peteowen1.github.io/torp/reference/check_wp_model_health.md)
    (model diagnostics)
  - [`harmonic_mean()`](https://peteowen1.github.io/torp/reference/harmonic_mean.md),
    [`norm_name()`](https://peteowen1.github.io/torp/reference/norm_name.md)
    (utility helpers)

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
