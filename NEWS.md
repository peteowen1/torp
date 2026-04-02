# torp 1.3.0 (2026-04-02)

## New Features

* **Coordinate sign-flip correction** — `fix_chain_coordinates_dt()` detects and corrects AFL API sign-flipped x,y coordinates at possession changes. 8-step pipeline (throw-in fix, iterative sign-flip, both-neighbor confirmation, neighbor interpolation, paired flip) eliminates 99.7% of >100m pitch-relative jumps. New constants `COORD_JUMP_THRESHOLD` (100m) and `COORD_FLIP_TOLERANCE` (70m).

* **Player xG skill extraction** — `extract_player_xg_skill()` extracts per-player shooting ability from the shot GAM's random effects. Returns player-level xG skill adjustments, standard errors, and shot counts.

* **Generic GAM random effect extractor** — `extract_gam_random_effects()` extracts coefficients and SEs from any mgcv GAM random effect smooth, recovering actual factor level names from the model's training data.

* **Team quality residuals in simulation** — `simulate_afl_season()` now correctly displays team GAM residuals in the summary table (fixed factor level name extraction).

* **Win probability model fitting** — `fit_win_probability()` now exported for custom WP model training.

## Bug Fixes

* Fixed AFL API delivering coordinates in the wrong team's frame for ~12% of PBP rows at possession changes (Spoils, Loose Ball Gets, Contested Marks, etc.).

* Fixed team residual extraction returning numeric indices instead of team names, causing all residuals to be zero in simulation output.

* Fixed parallel connection error in stat rating optimization (`02_optimize_stat_rating_params.R`) caused by stale file handles from prior pipeline phases exhausting R's connection pool.

* Fixed non-ASCII character (`x` instead of `×`) in `win_probability.R` that caused R CMD check WARNING.

* Fixed integer truncation warnings in pitch-relative coordinate conversion by using `as.double()`.

## Model Updates

* Retrained EP model (128 rounds), WP model (50 rounds), shot GAM, and match prediction GAMs on coordinate-corrected data.

* Re-optimized all 56 stat rating hyperparameters with cleaner coordinate data.

## Documentation

* Added Coordinate System section to ARCHITECTURE.md documenting the 8-step sign-flip fix pipeline with step-by-step table.

* Added Release Workflow section to CLAUDE.md (pre-PR checklist, version bumping, NEWS.md conventions).

---

# torp 1.2.0 (2026-03-31)

## New Features

* **Player Stat Ratings system** — Bayesian estimation of 48 rate stats + 6 efficiency stats with positional priors and exponential decay. New exported functions:
  - `estimate_player_stat_ratings()` — batch stat rating estimation
  - `player_stat_rating_profile()` — per-player stat rating profiles with percentile ranks
  - `get_player_stat_ratings()` — lookup stat ratings for a player
  - `team_stat_rating_profile()` — team-aggregated stat rating profiles
  - `get_team_stat_ratings()` — lookup team stat ratings
  - `aggregate_team_stat_ratings()` — aggregate player stat ratings to team level
  - `stat_rating_definitions()`, `default_stat_rating_params()`, `stat_rating_position_map()` — configuration helpers

* **Player Skill Rating (PSR)** — glmnet model mapping stat ratings to predicted margin contribution. New functions: `calculate_psr()`, `calculate_psr_components()`, `calculate_psv()`, `calculate_psv_components()`, `psr_ratings()`.

* **TORP blend** — `torp_ratings()` now combines EPR (50%) + PSR (50%) for a complete player rating. Deprecates `calculate_torp_ratings()`.

* **Win Probability Added (WPA) credit** — `create_wp_credit()` allocates WPA between disposers and receivers.

* **Player attribution** — `calculate_player_attribution()` and `batch_player_attribution()` for zero-ablation player impact measurement.

* **Network centrality** — `calculate_player_centrality()` for opponent quality adjustment.

* **Team profiles** — `team_profile()`, `team_stat_rating_profile()`, `get_team_stat_ratings()` for team-level analysis.

* **Weather data loading** — `load_weather()` for historical weather data from torpdata releases.

* **Injury scheduling** — `build_injury_schedule()` and `load_preseason_injuries()` for simulation-aware injury management.

* **New load functions** — `load_player_stat_ratings()`, `load_psr()`, `load_retrodictions()`.

## Documentation

* Updated README, vignettes, and pkgdown reference to use `torp_ratings()` instead of deprecated `calculate_torp()` / `calculate_torp_ratings()`.

* Expanded CLAUDE.md Key Files section to cover all 52 R files.

* Expanded ARCHITECTURE.md Code References table with 14 previously undocumented components.

* Added position codes and RAPM to ARCHITECTURE.md glossary.

* Added GitHub issue and PR templates.

---

# torp 1.1.0 (2026-03-10)

## Breaking Changes

* **Standardised all column names to canonical `snake_case`** across the entire torp ecosystem. Old column names from multiple API schema versions (CFS camelCase, v2 dot-notation, ad-hoc abbreviations) are now normalised at load/fetch time via central column maps in `R/column_schema.R`. Key renames include:
  - Fixtures: `providerId` → `match_id`, `compSeason.year` → `season`, `round.roundNumber` → `round_number`, `home.score.totalScore` → `home_score`
  - PBP: `home_team_team_name` → `home_team_name`, `home_team_score_total_score` → `home_score`
  - Chains: `matchId` → `match_id`, `playerId` → `player_id`, `displayOrder` → `display_order`
  - Player stats: `extended_stats_spoils` → `spoils`, `clearances_total_clearances` → `clearances`
  - Player game data: `plyr_nm` → `player_name`, `tot_p` → `total_credits`, `recv_pts` → `recv_credits`, `tm` → `team`

* Old parquet files with legacy column names are normalised automatically at load time — no data regeneration required for backward compatibility.

## New Features

* **Central column schema infrastructure** (`R/column_schema.R`): per-data-type column maps (`FIXTURE_COL_MAP`, `PBP_COL_MAP`, `CHAINS_COL_MAP`, `PLAYER_STATS_COL_MAP`, `PLAYER_GAME_COL_MAP`, `TEAMS_COL_MAP`) and a generic `.normalise_columns()` function that remaps old names at load time.

## Bug Fixes

* Fixed dplyr data masking bug in `filter_game_data()` where renaming column `tm` → `team` caused it to shadow the function parameter. Now uses `.env$` pronoun for disambiguation.

* Fixed `get_afl_player_stats()` returning `providerId` instead of `match_id` in output.

* Fixed `detect_chains_columns()` silently returning wrong column name mappings when passed a plain data.frame (non-data.table) with camelCase columns.

---

# torp 1.0.0 (2026-03-05)

## Breaking Changes

* `load_pbp()` and `load_chains()` now default `rounds = TRUE` (all rounds) instead of the current week. Callers that relied on getting only the latest round without specifying `rounds` will now receive the full season. Use `rounds = get_afl_week()` to restore the old behaviour.

* `load_from_url()` now defaults `use_disk_cache = FALSE` (previously `TRUE`). Pass `use_disk_cache = TRUE` explicitly if you want persistent disk caching.

* `load_fixtures(all = TRUE)` now starts from 2021 (previously 2018). Seasons before 2021 are outside `validate_seasons()` range and were producing errors.

* `check_internet_connection()` has been removed. Use `curl::has_internet()` directly.

* Unexported internal-use functions: `get_wp_model_info()`, `check_wp_model_health()`, `harmonic_mean()`, `norm_name()`. These remain accessible via `torp:::`.

## New Features

* **Local-first data loading**: All `load_*()` functions now check `torpdata/data/` first, then disk cache, then download. Downloaded files are auto-saved locally.

* `download_torp_data()` for bulk-downloading parquet files for offline access.

* **Parallel downloads**: Multi-URL loads use `curl::multi_download()` for faster batch fetching.

* **Negative cache (skip markers)**: 404 URLs are marked with `.skip` files to avoid repeated failed downloads. Use `clear_skip_markers()` to retry.

* **Column selection**: All `load_*()` functions accept a `columns` parameter to read only specific columns.

* New load functions: `load_ep_wp_charts()`, `load_player_game_ratings()`, `load_player_season_ratings()`, `load_team_ratings()`.

* `CREDIT_POS_ADJ_QUANTILE` split into 4 per-dimension constants: `CREDIT_POS_ADJ_QUANTILE_RECV`, `_DISP`, `_SPOIL`, `_HITOUT`.

* Added `R/constants.R` with centralized AFL and model constants:
  - `AFL_GOAL_WIDTH`, `AFL_QUARTER_DURATION`, `AFL_TOTAL_GAME_SECONDS`
  - `RATING_DECAY_DEFAULT_DAYS`, `SIM_NOISE_SD`, `SIM_WP_SCALING_FACTOR`

* Placeholder dashboard functions (`create_monitoring_dashboard_data()`, `get_model_health_status()`) now return informative empty structures instead of fake data.

## Bug Fixes

* `parquet_from_urls_parallel()` now warns instead of silently dropping data when column selection finds no matching columns.

* `parquet_from_urls_parallel()` now errors (instead of just warning) when downloads completely fail and no local data is available.

* `mark_download_skippable()` now logs write errors instead of silently swallowing them.

* `read_local_parquet()` now only deletes files on likely corruption errors, not transient failures (memory, locking).

* `load_torp_ratings()` and `load_team_ratings()` now warn when returning empty data.

* `load_from_url()` now warns when round filtering is requested but no round column exists in the data.

* Integration test data loading is now guarded against CRAN environments.

* Fixed double fixture load in `get_afl_week()` - now loads fixtures once and filters twice.

## Optimized Parameters

* Re-optimized rating constants with per-component decay: `RATING_DECAY_RECV` (260), `RATING_DECAY_DISP` (700), `RATING_DECAY_SPOIL` (295), `RATING_DECAY_HITOUT` (700). Prior games: `RATING_PRIOR_GAMES_RECV` (12.56), `RATING_PRIOR_GAMES_DISP` (5.83), `RATING_PRIOR_GAMES_SPOIL` (3.00), `RATING_PRIOR_GAMES_HITOUT` (15.00).

* Re-optimized credit assignment constants for disposal, reception, and position adjustment.

## Code Quality

* Merged `logging_monitoring.R` and `safe_logging.R` into a single `logging.R` file for better organization.

* Fixed global state issues by replacing `<<-` operator with package-level environment for logging state.

* Improved AUC calculation efficiency using Mann-Whitney U-statistic (O(n log n) instead of O(n^2)).

* Fixed inefficient `rbind()` patterns in `compare_baseline_models()` and `evaluate_baseline_models()` by using pre-allocated lists with `dplyr::bind_rows()`.

* Fixed `mutate_all()` performance issue in data validation using `lapply()` for column-wise operations.

* Removed unused `match_id` parameter from `match_xgs()` function.

* Replaced deprecated `dplyr::group_by_all()` with `dplyr::group_by(dplyr::across(dplyr::everything()))`.

## Documentation

* Added two vignettes: Getting Started and torp Reference Guide (consolidating ratings, models, data architecture, and simulation).

* Added pkgdown site configuration with comprehensive reference sections.

* Improved README with lifecycle badge, ecosystem table, and torpmodels install instructions.

## Internal Changes

* Reduced exported functions - internalized helper functions:
  - `rds_from_url()`, `file_reader()` (internal data loading helpers)
  - `predict_wp_naive()`, `predict_wp_time_only()` (baseline model internals)
  - `log_prediction_event()`, `log_data_quality()` (internal logging helpers)
  - `get_wp_model_info()`, `check_wp_model_health()` (model diagnostics)
  - `harmonic_mean()`, `norm_name()` (utility helpers)

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
