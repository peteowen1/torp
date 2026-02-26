# Package index

## Data Loading

Load processed AFL data from torpdata GitHub releases

- [`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md)
  : Load Chains Data
- [`load_ep_wp_charts()`](https://peteowen1.github.io/torp/reference/load_ep_wp_charts.md)
  : Load EP/WP Chart Data
- [`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md)
  : Load AFL Fixture Data
- [`load_from_url()`](https://peteowen1.github.io/torp/reference/load_from_url.md)
  : Load parquet files from remote URLs
- [`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md)
  : Load Play By Play Data
- [`load_player_details()`](https://peteowen1.github.io/torp/reference/load_player_details.md)
  : Load AFL Player Details Data
- [`load_player_game_data()`](https://peteowen1.github.io/torp/reference/load_player_game_data.md)
  : Load Player Game Data
- [`load_player_game_ratings()`](https://peteowen1.github.io/torp/reference/load_player_game_ratings.md)
  : Load Player Game Ratings Data
- [`load_player_season_ratings()`](https://peteowen1.github.io/torp/reference/load_player_season_ratings.md)
  : Load Player Season Ratings Data
- [`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md)
  : Load Player Stats Data
- [`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md)
  : Load AFL Match Predictions Data
- [`load_results()`](https://peteowen1.github.io/torp/reference/load_results.md)
  : Load AFL Match Results Data
- [`load_team_ratings()`](https://peteowen1.github.io/torp/reference/load_team_ratings.md)
  : Load Team Ratings Data
- [`load_teams()`](https://peteowen1.github.io/torp/reference/load_teams.md)
  : Load AFL Team and Lineup Data
- [`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md)
  : Load TORP Ratings Data
- [`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md) :
  Load Expected Goals (xG) Data
- [`fixtures`](https://peteowen1.github.io/torp/reference/fixtures.md) :
  AFL Season Fixtures

## Prediction Models

Add expected points, win probability, and shot predictions to
play-by-play data

- [`add_epv_vars()`](https://peteowen1.github.io/torp/reference/add_epv_vars.md)
  : Add Expected Points Value (EPV) Variables
- [`add_wp_vars()`](https://peteowen1.github.io/torp/reference/add_wp_vars.md)
  : Add Win Probability Variables
- [`add_shot_vars()`](https://peteowen1.github.io/torp/reference/add_shot_vars.md)
  : Add Shot Variables

## Data Cleaning

Clean and prepare AFL data for analysis

- [`clean_pbp()`](https://peteowen1.github.io/torp/reference/clean_pbp.md)
  : Clean Play-by-Play Data
- [`clean_model_data_epv()`](https://peteowen1.github.io/torp/reference/clean_model_data_epv.md)
  : Clean Model Data for Expected Points Value (EPV)
- [`clean_model_data_wp()`](https://peteowen1.github.io/torp/reference/clean_model_data_wp.md)
  : Clean Model Data for Win Probability (WP)
- [`clean_shots_data()`](https://peteowen1.github.io/torp/reference/clean_shots_data.md)
  : Clean Shots Data
- [`norm_name()`](https://peteowen1.github.io/torp/reference/norm_name.md)
  : Normalize Player Names

## Player Ratings

Calculate and view TORP player ratings

- [`calculate_torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md)
  [`torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md)
  : Get TORP ratings
- [`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md)
  : Get game ratings
- [`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md)
  : Get season total ratings
- [`player_profile()`](https://peteowen1.github.io/torp/reference/player_profile.md)
  : Get a Player Profile
- [`print(`*`<torp_player_profile>`*`)`](https://peteowen1.github.io/torp/reference/print.torp_player_profile.md)
  : Print a player profile
- [`create_player_game_data()`](https://peteowen1.github.io/torp/reference/create_player_game_data.md)
  : Create Player Game Data
- [`default_credit_params()`](https://peteowen1.github.io/torp/reference/default_credit_params.md)
  : Default credit assignment parameters

## Season Simulation

Simulate AFL seasons and predict match outcomes

- [`simulate_season()`](https://peteowen1.github.io/torp/reference/simulate_season.md)
  [`sim_season()`](https://peteowen1.github.io/torp/reference/simulate_season.md)
  : Simulate a season of games

## Chain Analysis

Access and analyse possession chains

- [`get_match_chains()`](https://peteowen1.github.io/torp/reference/get_match_chains.md)
  : Get Match Chains
- [`get_week_chains()`](https://peteowen1.github.io/torp/reference/get_week_chains.md)
  : Get Week Chains
- [`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md)
  : Load Chains Data

## Match Analysis

Match-level expected goals and predictions

- [`calculate_match_xgs()`](https://peteowen1.github.io/torp/reference/calculate_match_xgs.md)
  [`match_xgs()`](https://peteowen1.github.io/torp/reference/calculate_match_xgs.md)
  : Calculate xGs for AFL Matches
- [`calculate_player_stats()`](https://peteowen1.github.io/torp/reference/calculate_player_stats.md)
  : Calculate player statistics
- [`get_season_data()`](https://peteowen1.github.io/torp/reference/get_season_data.md)
  : Get season data
- [`filter_game_data()`](https://peteowen1.github.io/torp/reference/filter_game_data.md)
  : Filter game data
- [`prepare_final_dataframe()`](https://peteowen1.github.io/torp/reference/prepare_final_dataframe.md)
  : Prepare final dataframe

## Model Validation

Evaluate model performance, calibration, and drift

- [`evaluate_model_comprehensive()`](https://peteowen1.github.io/torp/reference/evaluate_model_comprehensive.md)
  : Comprehensive Model Evaluation
- [`compare_models_statistical()`](https://peteowen1.github.io/torp/reference/compare_models_statistical.md)
  : Compare Multiple Models Statistically
- [`create_calibration_plot()`](https://peteowen1.github.io/torp/reference/create_calibration_plot.md)
  : Create Calibration Plot Data
- [`create_validation_report()`](https://peteowen1.github.io/torp/reference/create_validation_report.md)
  : Create Model Validation Report
- [`create_model_comparison_report()`](https://peteowen1.github.io/torp/reference/create_model_comparison_report.md)
  : Create Model Comparison Report
- [`check_wp_model_health()`](https://peteowen1.github.io/torp/reference/check_wp_model_health.md)
  : Check Win Probability Model Health
- [`get_wp_model_info()`](https://peteowen1.github.io/torp/reference/get_wp_model_info.md)
  : Get Win Probability Model Version Information
- [`log_model_performance()`](https://peteowen1.github.io/torp/reference/log_model_performance.md)
  : Log Model Performance Metrics
- [`monitor_model_drift()`](https://peteowen1.github.io/torp/reference/monitor_model_drift.md)
  : Monitor Model Drift
- [`validate_data_freshness()`](https://peteowen1.github.io/torp/reference/validate_data_freshness.md)
  : Validate Data Freshness
- [`validate_data_quality()`](https://peteowen1.github.io/torp/reference/validate_data_quality.md)
  : Validate Data Quality
- [`validate_data_schema()`](https://peteowen1.github.io/torp/reference/validate_data_schema.md)
  : Validate Data Schema

## Model Training

Utilities for model development and cross-validation

- [`create_grouped_cv_folds()`](https://peteowen1.github.io/torp/reference/create_grouped_cv_folds.md)
  : Create Grouped Cross-Validation Folds
- [`create_temporal_splits()`](https://peteowen1.github.io/torp/reference/create_temporal_splits.md)
  : Time-Based Train/Validation/Test Split
- [`select_epv_model_vars()`](https://peteowen1.github.io/torp/reference/select_epv_model_vars.md)
  : Select EPV Model Variables
- [`select_wp_model_vars()`](https://peteowen1.github.io/torp/reference/select_wp_model_vars.md)
  : Select WP Model Variables
- [`select_afl_model_vars()`](https://peteowen1.github.io/torp/reference/select_afl_model_vars.md)
  : Select AFL Match Prediction Model Variables

## Caching

In-memory and disk caching utilities

- [`clear_disk_cache()`](https://peteowen1.github.io/torp/reference/clear_disk_cache.md)
  : Clear Disk Cache
- [`clear_fixture_cache()`](https://peteowen1.github.io/torp/reference/clear_fixture_cache.md)
  : Clear Fixture Cache
- [`clear_model_cache()`](https://peteowen1.github.io/torp/reference/clear_model_cache.md)
  : Clear Model Cache
- [`clear_skip_markers()`](https://peteowen1.github.io/torp/reference/clear_skip_markers.md)
  : Clear Skip Markers (Negative Cache)
- [`get_cache_info()`](https://peteowen1.github.io/torp/reference/get_cache_info.md)
  : Get Cache Information
- [`get_disk_cache_info()`](https://peteowen1.github.io/torp/reference/get_disk_cache_info.md)
  : Get Disk Cache Information
- [`get_disk_cache_size()`](https://peteowen1.github.io/torp/reference/get_disk_cache_size.md)
  : Get Disk Cache Size
- [`get_model_cache_info()`](https://peteowen1.github.io/torp/reference/get_model_cache_info.md)
  : Get Model Cache Info
- [`set_disk_cache_options()`](https://peteowen1.github.io/torp/reference/set_disk_cache_options.md)
  : Set Disk Cache Options

## Constants

AFL and model constants

## Utilities

Helper and convenience functions

- [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md)
  : Get AFL Season
- [`get_afl_week()`](https://peteowen1.github.io/torp/reference/get_afl_week.md)
  : Get AFL Week
- [`harmonic_mean()`](https://peteowen1.github.io/torp/reference/harmonic_mean.md)
  : Vectorized Harmonic Mean of Two Numeric Vectors
- [`parquet_from_url()`](https://peteowen1.github.io/torp/reference/parquet_from_url.md)
  : Load parquet file from a remote connection
- [`save_to_release()`](https://peteowen1.github.io/torp/reference/save_to_release.md)
  : Save a Data Frame to a GitHub Release via Piggyback
- [`set_torp_data_repo()`](https://peteowen1.github.io/torp/reference/set_torp_data_repo.md)
  : Set TORP Data Repository
- [`setup_torp_logging()`](https://peteowen1.github.io/torp/reference/setup_torp_logging.md)
  : Setup TORP Logging Configuration
- [`download_torp_data()`](https://peteowen1.github.io/torp/reference/download_torp_data.md)
  : Download TORP Data for Local Storage
- [`get_local_data_dir()`](https://peteowen1.github.io/torp/reference/get_local_data_dir.md)
  : Get Local Data Directory
- [`save_locally()`](https://peteowen1.github.io/torp/reference/save_locally.md)
  : Save Data Frame Locally in torpdata/data/
- [`set_local_data_dir()`](https://peteowen1.github.io/torp/reference/set_local_data_dir.md)
  : Set Local Data Directory

## Internal / Developer

Internal utilities accessible via torp::: for model training and data
scraping

- [`access_api()`](https://peteowen1.github.io/torp/reference/access_api.md)
  : Access API
- [`get_players()`](https://peteowen1.github.io/torp/reference/get_players.md)
  : Get Players
- [`get_round_games()`](https://peteowen1.github.io/torp/reference/get_round_games.md)
  : Get Round Games
- [`get_season_games()`](https://peteowen1.github.io/torp/reference/get_season_games.md)
  : Get Season Games
- [`select_shot_model_vars()`](https://peteowen1.github.io/torp/reference/select_shot_model_vars.md)
  : Select Shot Model Variables
- [`predict_shot_probabilities()`](https://peteowen1.github.io/torp/reference/predict_shot_probabilities.md)
  : Predict Shot Probabilities
