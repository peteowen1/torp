# data-raw/

This folder contains scripts for collecting data, training models, computing player ratings, and running analysis. The numbered folders indicate the general workflow order.

## Folder Structure

```
data-raw/
├── 01-data/           # Data collection & preparation
├── 02-models/         # Match prediction scripts
├── 03-ratings/        # EPR/TORP rating pipeline
├── 04-analysis/       # Analysis & simulation
├── 05-validation/     # Model validation & comparison
├── 06-stat-ratings/   # Stat rating estimation pipeline
├── cache-skills/      # Cached skill estimation data
├── stat-models/       # Pre-trained stat GAM models (RDS)
├── utils/             # Helper scripts
├── rebuild_everything.R  # Master rebuild script (CLI flags for phase selection)
└── README.md
```

## Workflow

### 1. Data Collection (`01-data/`)

| Script | Purpose |
|--------|---------|
| `create_aggregated_files.R` | Create per-season aggregated parquet files |
| `daily_release.R` | Daily PBP data extraction and release to torpdata |
| `get_stadium_data.R` | Scrape AFL stadium lat/lon and capacity |
| `get_weather_data.R` | Fetch historical weather from Open-Meteo API |
| `rebuild_all_release_data.R` | Full historical data rebuild (2021+) |
| `release_data.R` | Package data for torpdata GitHub releases |
| `update_fixture_table.R` | Fetch fixtures from fitzRoy, normalize |

### 2. Match Predictions (`02-models/`)

These scripts build match-level predictions using the torp package's GAM/XGBoost pipeline. **Note:** Core model training (EP, WP, shot) lives in `torpmodels/data-raw/`, not here.

| Script | Purpose |
|--------|---------|
| `build_match_predictions.R` | Build weekly GAM-based match predictions (entry point: `run_predictions_pipeline()`) |
| `build_match_predictions_xgb.R` | Build XGBoost match predictions (alternative) |

### 3. Player Ratings (`03-ratings/`)

| Script | Purpose |
|--------|---------|
| `bayes_rapm.R` | Bayesian Regularized Adjusted Plus-Minus |
| `create_player_ratings_table.R` | Generate player ratings tables for release |
| `optimize_epr_ratings.R` | Grid search for EPV/EPR decay and prior hyperparameters |
| `run_epr_pipeline.R` | Main 6-stage EPR/TORP pipeline (daily CI entry point) |

### 4. Analysis (`04-analysis/`)

| Script | Purpose |
|--------|---------|
| `rankings.R` | Team and player rankings |
| `simming_seasons.R` | Season simulation exploration |
| `simulate_seasons.R` | Season simulation scripts |
| `weather_eda.R` | Weather impact analysis |
| `wt_av_modelling.R` | Weighted average modelling experiments |

### 5. Validation (`05-validation/`)

| Script | Purpose |
|--------|---------|
| `compare_model_performance.R` | Compare model performance metrics |
| `validate_wp_model.R` | Win probability model diagnostics |

### 6. Stat Rating Pipeline (`06-stat-ratings/`)

Sequential pipeline for computing Bayesian per-stat player ratings and training the PSR model:

| Script | Purpose |
|--------|---------|
| `01_compute_match_stats.R` | Compute raw per-player-match stats from PBP |
| `02_optimize_stat_rating_params.R` | Tune Bayesian prior and decay hyperparameters |
| `03_estimate_stat_ratings.R` | Estimate player stat ratings by round (entry point) |
| `04_export_stat_ratings.R` | Export stat ratings to per-season parquets for torpdata |
| `05_compare_psr_models.R` | Compare PSR model formulations |
| `06_train_psr_model.R` | Train glmnet PSR model: stat ratings -> margin prediction |

## Supporting Directories

| Directory | Contents |
|-----------|----------|
| `stat-models/` | Pre-trained stat GAM model files (RDS format) |
| `cache-skills/` | Cached intermediate data for skill estimation |
| `utils/` | Shared helper scripts (e.g., `useful_paths.R`) |

## Running Scripts

Use `rebuild_everything.R` for a full end-to-end rebuild. It supports CLI flags to run specific phases:

```bash
# Full rebuild
Rscript rebuild_everything.R

# Specific season range
Rscript rebuild_everything.R 2024 2026

# Only retrain models
Rscript rebuild_everything.R --models-only

# Skip API scraping and simulation
Rscript rebuild_everything.R --skip-api --skip-sim
```

Most scripts assume:

1. The torp package is loaded via `devtools::load_all()`
2. Required data is available from torpdata releases
3. Previous steps in the workflow have been completed

## File Formats

- **Parquet** (`.parquet`): Used for all data frames and tabular data. Primary data format for the package.
- **RDS** (`.rds`): Used only for R-specific model objects (GAM, brms, glmnet) that cannot be serialized to Parquet. Found in `stat-models/`.
