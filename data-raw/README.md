# data-raw/

This folder contains scripts for generating package data, training models, and computing player ratings. The numbered folders indicate the general workflow order.

## Folder Structure

```
data-raw/
├── 01-data/        # Data collection & preparation
├── 02-models/      # Model training scripts
├── 03-ratings/     # Player rating systems
├── 04-analysis/    # Analysis & simulation
├── 05-validation/  # Model validation & comparison
├── archive/        # Deprecated/experimental scripts
├── outputs/        # Generated data files (gitignored)
├── stat-models/    # Pre-trained GAM models
├── rsconnect/      # Shiny deployment config
└── utils/          # Helper scripts
```

## Workflow

### 1. Data Collection (`01-data/`)

| Script | Purpose |
|--------|---------|
| `get_stadium_data.R` | Scrape and process AFL stadium information |
| `update_fixture_table.R` | Update fixture data from fitzRoy |
| `release_data.R` | Package data for torpdata GitHub releases |

### 2. Model Training (`02-models/`)

| Script | Purpose |
|--------|---------|
| `create_ep_model.R` | Train Expected Points GAM model |
| `create_wp_model.R` | Train Win Probability model |
| `create_shot_model.R` | Train shot outcome classification model |
| `build_match_predictions.R` | Build GAM-based match predictions |
| `build_match_predictions_xgb.R` | Build XGBoost match predictions |

### 3. Player Ratings (`03-ratings/`)

| Script | Purpose |
|--------|---------|
| `bayes_rapm.R` | Bayesian Regularized Adjusted Plus-Minus |
| `build_player_ratings.R` | Calculate TORP player ratings |
| `create_player_ratings_table.R` | Generate player ratings tables |

### 4. Analysis (`04-analysis/`)

| Script | Purpose |
|--------|---------|
| `rankings.R` | Team and player rankings |
| `simming_seasons.R` | Season simulation scripts |
| `wt_av_modelling.R` | Weighted average modelling |

### 5. Validation (`05-validation/`)

| Script | Purpose |
|--------|---------|
| `validate_wp_model.R` | Validate win probability model |
| `compare_model_performance.R` | Compare model performance metrics |

## Running Scripts

Use `_run-all.R` to see the recommended execution order. Most scripts assume:

1. The torp package is loaded via `devtools::load_all()`
2. Required data is available from torpdata releases
3. Previous steps in the workflow have been completed

```r
# Example: rebuild all models
source("data-raw/_run-all.R")
```

## Output Files

Large generated files are stored in `outputs/` and gitignored:

- `bayes_od_rapm_*.rds` - RAPM model outputs (RDS format required for R model objects that cannot be serialized to Parquet)
- `stadium_data.parquet` - Stadium information
- `stat_pred_df.parquet` - Statistical prediction data

**Note on file formats:**
- **Parquet** (`.parquet`): Used for all data frames and tabular data. This is the primary data format for the package, providing better compression and cross-language compatibility (Python, etc.).
- **RDS** (`.rds`): Used only for R-specific model objects (GAM, brms, glmnet) that cannot be serialized to Parquet format. These are stored in `stat-models/` and `outputs/`.

## Archive

The `archive/` folder contains deprecated scripts kept for reference:

- `rapm_build.R` - Superseded by `bayes_rapm.R`
- `off_def_bayes_rapm.R` - Superseded by `bayes_rapm.R`
- `season_sim.R` - Empty placeholder

## Notes

- Scripts use snake_case naming convention
- Pre-trained GAM models are in `stat-models/` (70+ models)
- Shiny deployment config is in `rsconnect/`
