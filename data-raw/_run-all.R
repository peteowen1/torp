# _run-all.R
# Master script showing execution order for data-raw scripts
#
# This script documents the recommended order for running data-raw scripts.
# It is NOT intended to be sourced directly - run scripts individually as needed.

# Setup ----

devtools::load_all()

# 01-DATA: Data Collection & Preparation ----

# Get stadium data (run once, or when stadium info changes)
# source("data-raw/01-data/get_stadium_data.R")

# Update fixture table (run during/after season)
# source("data-raw/01-data/update_fixture_table.R")

# 02-MODELS: Model Training ----

## Core Models ----
# source("data-raw/02-models/create_ep_model.R")      # Expected Points
# source("data-raw/02-models/create_wp_model.R")      # Win Probability
# source("data-raw/02-models/create_shot_model.R")    # Shot outcomes

## Match Predictions ----
# source("data-raw/02-models/build_match_predictions.R")      # GAM version
# source("data-raw/02-models/build_match_predictions_xgb.R")  # XGBoost version

# 03-RATINGS: Player Rating Systems ----

## RAPM Ratings ----
# source("data-raw/03-ratings/bayes_rapm.R")

## Player Ratings ----
# source("data-raw/03-ratings/run_ratings_pipeline.R")
# source("data-raw/03-ratings/create_player_ratings_table.R")

# 04-ANALYSIS: Analysis & Simulation ----

# source("data-raw/04-analysis/rankings.R")
# source("data-raw/04-analysis/simming_seasons.R")
# source("data-raw/04-analysis/wt_av_modelling.R")

# 05-VALIDATION: Model Validation ----

# source("data-raw/05-validation/validate_wp_model.R")
# source("data-raw/05-validation/compare_model_performance.R")

# RELEASE: Package Data for Distribution ----

# source("data-raw/01-data/release_data.R")
