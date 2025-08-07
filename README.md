
<!-- README.md is generated from README.Rmd. Please edit that file -->

# torp <img src="man/figures/logo.png" align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/peteowen1/torp/workflows/R-CMD-check/badge.svg)](https://github.com/peteowen1/torp/actions)
[![Codecov test
coverage](https://codecov.io/gh/peteowen1/torp/branch/main/graph/badge.svg)](https://app.codecov.io/gh/peteowen1/torp?branch=main)
[![R-CMD-check](https://github.com/peteowen1/torp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/peteowen1/torp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**torp** (Team Offensive Rating Points) is an R package for Australian
Football League (AFL) analytics, providing tools for player rating
systems, match predictions, and advanced statistical modeling.

## Features

- **Player Ratings**: Calculate comprehensive player ratings using
  advanced statistical models
- **Match Predictions**: Predict match outcomes using XGBoost and GAM
  models  
- **Expected Points (EP)**: Calculate expected points for any game
  situation
- **Win Probability**: Real-time win probability calculations during
  matches
- **Data Access**: Easy access to AFL data including play-by-play,
  player stats, and match results

## Installation

You can install the development version of torp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("peteowen1/torp")
```

## Quick Start

### Loading AFL Data

``` r
library(torp)

# Load the latest AFL player statistics
player_stats <- load_player_stats()

# Load match results for specific seasons
results <- load_results(seasons = 2023:2024)

# Load play-by-play data for analysis
pbp_data <- load_pbp(seasons = 2024, rounds = 1:5)
```

### Player Ratings and Match Predictions

``` r
# Calculate player ratings using built-in data
data(plyr_gm_df)
head(plyr_gm_df)

# Access pre-trained models for predictions
data(ep_model)   # Expected Points model
data(wp_model)   # Win Probability model

# Load match fixtures for predictions
fixtures <- load_fixtures(seasons = 2024)
```

### Data Processing and Analysis

``` r
# Clean and prepare data for modeling
# (requires raw play-by-play data)
cleaned_data <- clean_model_data_epv

# Select variables for Expected Points modeling
model_vars <- select_epv_model_vars(cleaned_data, label = TRUE)

# Calculate advanced statistics
harmonic_mean(c(10, 15, 20), c(12, 18, 25))
```

## Configuration

You can configure the data repository used by torp:

``` r
# Set a custom data repository
set_torp_data_repo("your_username/your_repo")

# Or use environment variable
Sys.setenv(TORP_DATA_REPO = "your_username/your_repo")
```

## Data Sources

The package accesses data from the [torpdata
repository](https://github.com/peteowen1/torpdata), which contains:

- Play-by-play tracking data
- Player statistics and biographical information  
- Match results and fixtures
- Team information and lineups
- Pre-trained statistical models

## Citation

If you use torp in your research, please cite:

    Owen, P. (2024). torp: AFL Analytics and Player Rating System. 
    R package version 0.0.0.9001. https://github.com/peteowen1/torp

## License

This package is licensed under the MIT License. See [LICENSE](LICENSE)
for details.
