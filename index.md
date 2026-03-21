# torp

**torp** is an R package for AFL analytics. It provides player ratings
(TORP), expected points and win probability models, season simulations,
and easy access to processed AFL data.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("peteowen1/torp")

# For the latest pre-trained models
devtools::install_github("peteowen1/torpmodels")
```

## Quick Start

### Load Data

All data is stored as parquet files on
[torpdata](https://github.com/peteowen1/torpdata) GitHub releases and
loaded on demand.

``` r
library(torp)

# Play-by-play data
pbp <- load_pbp(2025, rounds = 1:10)

# Player statistics
player_stats <- load_player_stats(2025)

# Match results and fixtures
results <- load_results(2025)
fixtures <- load_fixtures(all = TRUE)

# All available data (2021+)
pbp_all <- load_pbp(TRUE, rounds = TRUE)
```

### Player Ratings

TORP blends two independent rating systems: **EPR** (from play-by-play
data) and **PSR** (from box-score stats via Bayesian estimation +
glmnet). Per-game equivalents **EPV** and **PSV** measure single-game
contribution.

``` r
# Career TORP ratings (EPR + PSR blend)
ratings <- calculate_torp()

# Per-game values (EPV + PSV + torp_value)
game_ratings <- player_game_ratings(season_val = 2026)

# Season totals
season_totals <- player_season_ratings(2026)

# Bayesian stat ratings (per-stat next-game predictions)
stat_ratings <- load_player_stat_ratings(2026)
```

### Prediction Models

Three core models add expected points, win probability, and shot outcome
predictions to play-by-play data.

``` r
pbp <- load_pbp(2025, rounds = 1:5)
pbp_clean <- clean_pbp(pbp)

# Add model predictions
pbp_ep <- add_epv_vars(pbp_clean)     # Expected points
pbp_wp <- add_wp_vars(pbp_ep)         # Win probability
```

### Season Simulation

``` r
# Simulate remaining matches based on TORP ratings
sim_result <- simulate_season(sim_teams, sim_games)
```

## Package Ecosystem

| Package                                                   | Role                                             | Links                                             |
|-----------------------------------------------------------|--------------------------------------------------|---------------------------------------------------|
| **torp**                                                  | Core analytics, data loading, ratings, models    | [GitHub](https://github.com/peteowen1/torp)       |
| **[torpdata](https://github.com/peteowen1/torpdata)**     | Processed AFL data via GitHub releases (parquet) | [GitHub](https://github.com/peteowen1/torpdata)   |
| **[torpmodels](https://github.com/peteowen1/torpmodels)** | Pre-trained models via GitHub releases (RDS)     | [GitHub](https://github.com/peteowen1/torpmodels) |

Data flows from the AFL API (via torp’s `get_afl_*()` functions) through
torp’s cleaning pipeline into torpdata, with models stored in
torpmodels.

## Learn More

- [`vignette("getting-started")`](https://peteowen1.github.io/torp/articles/getting-started.md)
  – Installation and data loading
- [`vignette("torp-guide")`](https://peteowen1.github.io/torp/articles/torp-guide.md)
  – Ratings, models, data architecture, and simulation

## License

MIT
