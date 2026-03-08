# torp Reference Guide

## Data Architecture

### Package Ecosystem

| Package        | Role                                          | Storage                   |
|----------------|-----------------------------------------------|---------------------------|
| **torp**       | Core analytics, data loading, ratings, models | R package                 |
| **torpdata**   | Processed AFL data                            | GitHub releases (parquet) |
| **torpmodels** | Pre-trained models                            | GitHub releases (RDS)     |

### Data Flow

    AFL API (get_afl_*) --> torp (clean + enrich) --> torpdata (store)
                                |
                           torpmodels (models)
                                |
                           torp (predict)

1.  **Collection**: Raw data fetched from the AFL API via torp’s
    `get_afl_*()` functions
2.  **Processing**: torp cleans data, adds EP/WP/shot model predictions
3.  **Storage**: Processed data uploaded to torpdata GitHub releases as
    parquet files
4.  **Loading**: torp `load_*()` functions download from torpdata on
    demand
5.  **Models**: Trained models stored in torpmodels, loaded via
    [`load_model_with_fallback()`](https://peteowen1.github.io/torp/reference/load_model_with_fallback.md)

### Data Types

| Function                                                                                         | Data                          | Approx Size       |
|--------------------------------------------------------------------------------------------------|-------------------------------|-------------------|
| [`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md)                           | Play-by-play events           | ~320K rows/season |
| [`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md)                     | Possession sequences          | ~160K rows/season |
| [`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md)         | Player game stats             | ~10K rows/season  |
| [`load_player_game_data()`](https://peteowen1.github.io/torp/reference/load_player_game_data.md) | Player credit points for TORP | ~10K rows/season  |
| [`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md)                 | Match schedule                | ~200 rows/season  |
| [`load_results()`](https://peteowen1.github.io/torp/reference/load_results.md)                   | Match scores                  | ~200 rows/season  |
| [`load_teams()`](https://peteowen1.github.io/torp/reference/load_teams.md)                       | Team lineups                  | ~10K rows/season  |
| [`load_player_details()`](https://peteowen1.github.io/torp/reference/load_player_details.md)     | Player bio info               | ~800 rows/season  |
| [`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md)                             | Expected goals                | ~200 rows/season  |
| [`load_torp_ratings()`](https://peteowen1.github.io/torp/reference/load_torp_ratings.md)         | Pre-computed TORP ratings     | ~113K rows total  |

### Loading Examples

``` r
library(torp)
pbp <- load_pbp(2025)
pbp <- load_pbp(2025, rounds = 1:10)
pbp_all <- load_pbp(TRUE, rounds = TRUE)
```

### Caching

All data downloads are cached to disk. Fixtures cached in-memory within
a session. Models cached in memory to avoid reloading.

``` r
get_disk_cache_dir()
clear_disk_cache()
clear_fixture_cache()
clear_model_cache()
```

### torpdata Releases

| Release Tag           | Contents                          |
|-----------------------|-----------------------------------|
| `pbp-data`            | Play-by-play files by season      |
| `chains-data`         | Chain files by season             |
| `player_stats-data`   | Player stat files by season       |
| `player_game-data`    | Player game credit data by season |
| `fixtures-data`       | Fixture files by season           |
| `results-data`        | Result files by season            |
| `teams-data`          | Team lineup files by season       |
| `player_details-data` | Player detail files by season     |
| `xg-data`             | Expected goals by season          |
| `ratings-data`        | Pre-computed TORP ratings         |
| `predictions`         | Match predictions                 |

------------------------------------------------------------------------

## TORP Player Ratings

### Overview

TORP (Total Overall Rating Points) evaluates AFL players across four
skill categories using play-by-play expected points data.

    torp = recv/(wt + prior_recv) + disp/(wt + prior_disp)
         + spoil/(wt + prior_spoil) + hitout/(wt + prior_hitout)

### Rating Components

**Disposal (torp_disp)**: Value added through kicks and handballs (delta
EPV). **Receiving (torp_recv)**: Value of receiving a disposal (EPV
change to receiver). **Spoil/Tackle (torp_spoil)**: Defensive impact
from spoils, tackles, pressure acts. **Hitout (torp_hitout)**: Ruck
contribution from hitouts and ruck contests.

### Getting Ratings

``` r
library(torp)
ratings <- calculate_torp_ratings()
ratings <- calculate_torp_ratings(season_val = 2025, round_val = 15)

# Game-level
game_ratings <- player_game_ratings(season_val = 2025, round_num = 10)

# Season totals
season <- player_season_ratings(2025)
```

### Decay Weighting

    weight = exp(-days_since_game / decay_days)

Decay varies by component: Receiving 260 days, Spoil 295 days,
Disposal/Hitout 700 days. A game one year ago has weight ~0.24 for
receiving, ~0.59 for disposal.

### Bayesian Shrinkage

| Component | Prior Games | Decay (days) |
|-----------|-------------|--------------|
| Receiving | 12.56       | 260          |
| Disposal  | 5.83        | 700          |
| Spoil     | 3.00        | 295          |
| Hitout    | 15.00       | 700          |

### Pre-Loading Data

``` r
player_game_data <- load_player_game_data(TRUE)
player_details <- load_player_details(2025)
ratings <- calculate_torp_ratings(
  season_val = 2025, round_val = 15,
  plyr_tm_df = player_details,
  player_game_data = player_game_data
)
```

------------------------------------------------------------------------

## Prediction Models

Three core models enrich play-by-play data. Models load from
[torpmodels](https://github.com/peteowen1/torpmodels) and are cached in
memory.

### Complete Pipeline

``` r
library(torp)
pbp <- load_pbp(2025, rounds = 1:10)
pbp_clean <- clean_pbp(pbp)
pbp_ep <- add_epv_vars(pbp_clean)        # Expected Points
pbp_wp <- add_wp_vars(pbp_ep)            # Win Probability
shots <- pbp_wp[!is.na(pbp_wp$shot_at_goal), ]
shots <- add_shot_vars(shots)             # Shot Model
```

### Expected Points (EP)

XGBoost model: 5 outcome probabilities. Adds `exp_pts` and `delta_epv`.

### Win Probability (WP)

Ensemble of XGBoost + baseline. Adds `wp`, `wpa`, `wp_category`,
`high_leverage`.

### Shot Model

GAM with 3-class probabilities. Adds `goal_prob`, `behind_prob`,
`clanger_prob`, `xscore`.

------------------------------------------------------------------------

## Season Simulation

[`simulate_season()`](https://peteowen1.github.io/torp/reference/simulate_season.md)
simulates remaining AFL matches using team TORP ratings.

For each unplayed match: expected margin from TORP difference + home
advantage (6 pts) + random noise (SD = 26 pts).

``` r
sim_result <- simulate_season(sim_teams, sim_games)

# Multiple simulations
n_sims <- 1000
all_sims <- lapply(seq_len(n_sims), function(i) {
  sim <- simulate_season(sim_teams, sim_games)
  sim$sim_id <- i
  sim
})
all_sims_df <- data.table::rbindlist(all_sims)
```

| Constant                | Default | Description                               |
|-------------------------|---------|-------------------------------------------|
| `SIM_NOISE_SD`          | 26      | Standard deviation of random margin noise |
| `SIM_HOME_ADVANTAGE`    | 6       | Points added to home team expected margin |
| `SIM_WP_SCALING_FACTOR` | 50      | Scaling for TORP diff to win probability  |
