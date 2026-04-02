# Getting Started with torp

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("peteowen1/torp")
```

For the latest pre-trained models, also install torpmodels:

``` r
devtools::install_github("peteowen1/torpmodels")
```

## Loading Data

torp loads processed AFL data from
[torpdata](https://github.com/peteowen1/torpdata) GitHub releases. All
data is stored as parquet files and downloaded on demand.

### Play-by-Play

``` r
library(torp)

# Current season
pbp <- load_pbp(2025)

# Specific rounds
pbp <- load_pbp(2025, rounds = 1:10)

# All available data (2021+)
pbp_all <- load_pbp(TRUE, rounds = TRUE)
```

### Player Statistics

``` r
# Current season
stats <- load_player_stats(2025)

# All seasons
stats_all <- load_player_stats(TRUE)
```

### Other Data

``` r
# Possession chains
chains <- load_chains(2025)

# Match results
results <- load_results(2025)

# Fixtures and schedule
fixtures <- load_fixtures(all = TRUE)

# Player biographical details
details <- load_player_details(2025)

# Expected goals
xg <- load_xg(2025)
```

## Player Ratings

TORP (Total Overall Rating Points) evaluates players across four
components.

``` r
# Current TORP ratings (defaults to next round)
ratings <- torp_ratings()

# Ratings at a specific point
ratings <- torp_ratings(season_val = 2025, round_val = 15)

# Game-level player performance
game_ratings <- player_game_ratings(season_val = 2025, round_num = 10)

# Season totals
season_totals <- player_season_ratings(2025)
```

See
[`vignette("torp-guide")`](https://peteowen1.github.io/torp/articles/torp-guide.md)
for the full methodology.

## Stat Ratings

Bayesian estimation of per-stat player ability, used to compute PSR:

``` r
# Pre-computed stat ratings (one row per player-round)
stat_ratings <- load_player_stat_ratings(2025)

# Player stat rating profile with percentile ranks
profile <- player_stat_rating_profile("Heeney")
print(profile)
```

See
[`vignette("stat-ratings")`](https://peteowen1.github.io/torp/articles/stat-ratings.md)
for the full stat rating and PSR system.

## Prediction Models

torp includes three core models that enrich play-by-play data:

``` r
pbp <- load_pbp(2025, rounds = 1:5)
pbp_clean <- clean_pbp(pbp)

# Expected Points -- predicts point value of each possession
pbp_ep <- add_epv_vars(pbp_clean)

# Win Probability -- estimates each team's chance of winning
pbp_wp <- add_wp_vars(pbp_ep)

# Shot Model -- goal/behind/miss probabilities for shots
shots <- pbp_wp[!is.na(pbp_wp$shot_at_goal), ]
shots <- add_shot_vars(shots)
```

See
[`vignette("torp-guide")`](https://peteowen1.github.io/torp/articles/torp-guide.md)
for details on each model.

## Caching

### Disk Cache

Data downloads are cached to disk so subsequent calls don’t re-download:

``` r
# Cached by default
pbp <- load_pbp(2025)

# Cache location
get_disk_cache_dir()

# Clear if needed
clear_disk_cache()
```

### In-Memory Cache

Fixtures and models are cached in memory within a session:

``` r
fixtures <- load_fixtures(all = TRUE)  # downloads
fixtures <- load_fixtures(all = TRUE)  # instant (cached)

# Force refresh
clear_fixture_cache()
```

## Next Steps

- [`vignette("torp-guide")`](https://peteowen1.github.io/torp/articles/torp-guide.md)
  – Full reference: data architecture, ratings, models, simulation
