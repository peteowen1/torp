# Data Architecture

## Package Ecosystem

The torpverse consists of three packages:

| Package        | Role                                          | Storage                   |
|----------------|-----------------------------------------------|---------------------------|
| **torp**       | Core analytics, data loading, ratings, models | R package                 |
| **torpdata**   | Processed AFL data                            | GitHub releases (parquet) |
| **torpmodels** | Pre-trained models                            | GitHub releases (RDS)     |

## Data Flow

    AFL API (fitzRoy) --> torp (clean + enrich) --> torpdata (store)
                              |
                         torpmodels (models)
                              |
                         torp (predict)

1.  **Collection**: Raw data fetched from the AFL API via
    [fitzRoy](https://jimmyday12.github.io/fitzRoy/)
2.  **Processing**: torp cleans data, adds EP/WP/shot model predictions
3.  **Storage**: Processed data uploaded to torpdata GitHub releases as
    parquet files
4.  **Loading**: torp’s `load_*()` functions download from torpdata on
    demand
5.  **Models**: Trained models stored in torpmodels, loaded via
    [`load_model_with_fallback()`](https://peteowen1.github.io/torp/reference/load_model_with_fallback.md)

## Data Types

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

# Single season
pbp <- load_pbp(2025)

# Specific rounds
pbp <- load_pbp(2025, rounds = 1:10)

# All available data (2021+)
pbp_all <- load_pbp(TRUE, rounds = TRUE)
chains_all <- load_chains(TRUE)
stats_all <- load_player_stats(TRUE)
```

## Caching

### Disk Cache

All data downloads are cached to disk. Subsequent calls for the same
data load from cache instead of re-downloading.

``` r
# Cache location
get_disk_cache_dir()
# Typically: tools::R_user_dir("torp", "cache")

# Check cache size
get_disk_cache_size()

# Clear cache
clear_disk_cache()
```

### In-Memory Cache

Fixtures are cached in-memory within a session for fast repeated access:

``` r
fixtures <- load_fixtures(all = TRUE)  # downloads + caches
fixtures <- load_fixtures(all = TRUE)  # returns cached copy

# Force refresh
clear_fixture_cache()
```

### Model Cache

Loaded models are cached in memory to avoid reloading:

``` r
# First call loads model from torpmodels
pbp_ep <- add_epv_vars(pbp)

# Subsequent calls use cached model
pbp_ep2 <- add_epv_vars(pbp2)

# Clear after updating torpmodels
clear_model_cache()
```

## torpdata Releases

Processed data is organised into GitHub release tags:

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

## torpmodels Releases

| Release Tag   | Contents                                           |
|---------------|----------------------------------------------------|
| `core-models` | EP, WP, shot, and match prediction models          |
| `stat-models` | 58 stat-specific GAM models for player projections |

## Automated Updates

A GitHub Action (`.github/workflows/daily-data-release.yml`) runs daily
at 2:00 AM AEST to:

1.  Fetch latest data from the AFL API
2.  Process through torp’s cleaning pipeline
3.  Upload to torpdata releases

It can also be triggered manually with `force_release` or
`rebuild_aggregates` options.

## See Also

- [`vignette("getting-started")`](https://peteowen1.github.io/torp/articles/getting-started.md)
  – Installation and data loading
- [`vignette("model-usage")`](https://peteowen1.github.io/torp/articles/model-usage.md)
  – EP, WP, and shot models
- [`vignette("player-ratings")`](https://peteowen1.github.io/torp/articles/player-ratings.md)
  – TORP rating methodology
