# Stat Models Directory

This directory contains 58 pre-trained GAM (Generalized Additive Model) models for predicting player statistics in AFL matches.

## Overview

Each model predicts a specific player statistic based on game context features (opponent strength, venue, etc.). Models are stored as `.rds` files and can be loaded via the `torpmodels` package:

```r
# Load via torpmodels
torpmodels::load_stat_model("goals")

# Or load directly
model <- readRDS("data-raw/stat-models/goals.rds")
```

## Model Categories

### Core Statistics (12 models)
| Model | Description |
|-------|-------------|
| `disposals` | Total disposals |
| `kicks` | Total kicks |
| `handballs` | Total handballs |
| `marks` | Total marks |
| `goals` | Goals kicked |
| `behinds` | Behinds kicked |
| `tackles` | Total tackles |
| `hitouts` | Ruck hitouts |
| `frees_for` | Free kicks for |
| `frees_against` | Free kicks against |
| `clangers` | Clangers |
| `turnovers` | Turnovers |

### Possession Statistics (5 models)
| Model | Description |
|-------|-------------|
| `total_possessions` | Total possessions |
| `contested_possessions` | Contested possessions |
| `uncontested_possessions` | Uncontested possessions |
| `disposal_efficiency` | Disposal efficiency % |
| `bounces` | Bounces |

### Clearances (3 models)
| Model | Description |
|-------|-------------|
| `clearances_total_clearances` | Total clearances |
| `clearances_centre_clearances` | Centre bounce clearances |
| `clearances_stoppage_clearances` | Stoppage clearances |

### Inside 50 & Scoring (6 models)
| Model | Description |
|-------|-------------|
| `inside50s` | Inside 50 entries |
| `marks_inside50` | Marks inside 50 |
| `shots_at_goal` | Shots at goal |
| `goal_accuracy` | Goal accuracy % |
| `goal_assists` | Goal assists |
| `score_involvements` | Score involvements |

### Defensive Statistics (4 models)
| Model | Description |
|-------|-------------|
| `rebound50s` | Rebound 50s |
| `intercepts` | Intercepts |
| `one_percenters` | One percenters |
| `contested_marks` | Contested marks |

### Extended Statistics - Pressure & Contests (10 models)
| Model | Description |
|-------|-------------|
| `extended_stats_pressure_acts` | Pressure acts |
| `extended_stats_def_half_pressure_acts` | Defensive half pressure acts |
| `extended_stats_tackles_inside50` → `tackles_inside50` | Tackles inside 50 |
| `extended_stats_contest_def_one_on_ones` | Defensive contest one-on-ones |
| `extended_stats_contest_off_one_on_ones` | Offensive contest one-on-ones |
| `extended_stats_contest_def_losses` | Defensive contest losses |
| `extended_stats_contest_def_loss_percentage` | Defensive contest loss % |
| `extended_stats_contest_off_wins` | Offensive contest wins |
| `extended_stats_contest_off_wins_percentage` | Offensive contest win % |
| `extended_stats_contested_possession_rate` | Contested possession rate |

### Extended Statistics - Ground Ball & Marks (4 models)
| Model | Description |
|-------|-------------|
| `extended_stats_ground_ball_gets` | Ground ball gets |
| `extended_stats_f50ground_ball_gets` | F50 ground ball gets |
| `extended_stats_intercept_marks` | Intercept marks |
| `extended_stats_marks_on_lead` | Marks on lead |

### Extended Statistics - Kicks & Disposals (4 models)
| Model | Description |
|-------|-------------|
| `extended_stats_effective_kicks` | Effective kicks |
| `extended_stats_effective_disposals` | Effective disposals |
| `extended_stats_kick_efficiency` | Kick efficiency % |
| `extended_stats_kick_to_handball_ratio` | Kick to handball ratio |

### Extended Statistics - Ruck (4 models)
| Model | Description |
|-------|-------------|
| `extended_stats_ruck_contests` | Ruck contests |
| `extended_stats_hitouts_to_advantage` | Hitouts to advantage |
| `extended_stats_hitout_to_advantage_rate` | Hitout to advantage rate |
| `extended_stats_hitout_win_percentage` | Hitout win percentage |

### Extended Statistics - Other (6 models)
| Model | Description |
|-------|-------------|
| `extended_stats_centre_bounce_attendances` | Centre bounce attendances |
| `extended_stats_kickins` | Kick-ins |
| `extended_stats_kickins_playon` | Kick-ins play on |
| `extended_stats_spoils` | Spoils |
| `extended_stats_score_launches` | Score launches |
| `time_on_ground_percentage` | Time on ground % |

## Model Details

- **Type**: GAM models trained with `mgcv::bam()`
- **Training Data**: Historical player statistics from 2021-2024
- **Last Updated**: July 2025
- **File Format**: RDS (R serialized object)
- **Total Size**: ~177 MB

## Usage Example

```r
library(torp)
library(torpmodels)

# Load a stat model
goals_model <- torpmodels::load_stat_model("goals")

# Get player data for prediction
player_data <- load_player_stats(2024)

# Make predictions
predictions <- predict(goals_model, newdata = player_data, type = "response")
```

## Notes

- Models with `extended_stats_` prefix use AFL's extended statistics which may have limited historical availability
- Some percentage/rate models may require special handling for edge cases (0 attempts)
- Models are designed for per-game predictions, not season totals
