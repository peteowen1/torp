# Using Prediction Models

torp includes three core prediction models for AFL analytics. Models are
loaded automatically from the
[torpmodels](https://github.com/peteowen1/torpmodels) package and cached
in memory.

## Complete Pipeline

``` r
library(torp)

# 1. Load and clean play-by-play data
pbp <- load_pbp(2025, rounds = 1:10)
pbp_clean <- clean_pbp(pbp)

# 2. Add expected points
pbp_ep <- add_epv_vars(pbp_clean)

# 3. Add win probability (requires EP variables)
pbp_wp <- add_wp_vars(pbp_ep)

# 4. For shots, add outcome probabilities
shots <- pbp_wp[!is.na(pbp_wp$shot_at_goal), ]
shots <- add_shot_vars(shots)
```

## Expected Points (EP)

The EP model predicts the expected point outcome of each possession
based on field position and game state. It’s an XGBoost model that
outputs probabilities for five outcomes:

- Opposition goal (6 pts to opposition)
- Opposition behind (1 pt to opposition)
- No score (end of quarter)
- Team behind (1 pt to team)
- Team goal (6 pts to team)

``` r
pbp_ep <- add_epv_vars(pbp_clean)

# Key columns added:
# exp_pts    -- expected point value of possession
# delta_epv  -- change in expected points from the play
```

The `delta_epv` column is the foundation of the TORP player rating
system – it measures how much each action changed the expected outcome.

## Win Probability (WP)

The WP model estimates each team’s probability of winning at any point
during a match. It uses an enhanced ensemble approach that combines an
XGBoost model with a baseline model.

``` r
# Requires EP variables first
pbp_wp <- add_wp_vars(pbp_ep)

# Key columns added:
# wp           -- win probability for the team with possession
# wpa          -- win probability added by the play
# wp_category  -- descriptive label (very_likely, likely, toss_up, etc.)
# high_leverage -- flag for high-impact situations
```

Features used: score differential, time remaining, expected points from
field position, home/away status, and recent scoring momentum.

## Shot Model

The shot model predicts outcomes for shots at goal using a GAM
(Generalised Additive Model). It produces three-class probabilities:

``` r
shots <- add_shot_vars(shots)

# Key columns added:
# goal_prob     -- probability of goal
# behind_prob   -- probability of behind
# clanger_prob  -- probability of miss/clanger
# xscore        -- expected score (6 * goal_prob + behind_prob)
# on_target_prob -- probability of scoring (goal + behind)
```

Features used: shot distance, shot angle, shot type (set shot vs general
play), pressure indicators, and time in quarter.

## Model Loading

Models are loaded from torpmodels on first use and cached in memory:

``` r
# Install torpmodels for latest models
devtools::install_github("peteowen1/torpmodels")

# Direct model access
ep_model <- torpmodels::load_torp_model("ep")
wp_model <- torpmodels::load_torp_model("wp")
shot_model <- torpmodels::load_torp_model("shot")

# Clear model cache (e.g., after updating torpmodels)
clear_model_cache()
```

## See Also

- [`vignette("getting-started")`](https://peteowen1.github.io/torp/articles/getting-started.md)
  – Installation and data loading
- [`vignette("player-ratings")`](https://peteowen1.github.io/torp/articles/player-ratings.md)
  – TORP rating methodology
- [`vignette("data-architecture")`](https://peteowen1.github.io/torp/articles/data-architecture.md)
  – Data pipeline and caching
