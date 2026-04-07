# ── Live Model Export ─────────────────────────────────────────
# Exports model coefficients/lookup tables for live in-browser computation
# on inthegame.blog match pages.
#
# Run from torpverse/torp:
#   source("scripts/live-model-export.R")
#
# Outputs:
#   inst/extdata/xg_lookup.json     — xG lookup (goal_x × abs_y → probabilities)
#   inst/extdata/psr_live_coefficients.csv — PSR refit with live-available stats only
#   inst/extdata/epv_live_weights.json    — EPV weights for live-available stats only

library(dplyr)
library(jsonlite)

devtools::load_all()

# ── Stats available from CFS live player stats endpoint ──────
# These are the stats we can get from the AFL API during a live game.
# Some PSR/EPV stats (ground_ball_gets, pressure_acts, spoils, etc.)
# are NULL or missing from the live feed.

LIVE_STATS <- c(
 "goals", "behinds", "shots_at_goal", "score_involvements", "goal_assists",
 "kicks", "handballs", "disposals", "marks",
 "contested_possessions", "uncontested_possessions", "contested_marks",
 "clearances", "centre_clearances", "stoppage_clearances",
 "inside50s", "marks_inside50", "rebound50s",
 "metres_gained", "tackles", "intercepts", "one_percenters",
 "hitouts", "frees_for", "frees_against",
 "clangers", "turnovers", "bounces"
)

# Derived stats we CAN compute from the above:
# disposal_efficiency = effective_disposals / disposals (but we don't have effective_disposals)
# contested_poss_rate = contested_possessions / (contested_possessions + uncontested_possessions)
# goal_accuracy = goals / shots_at_goal

LIVE_DERIVED <- c(
  "contested_poss_rate",
  "goal_accuracy"
)

cat("\n══ Live Model Export ══════════════════════════════════════\n")
cat("Stats available live:", length(LIVE_STATS), "raw +", length(LIVE_DERIVED), "derived\n\n")

# ══════════════════════════════════════════════════════════════
# 1. xG LOOKUP TABLE
# ══════════════════════════════════════════════════════════════
cat("── 1. xG Lookup Table ──────────────────────────────────────\n")

tryCatch({
  # Load the shot model from torpmodels
  shot_model <- load_model_with_fallback("shot")

  if (!requireNamespace("mgcv", quietly = TRUE)) stop("mgcv required")
  if (!"mgcv" %in% .packages()) attachNamespace("mgcv")

  if (is.null(shot_model$model)) {
    stop("Shot model has been stripped ($model is NULL). ",
         "Re-load unstripped model for factor level extraction.")
  }

  # Build prediction grid: goal_x (5-65m) × abs_y (0-35m)
  # 1m resolution = ~2000 cells, small enough for JSON
  grid <- expand.grid(
    goal_x = seq(5, 65, by = 1),
    abs_y  = seq(0, 35, by = 1),
    # Use "average" values for categorical features
    phase_of_play = factor("openPlay", levels = levels(shot_model$model$phase_of_play)),
    lag_goal_x = 0,
    lag_y = 0,
    play_type = factor("kick", levels = levels(shot_model$model$play_type)),
    player_position_fac = factor("Forward", levels = levels(shot_model$model$player_position_fac)),
    player_id_shot = factor("Other", levels = levels(shot_model$model$player_id_shot))
  )

  preds <- predict(shot_model, newdata = grid, type = "response")

  xg_lookup <- data.frame(
    goal_x = grid$goal_x,
    abs_y  = grid$abs_y,
    clanger_prob = round(preds[, 1], 4),
    behind_prob  = round(preds[, 2], 4),
    goal_prob    = round(preds[, 3], 4)
  )

  xg_lookup$xscore <- round(xg_lookup$goal_prob * 6 + xg_lookup$behind_prob, 3)

  out_path <- "inst/extdata/xg_lookup.json"
  write_json(xg_lookup, out_path, pretty = FALSE)
  cat("  ✓ Wrote", nrow(xg_lookup), "cells to", out_path, "\n")
  cat("  Grid: goal_x", range(xg_lookup$goal_x), "abs_y", range(xg_lookup$abs_y), "\n")
  cat("  xscore range:", range(xg_lookup$xscore), "\n")
}, error = function(e) {
  cat("  ✗ xG export failed:", e$message, "\n")
  cat("  You may need to check factor levels in the shot model.\n")
  cat("  Try: levels(shot_model$model$phase_of_play) to see valid values.\n")
})

# ══════════════════════════════════════════════════════════════
# 2. PSR — REFIT WITH LIVE-ONLY STATS
# ══════════════════════════════════════════════════════════════
cat("\n── 2. PSR Live Refit ───────────────────────────────────────\n")
cat("  TODO: Refit elastic net using only LIVE_STATS columns.\n")
cat("  Steps:\n")
cat("  1. Load the PSR training data (player-game stats + actual margins)\n")
cat("  2. Subset to LIVE_STATS + LIVE_DERIVED columns only\n")
cat("  3. Refit glmnet with same alpha/lambda selection\n")
cat("  4. Export new coefficients to inst/extdata/psr_live_coefficients.csv\n\n")

# Scaffold — fill in once you have the training data loaded:
#
# train_data <- get_psr_training_data()  # however PSR training data is loaded
#
# live_cols <- intersect(LIVE_STATS, colnames(train_data))
# derived <- train_data |>
#   mutate(
#     contested_poss_rate = contested_possessions / (contested_possessions + uncontested_possessions),
#     goal_accuracy = ifelse(shots_at_goal > 0, goals / shots_at_goal, 0)
#   )
#
# X <- as.matrix(derived[, c(live_cols, "contested_poss_rate", "goal_accuracy")])
# y <- derived$margin  # or whatever the PSR response variable is
#
# # Standardize
# sds <- apply(X, 2, sd, na.rm = TRUE)
# X_scaled <- scale(X)
#
# fit <- glmnet::cv.glmnet(X_scaled, y, alpha = 0.5)  # elastic net
# coefs <- coef(fit, s = "lambda.min")[-1]  # drop intercept
#
# psr_live <- data.frame(
#   stat_name = colnames(X),
#   beta = as.numeric(coefs),
#   sd = sds
# )
# write.csv(psr_live, "inst/extdata/psr_live_coefficients.csv", row.names = FALSE)

# ══════════════════════════════════════════════════════════════
# 3. EPV — WEIGHTS FOR LIVE-ONLY STATS
# ══════════════════════════════════════════════════════════════
cat("── 3. EPV Live Weights ────────────────────────────────────\n")
cat("  TODO: Create EPV variant using only LIVE_STATS columns.\n")
cat("  Steps:\n")
cat("  1. Take the existing EPV weight constants from R/constants.R\n")
cat("  2. Keep only weights for stats in LIVE_STATS\n")
cat("  3. Optionally re-calibrate weights to compensate for missing stats\n")
cat("  4. Export to inst/extdata/epv_live_weights.json\n\n")

# Current EPV weights that ARE available live (from R/constants.R):
epv_live_weights <- list(
  # Spoil/Tackle component (partial)
  tackles = EPV_TACKLE_WT,
  intercepts = EPV_INTERCEPTS_WT,
  one_percenters = EPV_ONE_PERCENTERS_WT,
  rebound50s = EPV_REBOUND50S_WT,
  frees_against = EPV_FREES_AGAINST_WT,
  # Missing: spoils, pressure_acts, def_half_pressure_acts

  # Hitout component (partial)
  hitouts = EPV_HITOUT_WT,
  # Missing: hitouts_to_advantage, ruck_contests

  # Disposal component (partial)
  inside50s = EPV_INSIDE50S_WT,
  clangers = EPV_CLANGERS_WT,
  score_involvements = EPV_SCORE_INVOLVEMENTS_WT,
  kicks = EPV_KICKS_WT,
  handballs = EPV_HANDBALLS_WT,
  metres_gained = EPV_METRES_GAINED_WT,
  turnovers = EPV_TURNOVERS_WT,
  goal_assists = EPV_GOAL_ASSISTS_WT,
  goals = EPV_GOALS_WT,
  behinds = EPV_BEHINDS_WT,
  shots_at_goal = EPV_SHOTS_AT_GOAL_WT,

  # Reception component (partial)
  contested_possessions = EPV_CONTESTED_POSS_WT,
  contested_marks = EPV_CONTESTED_MARKS_WT,
  marks_inside50 = EPV_MARKS_INSIDE50_WT,
  marks = EPV_MARKS_WT,
  uncontested_possessions = EPV_UNCONTESTED_POSS_WT,
  frees_for = EPV_FREES_FOR_WT
  # Missing: ground_ball_gets
)

cat("  Available EPV weights:", length(epv_live_weights), "stats\n")
cat("  Missing stats: spoils, pressure_acts, def_half_pressure_acts,\n")
cat("                 ground_ball_gets, hitouts_to_advantage, ruck_contests\n\n")

# Export current weights as-is (you can re-calibrate later)
tryCatch({
  out_path <- "inst/extdata/epv_live_weights.json"
  write_json(epv_live_weights, out_path, pretty = TRUE, auto_unbox = TRUE)
  cat("  ✓ Wrote EPV live weights to", out_path, "\n")
}, error = function(e) {
  cat("  ✗ EPV export failed:", e$message, "\n")
})

cat("\n══ Done ═══════════════════════════════════════════════════\n")
cat("Next steps:\n")
cat("  1. Run this script to generate xg_lookup.json\n")
cat("  2. Fill in the PSR refit scaffold with actual training data\n")
cat("  3. Optionally re-calibrate EPV weights for live-only stats\n")
cat("  4. Copy JSON files to inthegame-blog for browser use\n")
