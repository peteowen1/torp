# 05_compare_psr_models.R
# ======================
# Compare v1 (diff features) vs v2 (home/away features) PSR models on holdout.
#
# Evaluation: which model's team-level PSR diff better predicts xscore_diff
# (expected score differential from shot-level xG — a less noisy target than
# actual margin, so it better measures true team quality).
#
# If v1 margin PSR is superior, we use v1 coefficients as the authoritative
# total PSR, but decompose into OSR/DSR via v2's offense/defense models
# (reconciled via calculate_psr_components() residual adjustment).
#
# Train: 2021-2024, Test: 2025

# Setup ----
devtools::load_all()
library(glmnet)
library(data.table)

cache_dir <- file.path("data-raw", "cache-stat-ratings")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

rmse <- function(a, p) sqrt(mean((a - p)^2))
mae  <- function(a, p) mean(abs(a - p))
r2   <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

# 1. Load data (shared) ----
cli::cli_h1("Loading data")

stat_ratings <- as.data.table(load_player_stat_ratings(TRUE))
teams <- as.data.table(load_teams(TRUE))
fixtures <- as.data.table(load_fixtures(all = TRUE))
xg_df <- as.data.table(load_xg(TRUE))

# Extract margins + join xscore_diff
fixtures_margin <- fixtures[
  !is.na(home_score) & !is.na(away_score),
  .(match_id,
    season = as.integer(season),
    round = as.integer(round_number),
    home_score = as.numeric(home_score),
    away_score = as.numeric(away_score),
    home_margin = home_score - away_score,
    match_date = as.Date(substr(utc_start_time, 1, 10)))
]

# Join xscore_diff from xg data
xg_match <- xg_df[, .(match_id, xscore_diff = home_xscore - away_xscore)]
fixtures_margin <- merge(fixtures_margin, xg_match, by = "match_id", all.x = TRUE)

n_with_xg <- sum(!is.na(fixtures_margin$xscore_diff))
cli::cli_inform("Matches with xscore_diff: {n_with_xg}/{nrow(fixtures_margin)}")

# Filter teams
teams <- teams[is.na(position) | (position != "EMERG" & position != "SUB")]
teams[, round := as.integer(round_number)]
teams[, player_id := as.character(player_id)]
teams[, season := as.integer(season)]
stat_ratings[, player_id := as.character(player_id)]
stat_ratings[, season := as.integer(season)]
stat_ratings[, round := as.integer(round)]

# 2. Join stat ratings to lineups ----
cli::cli_h1("Joining stat ratings to lineups")

stat_defs <- stat_rating_definitions()
exclude_stats <- c("cond_tog", "squad_selection", "dream_team_points", "rating_points")
all_rating_names <- setdiff(stat_defs$stat_name, exclude_stats)
rating_cols <- intersect(paste0(all_rating_names, "_rating"), names(stat_ratings))

ratings_join <- stat_ratings[, c("player_id", "season", "round", "pos_group", rating_cols), with = FALSE]
merged <- merge(teams, ratings_join, by = c("player_id", "season", "round"), all.x = TRUE)

# Impute missing with position-group means, then global means
pos_means <- merged[!is.na(pos_group), lapply(.SD, mean, na.rm = TRUE),
                    by = pos_group, .SDcols = rating_cols]
global_means <- merged[, lapply(.SD, mean, na.rm = TRUE), .SDcols = rating_cols]
for (sc in rating_cols) {
  na_idx <- which(is.na(merged[[sc]]))
  if (length(na_idx) > 0) {
    for (pg in unique(pos_means$pos_group)) {
      pg_idx <- na_idx[merged$pos_group[na_idx] == pg & !is.na(merged$pos_group[na_idx])]
      if (length(pg_idx) > 0) merged[pg_idx, (sc) := pos_means[pos_group == pg, get(sc)]]
    }
    still_na <- which(is.na(merged[[sc]]))
    if (length(still_na) > 0) merged[still_na, (sc) := global_means[[sc]]]
  }
}

# 3. Aggregate to team level (top 22) ----
cli::cli_h1("Aggregating to team level")

merged[, .total_rating := rowSums(.SD, na.rm = TRUE), .SDcols = rating_cols]
team_ratings <- merged[order(-.total_rating)][
  , head(.SD, 22), by = .(match_id, team_id)
][, {
  out <- list(n_players = .N)
  for (sc in rating_cols) out[[sc]] <- sum(get(sc), na.rm = TRUE)
  out
}, by = .(match_id, team_id, season, round)]

team_ratings <- merge(team_ratings,
  fixtures[, .(match_id, home_team_id, away_team_id)],
  by = "match_id", all.x = TRUE)
team_ratings[, team_type := fifelse(team_id == home_team_id, "home", "away")]

# Pivot home/away
home <- team_ratings[team_type == "home"]
away <- team_ratings[team_type == "away"]
home_cols <- paste0("home_", rating_cols)
away_cols <- paste0("away_", rating_cols)
setnames(home, rating_cols, home_cols)
setnames(away, rating_cols, away_cols)

match_df <- merge(
  home[, c("match_id", "season", "round", home_cols), with = FALSE],
  away[, c("match_id", away_cols), with = FALSE],
  by = "match_id")
match_df <- merge(match_df,
  fixtures_margin[, .(match_id, home_score, away_score, home_margin, match_date, xscore_diff)],
  by = "match_id")

cli::cli_inform("Match rows: {nrow(match_df)} ({sum(!is.na(match_df$xscore_diff))} with xscore_diff)")

# Weights + train/test
anchor_date <- as.Date("2024-12-31")
match_df[, weightz := exp(as.numeric(-(anchor_date - match_date)) / MATCH_WEIGHT_DECAY_DAYS)]
match_df[, weightz := weightz / mean(weightz, na.rm = TRUE)]

train_idx <- which(match_df$season < 2025)
test_idx <- which(match_df$season >= 2025)
w_train <- match_df$weightz[train_idx]

# CV folds (leave-one-season-out)
train_seasons <- match_df$season[train_idx]
foldid <- as.integer(factor(train_seasons, levels = sort(unique(train_seasons))))

# 4. Build feature matrices ----
cli::cli_h1("Building feature matrices")

# --- v1: diff features (52 cols) ---
diff_col_names <- sub("_rating$", "_diff", rating_cols)
X_diff_raw <- as.matrix(match_df[, home_cols, with = FALSE]) -
              as.matrix(match_df[, away_cols, with = FALSE])
colnames(X_diff_raw) <- diff_col_names

diff_train_sds <- apply(X_diff_raw[train_idx, ], 2, sd)
diff_train_sds[diff_train_sds == 0] <- 1
X_diff <- sweep(X_diff_raw, 2, diff_train_sds, "/")

# --- v2: separate home/away features (104 cols) ---
all_feat_cols <- c(home_cols, away_cols)
X_ha_raw <- as.matrix(match_df[, all_feat_cols, with = FALSE])

ha_train_sds <- apply(X_ha_raw[train_idx, ], 2, sd)
ha_train_sds[ha_train_sds == 0] <- 1
X_ha <- sweep(X_ha_raw, 2, ha_train_sds, "/")

cli::cli_inform("v1 diff features: {ncol(X_diff)}, v2 home/away features: {ncol(X_ha)}")

# 5. Shared training helper ----
alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)

fit_glmnet <- function(X_tr, y_tr, w_tr, fold_id, label) {
  best_cvm <- Inf
  best_fit <- NULL
  best_a <- NULL

  for (a in alpha_grid) {
    set.seed(42)
    cv_f <- cv.glmnet(X_tr, y_tr, weights = w_tr, alpha = a,
                       foldid = fold_id, type.measure = "mse", standardize = FALSE)
    if (min(cv_f$cvm) < best_cvm) {
      best_cvm <- min(cv_f$cvm)
      best_fit <- cv_f
      best_a <- a
    }
  }

  mdl <- glmnet(X_tr, y_tr, weights = w_tr, alpha = best_a,
                 lambda = best_fit$lambda.min, standardize = FALSE)

  n_nonzero <- sum(as.matrix(coef(mdl))[-1, 1] != 0)
  cli::cli_inform("  {label}: alpha={best_a}, CV RMSE={round(sqrt(best_cvm), 2)}, non-zero={n_nonzero}")

  list(model = mdl, cv_fit = best_fit, alpha = best_a, cv_rmse = sqrt(best_cvm))
}

# 6. Train v1 models (diff features) ----
cli::cli_h1("Training v1 models (diff features)")

y_margin_train <- match_df$home_margin[train_idx]
y_off_train <- match_df$home_score[train_idx]
y_def_train <- match_df$away_score[train_idx]

X_diff_train <- X_diff[train_idx, ]
X_diff_test <- X_diff[test_idx, ]

v1_margin <- fit_glmnet(X_diff_train, y_margin_train, w_train, foldid, "v1 Margin")
v1_off <- fit_glmnet(X_diff_train, y_off_train, w_train, foldid, "v1 Offense")
v1_def <- fit_glmnet(X_diff_train, y_def_train, w_train, foldid, "v1 Defense")

# 7. Train v2 models (home/away features) ----
cli::cli_h1("Training v2 models (home/away features)")

X_ha_train <- X_ha[train_idx, ]
X_ha_test <- X_ha[test_idx, ]

v2_margin <- fit_glmnet(X_ha_train, y_margin_train, w_train, foldid, "v2 Margin")
v2_off <- fit_glmnet(X_ha_train, y_off_train, w_train, foldid, "v2 Offense")
v2_def <- fit_glmnet(X_ha_train, y_def_train, w_train, foldid, "v2 Defense")

# 8. Extract coefficients ----
cli::cli_h1("Extracting player-level coefficients")

# v1: diff coefficients map directly to player PSR
extract_v1_coefs <- function(model, sds) {
  cs <- as.matrix(coef(model$model))
  data.frame(
    stat_name = sub("_diff$", "", rownames(cs)[-1]),
    beta = cs[-1, 1],
    sd = as.numeric(sds),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

v1_psr_coefs <- extract_v1_coefs(v1_margin, diff_train_sds)
v1_osr_coefs <- extract_v1_coefs(v1_off, diff_train_sds)

# For v1 DSR: negate betas (positive beta = more opp scoring = bad defense)
v1_dsr_coefs <- extract_v1_coefs(v1_def, diff_train_sds)
v1_dsr_coefs$beta <- -v1_dsr_coefs$beta

# v2: symmetric extraction (same as pannaverse)
extract_v2_coefs <- function(model, sds, type) {
  cs <- as.matrix(coef(model$model))[-1, 1]
  n <- length(rating_cols)
  home_coefs <- cs[1:n]
  away_coefs <- cs[(n + 1):(2 * n)]
  home_sds <- sds[home_cols]

  player_beta <- if (type == "margin") {
    (home_coefs - away_coefs) / 2
  } else if (type == "offense") {
    (home_coefs + away_coefs) / 2
  } else {
    -(home_coefs + away_coefs) / 2
  }

  data.frame(
    stat_name = sub("_rating$", "", rating_cols),
    beta = as.numeric(player_beta),
    sd = as.numeric(home_sds),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

v2_psr_coefs <- extract_v2_coefs(v2_margin, ha_train_sds, "margin")
v2_osr_coefs <- extract_v2_coefs(v2_off, ha_train_sds, "offense")
v2_dsr_coefs <- extract_v2_coefs(v2_def, ha_train_sds, "defense")

# 9. Calculate player-level PSR for each model ----
cli::cli_h1("Calculating player PSRs")

psr_v1 <- calculate_psr(stat_ratings, v1_psr_coefs, center = TRUE)
setnames(psr_v1, "psr", "psr_v1")

psr_v2 <- calculate_psr(stat_ratings, v2_psr_coefs, center = TRUE)
setnames(psr_v2, "psr", "psr_v2")

# 10. Aggregate to team PSR and compute PSR diff ----
cli::cli_h1("Evaluating on holdout")

# Helper: team-level PSR diff for a given PSR column
team_psr_diff <- function(psr_dt, psr_col, test_match_ids) {
  psr_dt <- data.table::copy(psr_dt)
  psr_dt[, player_id := as.character(player_id)]
  psr_dt[, season := as.integer(season)]
  psr_dt[, round := as.integer(round)]

  teams_dt <- as.data.table(load_teams(TRUE))
  teams_dt <- teams_dt[is.na(position) | (position != "EMERG" & position != "SUB")]
  teams_dt[, round := as.integer(round_number)]
  teams_dt[, player_id := as.character(player_id)]
  teams_dt[, season := as.integer(season)]

  m <- merge(teams_dt, psr_dt[, c("player_id", "season", "round", psr_col), with = FALSE],
             by = c("player_id", "season", "round"), all.x = TRUE)
  m[is.na(get(psr_col)), (psr_col) := 0]

  # Top 22 by PSR per match-team, sum
  team_psr <- m[order(-get(psr_col))][
    , head(.SD, 22), by = .(match_id, team_id)
  ][, .(team_psr = sum(get(psr_col), na.rm = TRUE)),
    by = .(match_id, team_id, season)]

  team_psr <- merge(team_psr,
    fixtures[, .(match_id, home_team_id, away_team_id)],
    by = "match_id", all.x = TRUE)
  team_psr[, team_type := fifelse(team_id == home_team_id, "home", "away")]

  home_psr <- team_psr[team_type == "home", .(match_id, home_psr = team_psr)]
  away_psr <- team_psr[team_type == "away", .(match_id, away_psr = team_psr)]
  match_psr <- merge(home_psr, away_psr, by = "match_id")
  match_psr[, psr_diff := home_psr - away_psr]

  match_psr[match_id %in% test_match_ids]
}

test_match_ids <- match_df$match_id[test_idx]

# TODO: implement evaluate_model_on_holdout() ----
# This function takes both models' team PSR diffs and the actual outcomes,
# then produces a comparison table.
#
# You'll want to consider:
#   - Correlation with xscore_diff (primary metric — less noisy)
#   - Correlation with home_margin (secondary metric — the actual outcome)
#   - RMSE of a simple linear fit: lm(xscore_diff ~ psr_diff)
#   - Maybe: does combining v1+v2 beat either alone?
#
# The function signature and data are set up below. Fill in the evaluation
# logic (roughly 5-10 lines) in the function body.

evaluate_model_on_holdout <- function(v1_match, v2_match, actuals) {
  # v1_match / v2_match: data.tables with columns match_id, psr_diff
  # actuals: data.table with match_id, home_margin, xscore_diff

  eval_dt <- merge(merge(
    v1_match[, .(match_id, v1_diff = psr_diff)],
    v2_match[, .(match_id, v2_diff = psr_diff)],
    by = "match_id"),
    actuals, by = "match_id")

  # Drop matches missing xscore_diff
  eval_dt <- eval_dt[!is.na(xscore_diff)]

  eval_dt[, ensemble_diff := (v1_diff + v2_diff) / 2]

  models <- c("v1_diff", "v2_diff", "ensemble_diff")
  targets <- c("xscore_diff", "home_margin")

  cat(sprintf("\nHoldout: %d matches\n\n", nrow(eval_dt)))
  cat(sprintf("%-18s  %10s  %8s  %8s  %6s\n", "Model", "Target", "Cor", "RMSE", "R2"))
  cat(paste(rep("-", 56), collapse = ""), "\n")

  for (m in models) {
    for (tgt in targets) {
      r <- cor(eval_dt[[m]], eval_dt[[tgt]], use = "complete.obs")
      fit <- lm(reformulate(m, tgt), data = eval_dt)
      pred <- predict(fit)
      res_rmse <- rmse(eval_dt[[tgt]], pred)
      res_r2 <- r2(eval_dt[[tgt]], pred)
      cat(sprintf("%-18s  %10s  %8.3f  %8.2f  %6.3f\n", m, tgt, r, res_rmse, res_r2))
    }
  }

  # Winner
  v1_cor <- cor(eval_dt$v1_diff, eval_dt$xscore_diff, use = "complete.obs")
  v2_cor <- cor(eval_dt$v2_diff, eval_dt$xscore_diff, use = "complete.obs")
  ens_cor <- cor(eval_dt$ensemble_diff, eval_dt$xscore_diff, use = "complete.obs")
  best <- which.max(c(v1 = v1_cor, v2 = v2_cor, ensemble = ens_cor))
  cat(sprintf("\nBest xscore_diff predictor: %s (r=%.3f)\n",
    c("v1", "v2", "ensemble")[best], c(v1_cor, v2_cor, ens_cor)[best]))

  invisible(eval_dt)
}

# Compute team PSR diffs on holdout
cli::cli_inform("Computing team PSR diffs on holdout...")
v1_match <- team_psr_diff(psr_v1, "psr_v1", test_match_ids)
v2_match <- team_psr_diff(psr_v2, "psr_v2", test_match_ids)

actuals <- match_df[test_idx, .(match_id, home_margin, xscore_diff)]

# Run evaluation
evaluate_model_on_holdout(v1_match, v2_match, actuals)

# 11. Save winning coefficients ----
cli::cli_h1("Saving outputs")

# After reviewing results above, save the winning PSR + v2 OSR/DSR
# coefficients. The recommendation is:
#   - PSR coefs: whichever model (v1 or v2) has higher xscore_diff correlation
#   - OSR/DSR coefs: always from v2 (cleaner decomposition)
#   - calculate_psr_components() handles the reconciliation automatically

# Uncomment and adjust once you've decided which wins:
# write.csv(v1_psr_coefs, file.path(cache_dir, "psr_coefficients.csv"), row.names = FALSE)  # if v1 wins
# write.csv(v2_psr_coefs, file.path(cache_dir, "psr_coefficients.csv"), row.names = FALSE)  # if v2 wins
# write.csv(v2_osr_coefs, file.path(cache_dir, "osr_coefficients.csv"), row.names = FALSE)
# write.csv(v2_dsr_coefs, file.path(cache_dir, "dsr_coefficients.csv"), row.names = FALSE)
