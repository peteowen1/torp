# 06_train_psr_model.R
# ====================
# Train PSR model: separate home/away stat rating features for cleaner
# offense/defense decomposition.
#
# OSR model: home_score ~ home_ratings + away_ratings
# DSR model: away_score ~ home_ratings + away_ratings
# Margin model: margin ~ home_ratings + away_ratings
#
# Player attribution:
#   OSR = Σ β_own_off × rating/sd  (your ratings → your team scores more)
#   DSR = -Σ β_opp_def × rating/sd (your ratings → opponent scores less)
#
# Train: 2021-2024, Test: 2025

# Setup ----
devtools::load_all()
library(glmnet)
library(data.table)

cache_dir <- file.path("data-raw", "cache-stat-ratings")

rmse <- function(a, p) sqrt(mean((a - p)^2))
mae <- function(a, p) mean(abs(a - p))
r2 <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

# 1. Load & prep data ----
cli::cli_h1("Loading data")

stat_ratings <- as.data.table(load_player_stat_ratings(TRUE))
teams <- as.data.table(load_teams(TRUE))
fixtures <- as.data.table(load_fixtures(all = TRUE))

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

teams <- teams[is.na(position) | (position != "EMERG" & position != "SUB")]
teams[, round := as.integer(round_number)]
teams[, player_id := as.character(player_id)]
teams[, season := as.integer(season)]
stat_ratings[, player_id := as.character(player_id)]
stat_ratings[, season := as.integer(season)]
stat_ratings[, round := as.integer(round)]

stat_defs <- stat_rating_definitions()
exclude_stats <- c("cond_tog", "squad_selection", "dream_team_points", "rating_points")
all_rating_names <- setdiff(stat_defs$stat_name, exclude_stats)

# Prefer _adj_rating (opponent-adjusted) when available, fall back to _rating
adj_rating_cols <- intersect(paste0(all_rating_names, "_adj_rating"), names(stat_ratings))
raw_rating_cols <- intersect(paste0(all_rating_names, "_rating"), names(stat_ratings))

if (length(adj_rating_cols) >= length(raw_rating_cols) * 0.8) {
  # Use adjusted ratings — copy _adj_rating into _rating columns for
  # downstream compatibility (glmnet coef names, PSR coefficient export)
  for (i in seq_along(adj_rating_cols)) {
    raw_col <- sub("_adj_rating$", "_rating", adj_rating_cols[i])
    if (raw_col %in% names(stat_ratings)) {
      stat_ratings[, (raw_col) := get(adj_rating_cols[i])]
    }
  }
  cli::cli_inform("Using opponent-adjusted stat ratings ({length(adj_rating_cols)} stats, copied into _rating columns)")
}
rating_cols <- intersect(paste0(all_rating_names, "_rating"), names(stat_ratings))
cli::cli_inform("{length(rating_cols)} stat rating columns for PSR training")

ratings_join <- stat_ratings[, c("player_id", "season", "round", "pos_group", rating_cols), with = FALSE]
merged <- merge(teams, ratings_join, by = c("player_id", "season", "round"), all.x = TRUE)

# Impute
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

# Aggregate to team level
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

# 2. Build separate home/away feature matrix ----
cli::cli_h1("Building separate home/away feature matrix")

home <- team_ratings[team_type == "home"]
away <- team_ratings[team_type == "away"]

home_cols <- paste0("home_", rating_cols)
away_cols <- paste0("away_", rating_cols)

setnames(home, rating_cols, home_cols)
setnames(away, rating_cols, away_cols)

match_df <- merge(
  home[, c("match_id", "season", "round", home_cols), with = FALSE],
  away[, c("match_id", away_cols), with = FALSE],
  by = "match_id"
)

match_df <- merge(match_df,
  fixtures_margin[, .(match_id, home_score, away_score, home_margin, match_date)],
  by = "match_id")

cli::cli_inform("Match rows: {nrow(match_df)}, Features: {length(home_cols) + length(away_cols)} (52 home + 52 away)")

# Weights
anchor_date <- as.Date("2024-12-31")
match_df[, weightz := exp(as.numeric(-(anchor_date - match_date)) / MATCH_WEIGHT_DECAY_DAYS)]
match_df[, weightz := weightz / mean(weightz, na.rm = TRUE)]

# Train/test split
train_idx <- which(match_df$season < 2025)
test_idx <- which(match_df$season >= 2025)

# Standardize by training SD
all_feat_cols <- c(home_cols, away_cols)
X_raw <- as.matrix(match_df[, all_feat_cols, with = FALSE])
train_sds <- apply(X_raw[train_idx, ], 2, sd)
train_sds[train_sds == 0] <- 1
X <- sweep(X_raw, 2, train_sds, "/")

X_train <- X[train_idx, ]
X_test <- X[test_idx, ]
w_train <- match_df$weightz[train_idx]

y_margin_train <- match_df$home_margin[train_idx]
y_margin_test <- match_df$home_margin[test_idx]
y_off_train <- match_df$home_score[train_idx]
y_off_test <- match_df$home_score[test_idx]
y_def_train <- match_df$away_score[train_idx]
y_def_test <- match_df$away_score[test_idx]

cli::cli_inform("Train: {length(train_idx)}, Test: {length(test_idx)}")

# CV folds
train_seasons <- match_df$season[train_idx]
foldid <- as.integer(factor(train_seasons, levels = sort(unique(train_seasons))))

# 3. Fit models ----
cli::cli_h1("Fitting models (104 features)")

alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)

fit_model <- function(y_tr, y_te, label) {
  best_cvm <- Inf
  best_fit <- NULL
  best_a <- NULL

  for (a in alpha_grid) {
    set.seed(42)
    cv_f <- cv.glmnet(X_train, y_tr, weights = w_train, alpha = a,
                       foldid = foldid, type.measure = "mse", standardize = FALSE)
    if (min(cv_f$cvm) < best_cvm) {
      best_cvm <- min(cv_f$cvm)
      best_fit <- cv_f
      best_a <- a
    }
  }

  mdl <- glmnet(X_train, y_tr, weights = w_train, alpha = best_a,
                 lambda = best_fit$lambda.min, standardize = FALSE)

  p_tr <- as.numeric(predict(mdl, X_train))
  p_te <- as.numeric(predict(mdl, X_test))

  cli::cli_inform("{label}: alpha={best_a}, CV RMSE={round(sqrt(best_cvm), 2)}, Train RMSE={round(rmse(y_tr, p_tr), 2)}, Test RMSE={round(rmse(y_te, p_te), 2)}, Test R2={round(r2(y_te, p_te), 3)}")

  cs <- as.matrix(coef(mdl))

  list(model = mdl, coefs = cs, best_alpha = best_a, pred_test = p_te,
       test_rmse = rmse(y_te, p_te))
}

margin_fit <- fit_model(y_margin_train, y_margin_test, "Margin (PSR)")
off_fit <- fit_model(y_off_train, y_off_test, "Offense (OSR)")
def_fit <- fit_model(y_def_train, y_def_test, "Defense (DSR)")

# PSR match prediction (off - def as margin proxy)
pred_psr_combined <- off_fit$pred_test - def_fit$pred_test

cat(sprintf("\nPSR (off - def): Test RMSE=%.2f, MAE=%.2f, R2=%.3f\n",
  rmse(y_margin_test, pred_psr_combined), mae(y_margin_test, pred_psr_combined),
  r2(y_margin_test, pred_psr_combined)))

# 4. Extract offense/defense coefficients ----
cli::cli_h1("Extracting player-level coefficients")

# Offensive model coefficients
off_cs <- off_fit$coefs
# home_* coefficients = "own ratings → own score" = OSR
# away_* coefficients = "opponent ratings → own score" = opponent's defensive impact
off_home_beta <- off_cs[paste0("home_", rating_cols), 1]
off_away_beta <- off_cs[paste0("away_", rating_cols), 1]

# Defensive model coefficients
def_cs <- def_fit$coefs
# home_* coefficients = "own ratings → opponent score" = own defensive impact
# away_* coefficients = "opponent ratings → opponent score" = opponent's OSR
def_home_beta <- def_cs[paste0("home_", rating_cols), 1]
def_away_beta <- def_cs[paste0("away_", rating_cols), 1]

# For a player on the HOME team:
#   OSR = off_home_beta × rating (my ratings → my team scores more)
#   DSR = -def_home_beta × rating (my ratings → opponent scores less; negate so positive = good)
#
# For a player on the AWAY team:
#   OSR = def_away_beta × rating (my ratings → my team scores more)
#   DSR = -off_away_beta × rating (my ratings → opponent scores less; negate so positive = good)
#
# For symmetric attribution, average the home and away perspectives:
#   OSR_beta = (off_home_beta + def_away_beta) / 2
#   DSR_beta = -(def_home_beta + off_away_beta) / 2

osr_beta <- (off_home_beta + def_away_beta) / 2
dsr_beta <- -(def_home_beta + off_away_beta) / 2

# SDs for the home_ columns (player stat ratings get divided by these)
home_sds <- train_sds[paste0("home_", rating_cols)]

# Build coef_dfs for calculate_psr()
osr_coef_df <- data.frame(
  stat_name = sub("_rating$", "", rating_cols),
  beta = as.numeric(osr_beta),
  sd = as.numeric(home_sds),
  stringsAsFactors = FALSE
)

dsr_coef_df <- data.frame(
  stat_name = sub("_rating$", "", rating_cols),
  beta = as.numeric(dsr_beta),
  sd = as.numeric(home_sds),
  stringsAsFactors = FALSE
)

# Margin model: average home and away perspectives too
margin_cs <- margin_fit$coefs
margin_home_beta <- margin_cs[paste0("home_", rating_cols), 1]
margin_away_beta <- margin_cs[paste0("away_", rating_cols), 1]
psr_beta <- (margin_home_beta - margin_away_beta) / 2

psr_coef_df <- data.frame(
  stat_name = sub("_rating$", "", rating_cols),
  beta = as.numeric(psr_beta),
  sd = as.numeric(home_sds),
  stringsAsFactors = FALSE
)

cat("\n--- OSR Top 10 Coefficients ---\n")
print(head(osr_coef_df[order(-abs(osr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

cat("\n--- DSR Top 10 Coefficients ---\n")
print(head(dsr_coef_df[order(-abs(dsr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

cat("\n--- PSR Top 10 Coefficients ---\n")
print(head(psr_coef_df[order(-abs(psr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

# 4b. Mask cross-model stat leakage ----
# Zero out scoring-category stats from DSR (defensive model should not reward
# attacking output) and defensive-category stats from OSR (offensive model
# should not reward defensive actions). The additive reconciliation in
# calculate_psr_components() ensures osr + dsr = psr regardless.
scoring_stats <- c("goals", "behinds", "shots_at_goal", "score_involvements",
                    "goal_assists", "goal_accuracy")
defensive_stats <- c("tackles", "spoils", "intercepts", "one_percenters",
                      "intercept_marks", "tackles_inside50")

dsr_coef_df$beta[dsr_coef_df$stat_name %in% scoring_stats] <- 0
osr_coef_df$beta[osr_coef_df$stat_name %in% defensive_stats] <- 0

# 5. Calculate player ratings ----
cli::cli_h1("Player ratings")

osr <- calculate_psr(stat_ratings, osr_coef_df, center = TRUE)
setnames(osr, c("psr_raw", "psr"), c("osr_raw", "osr"))

dsr <- calculate_psr(stat_ratings, dsr_coef_df, center = TRUE)
setnames(dsr, c("psr_raw", "psr"), c("dsr_raw", "dsr"))

psr_margin <- calculate_psr(stat_ratings, psr_coef_df, center = TRUE)
setnames(psr_margin, c("psr_raw", "psr"), c("margin_psr_raw", "margin_psr"))

id_cols <- intersect(names(osr), names(dsr))
id_cols <- setdiff(id_cols, c("osr_raw", "osr", "dsr_raw", "dsr"))
all_ratings <- merge(merge(psr_margin, osr, by = id_cols, all = TRUE),
                     dsr, by = id_cols, all = TRUE)
all_ratings[, psr := osr + dsr]

latest <- all_ratings[, .SD[round == max(round)], by = season]
latest <- latest[season == max(season)]

cat("\n--- Top 20 by PSR ---\n")
print(head(latest[order(-psr),
  .(player_name, pos_group, osr = round(osr, 2), dsr = round(dsr, 2),
    psr = round(psr, 2), margin_psr = round(margin_psr, 2))], 20), row.names = FALSE)

cat("\n--- Bottom 20 by PSR ---\n")
print(tail(latest[order(-psr),
  .(player_name, pos_group, osr = round(osr, 2), dsr = round(dsr, 2),
    psr = round(psr, 2), margin_psr = round(margin_psr, 2))], 20), row.names = FALSE)

cat(sprintf("\nOSR vs DSR correlation: %.3f\n",
  cor(latest$osr, latest$dsr, use = "complete.obs")))

# Best offensive players
cat("\n--- Top 10 by OSR ---\n")
print(head(latest[order(-osr),
  .(player_name, pos_group, osr = round(osr, 2), dsr = round(dsr, 2))], 10),
  row.names = FALSE)

# Best defensive players
cat("\n--- Top 10 by DSR ---\n")
print(head(latest[order(-dsr),
  .(player_name, pos_group, osr = round(osr, 2), dsr = round(dsr, 2))], 10),
  row.names = FALSE)

# 6. Save outputs ----
cli::cli_h1("Saving outputs")

write.csv(osr_coef_df, file.path(cache_dir, "osr_coefficients.csv"), row.names = FALSE)
write.csv(dsr_coef_df, file.path(cache_dir, "dsr_coefficients.csv"), row.names = FALSE)
write.csv(psr_coef_df, file.path(cache_dir, "psr_coefficients.csv"), row.names = FALSE)

model_out <- list(
  margin_model = margin_fit$model, off_model = off_fit$model, def_model = def_fit$model,
  psr_coef_df = psr_coef_df, osr_coef_df = osr_coef_df, dsr_coef_df = dsr_coef_df,
  margin_intercept = margin_fit$coefs[1, 1],
  off_intercept = off_fit$coefs[1, 1], def_intercept = def_fit$coefs[1, 1],
  train_sds = train_sds, home_sds = home_sds,
  rating_cols = rating_cols,
  train_seasons = sort(unique(train_seasons)),
  test_rmse_margin = margin_fit$test_rmse,
  test_rmse_psr = rmse(y_margin_test, pred_psr_combined)
)
saveRDS(model_out, file.path(cache_dir, "psr_model.rds"))

cli::cli_alert_success("Saved outputs")
cli::cli_alert_success("Done!")
