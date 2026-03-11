# 05_train_psr_model.R
# ====================
# Train PSR (Player Skill Rating) model: glmnet predicting match margin
# from team-aggregated player skill diffs, then apportion coefficients
# back to individual players.
#
# Train: 2021-2024, Test: 2025
# Output: cache-skills/psr_coefficients.csv, cache-skills/psr_model.rds

# Setup ----
devtools::load_all()
library(glmnet)
library(data.table)

cache_dir <- file.path("data-raw", "cache-skills")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# 1. Load data ----
cli::cli_h1("Loading data")

skills <- as.data.table(load_player_skills(TRUE))
teams <- as.data.table(load_teams(TRUE))
fixtures <- as.data.table(load_fixtures(all = TRUE))

cli::cli_inform("Skills: {nrow(skills)} rows, {length(unique(skills$season))} seasons")
cli::cli_inform("Teams: {nrow(teams)} rows")
cli::cli_inform("Fixtures: {nrow(fixtures)} rows")

# Extract margins from fixtures (home perspective only)
fixtures_margin <- fixtures[
  !is.na(home.score.totalScore) & !is.na(away.score.totalScore),
  .(providerId,
    season = as.integer(compSeason.year),
    round = as.integer(round.roundNumber),
    home_team_name = home.team.name,
    away_team_name = away.team.name,
    home_score = as.numeric(home.score.totalScore),
    away_score = as.numeric(away.score.totalScore),
    home_margin = home.score.totalScore - away.score.totalScore,
    match_date = as.Date(substr(utcStartTime, 1, 10)))
]

cli::cli_inform("Fixtures with scores: {nrow(fixtures_margin)} matches")

# Filter teams: exclude EMERG/SUB
teams <- teams[is.na(position) | (position != "EMERG" & position != "SUB")]

# load_teams() now returns snake_case columns (player_id, round_number, season)
if (!"round" %in% names(teams) && "round_number" %in% names(teams)) {
  teams[, round := as.integer(round_number)]
}
# Ensure consistent types
teams[, player_id := as.character(player_id)]
teams[, season := as.integer(season)]
skills[, player_id := as.character(player_id)]
skills[, season := as.integer(season)]
skills[, round := as.integer(round)]

# 2. Join skills to lineups ----
cli::cli_h1("Joining skills to lineups")

# Identify skill columns (exclude time_on_ground, dream_team_points, rating_points)
stat_defs <- skill_stat_definitions()
all_skill_names <- stat_defs$stat_name
exclude_stats <- c("cond_tog", "squad_selection", "dream_team_points", "rating_points")
all_skill_names <- setdiff(all_skill_names, exclude_stats)
skill_cols <- paste0(all_skill_names, "_skill")
skill_cols <- intersect(skill_cols, names(skills))

cli::cli_inform("Using {length(skill_cols)} skill columns (excl. {paste(exclude_stats, collapse = ', ')})")

# Select minimal columns from skills for join
skills_join <- skills[, c("player_id", "season", "round", "pos_group", skill_cols), with = FALSE]

# Join on player_id + season + round
merged <- merge(
  teams,
  skills_join,
  by = c("player_id", "season", "round"),
  all.x = TRUE
)

n_matched <- sum(!is.na(merged[[skill_cols[1]]]))
n_total <- nrow(merged)
cli::cli_inform("Skill match rate: {n_matched}/{n_total} ({round(100 * n_matched / n_total, 1)}%)")

# Impute missing skills with position-group means
cli::cli_h2("Imputing missing skills")

# Compute position-group means across all data
pos_means <- merged[!is.na(pos_group), lapply(.SD, mean, na.rm = TRUE),
                    by = pos_group, .SDcols = skill_cols]

# Global fallback means
global_means <- merged[, lapply(.SD, mean, na.rm = TRUE), .SDcols = skill_cols]

for (sc in skill_cols) {
  na_idx <- which(is.na(merged[[sc]]))
  if (length(na_idx) > 0) {
    # Try position-group imputation
    for (pg in unique(pos_means$pos_group)) {
      pg_idx <- na_idx[merged$pos_group[na_idx] == pg & !is.na(merged$pos_group[na_idx])]
      if (length(pg_idx) > 0) {
        merged[pg_idx, (sc) := pos_means[pos_group == pg, get(sc)]]
      }
    }
    # Remaining NAs: global mean
    still_na <- which(is.na(merged[[sc]]))
    if (length(still_na) > 0) {
      merged[still_na, (sc) := global_means[[sc]]]
    }
  }
}

n_still_na <- sum(is.na(merged[, skill_cols, with = FALSE]))
cli::cli_inform("Remaining NAs after imputation: {n_still_na}")

# 3. Aggregate to team level ----
cli::cli_h1("Aggregating to team level")

# Total skill per player (for top-22 selection)
merged[, .total_skill := rowSums(.SD, na.rm = TRUE), .SDcols = skill_cols]

# Top 22 by total skill per match-team, then sum skills
team_skills <- merged[
  order(-.total_skill)
][, head(.SD, 22), by = .(providerId, teamId)
][, {
  out <- list(n_players = .N)
  for (sc in skill_cols) {
    out[[sc]] <- sum(get(sc), na.rm = TRUE)
  }
  out
}, by = .(providerId, teamId, season, round)]

cli::cli_inform("Team-match skill rows: {nrow(team_skills)}")

# Identify home/away teams by joining to fixtures
team_skills <- merge(
  team_skills,
  fixtures[, .(providerId,
               home_teamId = home.team.providerId,
               away_teamId = away.team.providerId)],
  by = "providerId",
  all.x = TRUE
)
team_skills[, team_type := fifelse(teamId == home_teamId, "home", "away")]

# Pivot to one row per match: home vs away skill diffs
home <- team_skills[team_type == "home"]
away <- team_skills[team_type == "away"]

setnames(home, skill_cols, paste0(skill_cols, "_home"))
setnames(away, skill_cols, paste0(skill_cols, "_away"))

match_df <- merge(
  home[, c("providerId", "season", "round", paste0(skill_cols, "_home")), with = FALSE],
  away[, c("providerId", paste0(skill_cols, "_away")), with = FALSE],
  by = "providerId"
)

# Compute diffs
diff_cols <- paste0(all_skill_names[paste0(all_skill_names, "_skill") %in% skill_cols], "_diff")
for (i in seq_along(skill_cols)) {
  diff_col <- diff_cols[i]
  home_col <- paste0(skill_cols[i], "_home")
  away_col <- paste0(skill_cols[i], "_away")
  match_df[, (diff_col) := get(home_col) - get(away_col)]
}

# Join margin + scores
match_df <- merge(match_df,
                  fixtures_margin[, .(providerId, home_score, away_score, home_margin, match_date)],
                  by = "providerId")

cli::cli_inform("Match model rows: {nrow(match_df)}")

# 4. Build model matrix ----
cli::cli_h1("Building model matrix")

X_raw <- as.matrix(match_df[, diff_cols, with = FALSE])
y <- match_df$home_margin

# Exponential decay weights anchored to end of 2024
anchor_date <- as.Date("2024-12-31")
match_df[, weightz := exp(as.numeric(-(anchor_date - match_date)) / MATCH_WEIGHT_DECAY_DAYS)]
match_df[, weightz := weightz / mean(weightz, na.rm = TRUE)]
w <- match_df$weightz

# Train/test split
train_idx <- which(match_df$season < 2025)
test_idx <- which(match_df$season >= 2025)

# Standardize diff columns using training-set SDs
# (diffs are naturally mean ~0, so only scale by SD)
train_sds <- apply(X_raw[train_idx, ], 2, sd)
train_sds[train_sds == 0] <- 1
X <- sweep(X_raw, 2, train_sds, "/")

cli::cli_inform("Standardized {ncol(X)} diff columns by training SD")

X_train <- X[train_idx, ]
y_train <- y[train_idx]
w_train <- w[train_idx]
X_test <- X[test_idx, ]
y_test <- y[test_idx]

cli::cli_inform("Train: {length(train_idx)} matches ({paste(sort(unique(match_df$season[train_idx])), collapse = ', ')})")
cli::cli_inform("Test: {length(test_idx)} matches ({paste(sort(unique(match_df$season[test_idx])), collapse = ', ')})")

# 5. Fit glmnet ----
cli::cli_h1("Fitting glmnet with CV")

# Leave-one-season-out folds on training set
train_seasons <- match_df$season[train_idx]
unique_seasons <- sort(unique(train_seasons))
foldid <- as.integer(factor(train_seasons, levels = unique_seasons))

cli::cli_inform("CV folds (leave-one-season-out): {paste(unique_seasons, collapse = ', ')}")

# CV over alpha grid
alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)
cv_results <- data.frame(alpha = numeric(), lambda_min = numeric(),
                          cvm = numeric(), stringsAsFactors = FALSE)

best_cvm <- Inf
best_alpha <- NULL
best_cv_fit <- NULL

for (a in alpha_grid) {
  set.seed(42)
  cv_fit <- cv.glmnet(
    X_train, y_train,
    weights = w_train,
    alpha = a,
    foldid = foldid,
    type.measure = "mse",
    standardize = FALSE
  )

  min_cvm <- min(cv_fit$cvm)
  cv_results <- rbind(cv_results, data.frame(
    alpha = a,
    lambda_min = cv_fit$lambda.min,
    cvm = min_cvm
  ))

  cli::cli_inform("  alpha={a}: CV MSE={round(min_cvm, 1)}, RMSE={round(sqrt(min_cvm), 2)}, lambda={round(cv_fit$lambda.min, 4)}")

  if (min_cvm < best_cvm) {
    best_cvm <- min_cvm
    best_alpha <- a
    best_cv_fit <- cv_fit
  }
}

cli::cli_alert_success("Best alpha: {best_alpha} (CV RMSE: {round(sqrt(best_cvm), 2)})")

# Final model at best alpha + lambda.min
final_model <- glmnet(
  X_train, y_train,
  weights = w_train,
  alpha = best_alpha,
  lambda = best_cv_fit$lambda.min,
  standardize = FALSE
)

# 6. Evaluate ----
cli::cli_h1("Evaluation")

# Predictions
pred_train <- as.numeric(predict(final_model, X_train))
pred_test <- as.numeric(predict(final_model, X_test))

# Baseline: predict mean of training margin
baseline_pred <- mean(y_train)

# Metrics
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))
r_squared <- function(actual, predicted) {
  1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
}

cat("\n--- Alpha Grid Results ---\n")
print(cv_results)

cat("\n--- Model Performance ---\n")
cat(sprintf("Train RMSE: %.2f  MAE: %.2f  R²: %.3f\n",
            rmse(y_train, pred_train), mae(y_train, pred_train), r_squared(y_train, pred_train)))
cat(sprintf("Test  RMSE: %.2f  MAE: %.2f  R²: %.3f\n",
            rmse(y_test, pred_test), mae(y_test, pred_test), r_squared(y_test, pred_test)))
cat(sprintf("Baseline (predict mean) Test RMSE: %.2f\n", rmse(y_test, rep(baseline_pred, length(y_test)))))

# Extract coefficients (on standardized scale)
coefs <- as.matrix(coef(final_model))
coef_df <- data.frame(
  stat_name = sub("_diff$", "", rownames(coefs)[-1]),
  beta = coefs[-1, 1],
  sd = as.numeric(train_sds),
  stringsAsFactors = FALSE,
  row.names = NULL
)
coef_df <- coef_df[order(-abs(coef_df$beta)), ]
intercept_val <- coefs[1, 1]

cat(sprintf("\nIntercept (home advantage): %.2f\n", intercept_val))
cat("\n--- Top 15 Coefficients by |beta| (standardized scale) ---\n")
print(head(coef_df[, c("stat_name", "beta", "sd")], 15), row.names = FALSE)

n_nonzero <- sum(coef_df$beta != 0)
cat(sprintf("\nNon-zero coefficients: %d / %d\n", n_nonzero, nrow(coef_df)))

# 7. Calculate PSR for all players ----
cli::cli_h1("Calculating PSR")

psr <- calculate_psr(skills, coef_df, center = TRUE)

# Latest round leaderboard
latest <- psr[, .SD[round == max(round)], by = season]
latest <- latest[season == max(season)]
latest <- latest[order(-psr)]

cat("\n--- Top 20 Players by PSR (latest round) ---\n")
print(head(latest[, .(player_name, pos_group, season, round, psr = round(psr, 2))], 20),
      row.names = FALSE)

cat("\n--- Bottom 20 Players by PSR (latest round) ---\n")
print(tail(latest[, .(player_name, pos_group, season, round, psr = round(psr, 2))], 20),
      row.names = FALSE)

# Summary stats
cat(sprintf("\nPSR summary (latest round): mean=%.2f, sd=%.2f, min=%.2f, max=%.2f\n",
            mean(latest$psr), sd(latest$psr), min(latest$psr), max(latest$psr)))

# 8. OPSR / DPSR models ----
cli::cli_h1("Fitting OPSR (offense) and DPSR (defense) models")

# Helper: fit glmnet with alpha CV, return best model + coef_df
fit_psr_model <- function(X_tr, y_tr, w_tr, fold_id, X_te, y_te, label) {
  best_cvm_inner <- Inf
  best_fit_inner <- NULL

  for (a in alpha_grid) {
    set.seed(42)
    cv_f <- cv.glmnet(X_tr, y_tr, weights = w_tr, alpha = a,
                       foldid = fold_id, type.measure = "mse", standardize = FALSE)
    if (min(cv_f$cvm) < best_cvm_inner) {
      best_cvm_inner <- min(cv_f$cvm)
      best_fit_inner <- cv_f
      best_a <- a
    }
  }

  mdl <- glmnet(X_tr, y_tr, weights = w_tr, alpha = best_a,
                 lambda = best_fit_inner$lambda.min, standardize = FALSE)

  p_tr <- as.numeric(predict(mdl, X_tr))
  p_te <- as.numeric(predict(mdl, X_te))

  cli::cli_inform("{label}: alpha={best_a}, CV RMSE={round(sqrt(best_cvm_inner), 2)}, Train RMSE={round(rmse(y_tr, p_tr), 2)}, Test RMSE={round(rmse(y_te, p_te), 2)}, Test R2={round(r_squared(y_te, p_te), 3)}")

  cs <- as.matrix(coef(mdl))
  cdf <- data.frame(
    stat_name = sub("_diff$", "", rownames(cs)[-1]),
    beta = cs[-1, 1],
    sd = as.numeric(train_sds),
    stringsAsFactors = FALSE, row.names = NULL
  )

  list(model = mdl, coef_df = cdf, intercept = cs[1, 1],
       best_alpha = best_a, test_rmse = rmse(y_te, p_te))
}

# Offensive model: predict home_score from skill diffs
y_off_train <- match_df$home_score[train_idx]
y_off_test <- match_df$home_score[test_idx]
off_result <- fit_psr_model(X_train, y_off_train, w_train, foldid, X_test, y_off_test, "OPSR")

# Defensive model: predict away_score from skill diffs
y_def_train <- match_df$away_score[train_idx]
y_def_test <- match_df$away_score[test_idx]
def_result <- fit_psr_model(X_train, y_def_train, w_train, foldid, X_test, y_def_test, "DPSR")

cat("\n--- OPSR Top 10 Coefficients ---\n")
off_sorted <- off_result$coef_df[order(-abs(off_result$coef_df$beta)), ]
print(head(off_sorted[, c("stat_name", "beta")], 10), row.names = FALSE)

cat("\n--- DPSR Top 10 Coefficients ---\n")
def_sorted <- def_result$coef_df[order(-abs(def_result$coef_df$beta)), ]
print(head(def_sorted[, c("stat_name", "beta")], 10), row.names = FALSE)

# 9. Calculate OPSR, DPSR, TPSR ----
cli::cli_h1("Calculating OPSR, DPSR, TPSR")

# OPSR: positive beta = more skill → more own scoring (good)
opsr <- calculate_psr(skills, off_result$coef_df, center = TRUE)
setnames(opsr, c("psr_raw", "psr"), c("opsr_raw", "opsr"))

# DPSR: positive beta = more skill → more opponent scoring (bad!)
# Negate so positive DPSR = good defender
dpsr_coef <- data.table::copy(def_result$coef_df)
dpsr_coef$beta <- -dpsr_coef$beta
dpsr <- calculate_psr(skills, dpsr_coef, center = TRUE)
setnames(dpsr, c("psr_raw", "psr"), c("dpsr_raw", "dpsr"))

# Merge all ratings
id_cols <- intersect(names(opsr), names(dpsr))
id_cols <- setdiff(id_cols, c("opsr_raw", "opsr", "dpsr_raw", "dpsr"))
all_psr <- merge(
  merge(psr, opsr, by = id_cols, all = TRUE),
  dpsr, by = id_cols, all = TRUE
)
all_psr[, tpsr := opsr + dpsr]

# Latest round comparison
latest_all <- all_psr[, .SD[round == max(round)], by = season]
latest_all <- latest_all[season == max(season)]

cat("\n--- Top 20 by TPSR (latest round) ---\n")
print(
  head(latest_all[order(-tpsr), .(player_name, pos_group, opsr = round(opsr, 2),
                                   dpsr = round(dpsr, 2), tpsr = round(tpsr, 2),
                                   psr = round(psr, 2))], 20),
  row.names = FALSE
)

cat("\n--- Bottom 20 by TPSR (latest round) ---\n")
print(
  tail(latest_all[order(-tpsr), .(player_name, pos_group, opsr = round(opsr, 2),
                                   dpsr = round(dpsr, 2), tpsr = round(tpsr, 2),
                                   psr = round(psr, 2))], 20),
  row.names = FALSE
)

# PSR vs TPSR correlation
cat(sprintf("\nPSR vs TPSR correlation: %.3f\n", cor(latest_all$psr, latest_all$tpsr, use = "complete.obs")))
cat(sprintf("OPSR vs DPSR correlation: %.3f\n", cor(latest_all$opsr, latest_all$dpsr, use = "complete.obs")))

# Biggest differences: who benefits most from the split?
latest_all[, psr_tpsr_diff := tpsr - psr]
cat("\n--- Biggest TPSR > PSR (undervalued by margin model) ---\n")
print(head(latest_all[order(-psr_tpsr_diff), .(player_name, pos_group,
  opsr = round(opsr, 2), dpsr = round(dpsr, 2),
  tpsr = round(tpsr, 2), psr = round(psr, 2), diff = round(psr_tpsr_diff, 2))], 10),
  row.names = FALSE)

cat("\n--- Biggest PSR > TPSR (overvalued by margin model) ---\n")
print(head(latest_all[order(psr_tpsr_diff), .(player_name, pos_group,
  opsr = round(opsr, 2), dpsr = round(dpsr, 2),
  tpsr = round(tpsr, 2), psr = round(psr, 2), diff = round(psr_tpsr_diff, 2))], 10),
  row.names = FALSE)

# 10. Save outputs ----
cli::cli_h1("Saving outputs")

# PSR coefficients CSV
coef_out <- rbind(
  data.frame(stat_name = "(Intercept)", beta = intercept_val, sd = NA),
  coef_df
)
write.csv(coef_out, file.path(cache_dir, "psr_coefficients.csv"), row.names = FALSE)
cli::cli_alert_success("Saved: psr_coefficients.csv")

# OPSR coefficients CSV
opsr_coef_out <- rbind(
  data.frame(stat_name = "(Intercept)", beta = off_result$intercept, sd = NA),
  off_result$coef_df
)
write.csv(opsr_coef_out, file.path(cache_dir, "opsr_coefficients.csv"), row.names = FALSE)
cli::cli_alert_success("Saved: opsr_coefficients.csv")

# DPSR coefficients CSV (stored with original sign; negate at attribution time)
dpsr_coef_out <- rbind(
  data.frame(stat_name = "(Intercept)", beta = def_result$intercept, sd = NA),
  def_result$coef_df
)
write.csv(dpsr_coef_out, file.path(cache_dir, "dpsr_coefficients.csv"), row.names = FALSE)
cli::cli_alert_success("Saved: dpsr_coefficients.csv")

# All models RDS
model_out <- list(
  # Margin model (PSR)
  model = final_model,
  best_alpha = best_alpha,
  best_lambda = best_cv_fit$lambda.min,
  cv_results = cv_results,
  coef_df = coef_df,
  intercept = intercept_val,
  # Offensive model (OPSR)
  off_model = off_result$model,
  off_coef_df = off_result$coef_df,
  off_intercept = off_result$intercept,
  off_best_alpha = off_result$best_alpha,
  # Defensive model (DPSR)
  def_model = def_result$model,
  def_coef_df = def_result$coef_df,
  def_intercept = def_result$intercept,
  def_best_alpha = def_result$best_alpha,
  # Shared
  train_sds = train_sds,
  diff_cols = diff_cols,
  skill_cols = skill_cols,
  train_seasons = unique_seasons,
  test_rmse = rmse(y_test, pred_test),
  train_rmse = rmse(y_train, pred_train)
)
saveRDS(model_out, file.path(cache_dir, "psr_model.rds"))
cli::cli_alert_success("Saved: psr_model.rds")

cli::cli_alert_success("Done!")
