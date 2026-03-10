# 06_train_psr_v2.R
# =================
# PSR v2: Separate home/away skill features (not diffs) for cleaner
# offense/defense decomposition.
#
# OPSR model: home_score ~ home_skills + away_skills
# DPSR model: away_score ~ home_skills + away_skills
# Margin model: margin ~ home_skills + away_skills
#
# Player attribution:
#   OPSR = Σ β_own_off × skill/sd  (your skills → your team scores more)
#   DPSR = -Σ β_opp_def × skill/sd (your skills → opponent scores less)
#
# Train: 2021-2024, Test: 2025

# Setup ----
devtools::load_all()
library(glmnet)
library(data.table)

cache_dir <- file.path("data-raw", "cache-skills")

rmse <- function(a, p) sqrt(mean((a - p)^2))
mae <- function(a, p) mean(abs(a - p))
r2 <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

# 1. Load & prep data (same as v1) ----
cli::cli_h1("Loading data")

skills <- as.data.table(load_player_skills(TRUE))
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
teams[, player_id := as.character(player_id)]
teams[, season := as.integer(season)]
teams[, round := as.integer(round_number)]
skills[, player_id := as.character(player_id)]
skills[, season := as.integer(season)]
skills[, round := as.integer(round)]

stat_defs <- skill_stat_definitions()
exclude_stats <- c("cond_tog", "squad_selection", "dream_team_points", "rating_points")
all_skill_names <- setdiff(stat_defs$stat_name, exclude_stats)
skill_cols <- intersect(paste0(all_skill_names, "_skill"), names(skills))

cli::cli_inform("Using {length(skill_cols)} skill columns")

skills_join <- skills[, c("player_id", "season", "round", "pos_group", skill_cols), with = FALSE]
merged <- merge(teams, skills_join, by = c("player_id", "season", "round"), all.x = TRUE)

# Impute
pos_means <- merged[!is.na(pos_group), lapply(.SD, mean, na.rm = TRUE),
                    by = pos_group, .SDcols = skill_cols]
global_means <- merged[, lapply(.SD, mean, na.rm = TRUE), .SDcols = skill_cols]
for (sc in skill_cols) {
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
merged[, .total_skill := rowSums(.SD, na.rm = TRUE), .SDcols = skill_cols]
team_skills <- merged[order(-.total_skill)][
  , head(.SD, 22), by = .(match_id, team_id)
][, {
  out <- list(n_players = .N)
  for (sc in skill_cols) out[[sc]] <- sum(get(sc), na.rm = TRUE)
  out
}, by = .(match_id, team_id, season, round)]

team_skills <- merge(team_skills,
  fixtures[, .(match_id, home_team_id, away_team_id)],
  by = "match_id", all.x = TRUE)
team_skills[, team_type := fifelse(team_id == home_team_id, "home", "away")]

# 2. Build separate home/away feature matrix ----
cli::cli_h1("Building separate home/away feature matrix")

home <- team_skills[team_type == "home"]
away <- team_skills[team_type == "away"]

home_cols <- paste0("home_", skill_cols)
away_cols <- paste0("away_", skill_cols)

setnames(home, skill_cols, home_cols)
setnames(away, skill_cols, away_cols)

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

margin_fit <- fit_model(y_margin_train, y_margin_test, "Margin (v2 PSR)")
off_fit <- fit_model(y_off_train, y_off_test, "Offense (v2 OPSR)")
def_fit <- fit_model(y_def_train, y_def_test, "Defense (v2 DPSR)")

# TPSR match prediction
pred_tpsr_v2 <- off_fit$pred_test - def_fit$pred_test

cat(sprintf("\nv2 TPSR (off - def): Test RMSE=%.2f, MAE=%.2f, R2=%.3f\n",
  rmse(y_margin_test, pred_tpsr_v2), mae(y_margin_test, pred_tpsr_v2),
  r2(y_margin_test, pred_tpsr_v2)))

# 4. Extract offense/defense coefficients ----
cli::cli_h1("Extracting player-level coefficients")

# Offensive model coefficients
off_cs <- off_fit$coefs
# home_* coefficients = "own skills → own score" = OPSR
# away_* coefficients = "opponent skills → own score" = opponent's defensive impact
off_home_beta <- off_cs[paste0("home_", skill_cols), 1]
off_away_beta <- off_cs[paste0("away_", skill_cols), 1]

# Defensive model coefficients
def_cs <- def_fit$coefs
# home_* coefficients = "own skills → opponent score" = own defensive impact
# away_* coefficients = "opponent skills → opponent score" = opponent's OPSR
def_home_beta <- def_cs[paste0("home_", skill_cols), 1]
def_away_beta <- def_cs[paste0("away_", skill_cols), 1]

# For a player on the HOME team:
#   OPSR = off_home_beta × skill (my skills → my team scores more)
#   DPSR = -def_home_beta × skill (my skills → opponent scores less; negate so positive = good)
#
# For a player on the AWAY team:
#   OPSR = def_away_beta × skill (my skills → my team scores more)
#   DPSR = -off_away_beta × skill (my skills → opponent scores less; negate so positive = good)
#
# For symmetric attribution, average the home and away perspectives:
#   OPSR_beta = (off_home_beta + def_away_beta) / 2
#   DPSR_beta = -(def_home_beta + off_away_beta) / 2

opsr_beta <- (off_home_beta + def_away_beta) / 2
dpsr_beta <- -(def_home_beta + off_away_beta) / 2

# SDs for the home_ columns (player skills get divided by these)
home_sds <- train_sds[paste0("home_", skill_cols)]

# Build coef_dfs for calculate_psr()
opsr_coef_df <- data.frame(
  stat_name = sub("_skill$", "", skill_cols),
  beta = as.numeric(opsr_beta),
  sd = as.numeric(home_sds),
  stringsAsFactors = FALSE
)

dpsr_coef_df <- data.frame(
  stat_name = sub("_skill$", "", skill_cols),
  beta = as.numeric(dpsr_beta),
  sd = as.numeric(home_sds),
  stringsAsFactors = FALSE
)

# Margin model: average home and away perspectives too
margin_cs <- margin_fit$coefs
margin_home_beta <- margin_cs[paste0("home_", skill_cols), 1]
margin_away_beta <- margin_cs[paste0("away_", skill_cols), 1]
psr_beta <- (margin_home_beta - margin_away_beta) / 2

psr_coef_df <- data.frame(
  stat_name = sub("_skill$", "", skill_cols),
  beta = as.numeric(psr_beta),
  sd = as.numeric(home_sds),
  stringsAsFactors = FALSE
)

cat("\n--- v2 OPSR Top 10 Coefficients ---\n")
print(head(opsr_coef_df[order(-abs(opsr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

cat("\n--- v2 DPSR Top 10 Coefficients ---\n")
print(head(dpsr_coef_df[order(-abs(dpsr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

cat("\n--- v2 PSR Top 10 Coefficients ---\n")
print(head(psr_coef_df[order(-abs(psr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

# 5. Calculate player ratings ----
cli::cli_h1("Player ratings")

opsr <- calculate_psr(skills, opsr_coef_df, center = TRUE)
setnames(opsr, c("psr_raw", "psr"), c("opsr_raw", "opsr"))

dpsr <- calculate_psr(skills, dpsr_coef_df, center = TRUE)
setnames(dpsr, c("psr_raw", "psr"), c("dpsr_raw", "dpsr"))

psr_v2 <- calculate_psr(skills, psr_coef_df, center = TRUE)

id_cols <- intersect(names(opsr), names(dpsr))
id_cols <- setdiff(id_cols, c("opsr_raw", "opsr", "dpsr_raw", "dpsr"))
all_ratings <- merge(merge(psr_v2, opsr, by = id_cols, all = TRUE),
                     dpsr, by = id_cols, all = TRUE)
all_ratings[, tpsr := opsr + dpsr]

latest <- all_ratings[, .SD[round == max(round)], by = season]
latest <- latest[season == max(season)]

cat("\n--- Top 20 by v2 TPSR ---\n")
print(head(latest[order(-tpsr),
  .(player_name, pos_group, opsr = round(opsr, 2), dpsr = round(dpsr, 2),
    tpsr = round(tpsr, 2), psr = round(psr, 2))], 20), row.names = FALSE)

cat("\n--- Bottom 20 by v2 TPSR ---\n")
print(tail(latest[order(-tpsr),
  .(player_name, pos_group, opsr = round(opsr, 2), dpsr = round(dpsr, 2),
    tpsr = round(tpsr, 2), psr = round(psr, 2))], 20), row.names = FALSE)

cat(sprintf("\nOPSR vs DPSR correlation: %.3f\n",
  cor(latest$opsr, latest$dpsr, use = "complete.obs")))

# Best offensive players
cat("\n--- Top 10 by v2 OPSR ---\n")
print(head(latest[order(-opsr),
  .(player_name, pos_group, opsr = round(opsr, 2), dpsr = round(dpsr, 2))], 10),
  row.names = FALSE)

# Best defensive players
cat("\n--- Top 10 by v2 DPSR ---\n")
print(head(latest[order(-dpsr),
  .(player_name, pos_group, opsr = round(opsr, 2), dpsr = round(dpsr, 2))], 10),
  row.names = FALSE)

# 6. Compare v1 vs v2 vs TORP ----
cli::cli_h1("v1 vs v2 vs TORP comparison")

# Load v1 model
v1_out <- readRDS(file.path(cache_dir, "psr_model.rds"))

# v1 diff features (rebuild quickly)
diff_cols <- paste0(all_skill_names[paste0(all_skill_names, "_skill") %in% skill_cols], "_diff")
# Recompute diff matrix from the home/away columns in match_df
X_diff_raw <- as.matrix(match_df[, home_cols, with = FALSE]) -
              as.matrix(match_df[, away_cols, with = FALSE])
colnames(X_diff_raw) <- diff_cols
X_diff <- sweep(X_diff_raw, 2, v1_out$train_sds, "/")
X_diff_test <- X_diff[test_idx, ]

pred_v1_psr <- as.numeric(predict(v1_out$model, X_diff_test))
pred_v1_off <- as.numeric(predict(v1_out$off_model, X_diff_test))
pred_v1_def <- as.numeric(predict(v1_out$def_model, X_diff_test))
pred_v1_tpsr <- pred_v1_off - pred_v1_def

# TORP
torp_df <- as.data.table(load_torp_ratings())
torp_df[, player_id := as.character(player_id)]
torp_cols <- c("torp", "torp_recv", "torp_disp", "torp_spoil", "torp_hitout")
torp_join <- torp_df[, c("player_id", "season", "round", torp_cols), with = FALSE]
torp_join[, season := as.integer(season)]
torp_join[, round := as.integer(round)]

teams2 <- as.data.table(load_teams(TRUE))
teams2 <- teams2[is.na(position) | (position != "EMERG" & position != "SUB")]
teams2[, player_id := as.character(player_id)]
teams2[, season := as.integer(season)]
teams2[, round := as.integer(round_number)]

merged_torp <- merge(teams2, torp_join, by = c("player_id", "season", "round"), all.x = TRUE)
for (tc in torp_cols) {
  na_idx <- which(is.na(merged_torp[[tc]]))
  if (length(na_idx) > 0) merged_torp[na_idx, (tc) := median(merged_torp[[tc]], na.rm = TRUE)]
}

merged_torp[, .torp_total := fifelse(is.na(torp), 0, torp)]
team_torp <- merged_torp[order(-.torp_total)][
  , head(.SD, 22), by = .(match_id, team_id)
][, lapply(.SD, sum, na.rm = TRUE), by = .(match_id, team_id, season, round), .SDcols = torp_cols]

team_torp <- merge(team_torp,
  fixtures[, .(match_id, home_team_id, away_team_id)],
  by = "match_id", all.x = TRUE)
team_torp[, team_type := fifelse(team_id == home_team_id, "home", "away")]

home_t <- team_torp[team_type == "home"]
away_t <- team_torp[team_type == "away"]
setnames(home_t, torp_cols, paste0(torp_cols, "_home"))
setnames(away_t, torp_cols, paste0(torp_cols, "_away"))
match_torp <- merge(
  home_t[, c("match_id", "season", "round", paste0(torp_cols, "_home")), with = FALSE],
  away_t[, c("match_id", paste0(torp_cols, "_away")), with = FALSE],
  by = "match_id")
for (tc in torp_cols) {
  match_torp[, paste0(tc, "_diff") := get(paste0(tc, "_home")) - get(paste0(tc, "_away"))]
}
match_torp <- merge(match_torp, fixtures_margin[, .(match_id, home_margin, match_date)], by = "match_id")
torp_train <- match_torp[season < 2025]
torp_test <- match_torp[season >= 2025]
torp_train[, weightz := exp(as.numeric(-(anchor_date - match_date)) / 1000)]
torp_train[, weightz := weightz / mean(weightz, na.rm = TRUE)]
fit_torp <- lm(home_margin ~ torp_diff, data = torp_train, weights = weightz)
pred_torp <- predict(fit_torp, torp_test)

# Align
common_ids <- intersect(match_df$match_id[test_idx], torp_test$match_id)
psr_ci <- match(common_ids, match_df$match_id[test_idx])
torp_ci <- match(common_ids, torp_test$match_id)
y_common <- y_margin_test[psr_ci]
baseline <- mean(y_margin_train)

cat(sprintf("\nTest set: %d matches\n\n", length(common_ids)))
cat(sprintf("%-35s  %8s  %8s  %6s\n", "Model", "RMSE", "MAE", "R²"))
cat(paste(rep("-", 63), collapse = ""), "\n")

results <- list(
  "Baseline (predict mean)" = rep(baseline, length(common_ids)),
  "TORP: torp_diff OLS" = pred_torp[torp_ci],
  "v1 PSR (diffs)" = pred_v1_psr[psr_ci],
  "v1 TPSR (diffs)" = pred_v1_tpsr[psr_ci],
  "v2 PSR (home+away)" = margin_fit$pred_test[psr_ci],
  "v2 TPSR (home+away)" = pred_tpsr_v2[psr_ci],
  "Ensemble (TORP + v2 TPSR)" = (pred_torp[torp_ci] + pred_tpsr_v2[psr_ci]) / 2
)

for (nm in names(results)) {
  p <- results[[nm]]
  cat(sprintf("%-35s  %8.2f  %8.2f  %6.3f\n",
    nm, rmse(y_common, p), mae(y_common, p), r2(y_common, p)))
}

# 7. Save v2 outputs ----
cli::cli_h1("Saving v2 outputs")

write.csv(opsr_coef_df, file.path(cache_dir, "opsr_v2_coefficients.csv"), row.names = FALSE)
write.csv(dpsr_coef_df, file.path(cache_dir, "dpsr_v2_coefficients.csv"), row.names = FALSE)
write.csv(psr_coef_df, file.path(cache_dir, "psr_v2_coefficients.csv"), row.names = FALSE)

v2_out <- list(
  margin_model = margin_fit$model, off_model = off_fit$model, def_model = def_fit$model,
  psr_coef_df = psr_coef_df, opsr_coef_df = opsr_coef_df, dpsr_coef_df = dpsr_coef_df,
  margin_intercept = margin_fit$coefs[1, 1],
  off_intercept = off_fit$coefs[1, 1], def_intercept = def_fit$coefs[1, 1],
  train_sds = train_sds, home_sds = home_sds,
  skill_cols = skill_cols,
  train_seasons = sort(unique(train_seasons)),
  test_rmse_margin = margin_fit$test_rmse,
  test_rmse_tpsr = rmse(y_margin_test, pred_tpsr_v2)
)
saveRDS(v2_out, file.path(cache_dir, "psr_v2_model.rds"))

cli::cli_alert_success("Saved v2 outputs")
cli::cli_alert_success("Done!")
