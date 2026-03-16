# 06_train_psr_v2.R
# =================
# PSR v2: Separate home/away skill features (not diffs) for cleaner
# offense/defense decomposition.
#
# OSR model: home_score ~ home_skills + away_skills
# DSR model: away_score ~ home_skills + away_skills
# Margin model: margin ~ home_skills + away_skills
#
# Player attribution:
#   OSR = Σ β_own_off × skill/sd  (your skills → your team scores more)
#   DSR = -Σ β_opp_def × skill/sd (your skills → opponent scores less)
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
teams[, round := as.integer(round_number)]
teams[, player_id := as.character(player_id)]
teams[, season := as.integer(season)]
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
off_fit <- fit_model(y_off_train, y_off_test, "Offense (v2 OSR)")
def_fit <- fit_model(y_def_train, y_def_test, "Defense (v2 DSR)")

# PSR match prediction
pred_psr_v2 <- off_fit$pred_test - def_fit$pred_test

cat(sprintf("\nv2 PSR (off - def): Test RMSE=%.2f, MAE=%.2f, R2=%.3f\n",
  rmse(y_margin_test, pred_psr_v2), mae(y_margin_test, pred_psr_v2),
  r2(y_margin_test, pred_psr_v2)))

# 4. Extract offense/defense coefficients ----
cli::cli_h1("Extracting player-level coefficients")

# Offensive model coefficients
off_cs <- off_fit$coefs
# home_* coefficients = "own skills → own score" = OSR
# away_* coefficients = "opponent skills → own score" = opponent's defensive impact
off_home_beta <- off_cs[paste0("home_", skill_cols), 1]
off_away_beta <- off_cs[paste0("away_", skill_cols), 1]

# Defensive model coefficients
def_cs <- def_fit$coefs
# home_* coefficients = "own skills → opponent score" = own defensive impact
# away_* coefficients = "opponent skills → opponent score" = opponent's OSR
def_home_beta <- def_cs[paste0("home_", skill_cols), 1]
def_away_beta <- def_cs[paste0("away_", skill_cols), 1]

# For a player on the HOME team:
#   OSR = off_home_beta × skill (my skills → my team scores more)
#   DSR = -def_home_beta × skill (my skills → opponent scores less; negate so positive = good)
#
# For a player on the AWAY team:
#   OSR = def_away_beta × skill (my skills → my team scores more)
#   DSR = -off_away_beta × skill (my skills → opponent scores less; negate so positive = good)
#
# For symmetric attribution, average the home and away perspectives:
#   OSR_beta = (off_home_beta + def_away_beta) / 2
#   DSR_beta = -(def_home_beta + off_away_beta) / 2

osr_beta <- (off_home_beta + def_away_beta) / 2
dsr_beta <- -(def_home_beta + off_away_beta) / 2

# SDs for the home_ columns (player skills get divided by these)
home_sds <- train_sds[paste0("home_", skill_cols)]

# Build coef_dfs for calculate_psr()
osr_coef_df <- data.frame(
  stat_name = sub("_skill$", "", skill_cols),
  beta = as.numeric(osr_beta),
  sd = as.numeric(home_sds),
  stringsAsFactors = FALSE
)

dsr_coef_df <- data.frame(
  stat_name = sub("_skill$", "", skill_cols),
  beta = as.numeric(dsr_beta),
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

cat("\n--- v2 OSR Top 10 Coefficients ---\n")
print(head(osr_coef_df[order(-abs(osr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

cat("\n--- v2 DSR Top 10 Coefficients ---\n")
print(head(dsr_coef_df[order(-abs(dsr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

cat("\n--- v2 PSR Top 10 Coefficients ---\n")
print(head(psr_coef_df[order(-abs(psr_coef_df$beta)), c("stat_name", "beta")], 10), row.names = FALSE)

# 5. Calculate player ratings ----
cli::cli_h1("Player ratings")

osr <- calculate_psr(skills, osr_coef_df, center = TRUE)
setnames(osr, c("psr_raw", "psr"), c("osr_raw", "osr"))

dsr <- calculate_psr(skills, dsr_coef_df, center = TRUE)
setnames(dsr, c("psr_raw", "psr"), c("dsr_raw", "dsr"))

psr_v2 <- calculate_psr(skills, psr_coef_df, center = TRUE)
setnames(psr_v2, c("psr_raw", "psr"), c("margin_psr_raw", "margin_psr"))

id_cols <- intersect(names(osr), names(dsr))
id_cols <- setdiff(id_cols, c("osr_raw", "osr", "dsr_raw", "dsr"))
all_ratings <- merge(merge(psr_v2, osr, by = id_cols, all = TRUE),
                     dsr, by = id_cols, all = TRUE)
all_ratings[, psr := osr + dsr]

latest <- all_ratings[, .SD[round == max(round)], by = season]
latest <- latest[season == max(season)]

cat("\n--- Top 20 by v2 PSR ---\n")
print(head(latest[order(-psr),
  .(player_name, pos_group, osr = round(osr, 2), dsr = round(dsr, 2),
    psr = round(psr, 2), margin_psr = round(margin_psr, 2))], 20), row.names = FALSE)

cat("\n--- Bottom 20 by v2 PSR ---\n")
print(tail(latest[order(-psr),
  .(player_name, pos_group, osr = round(osr, 2), dsr = round(dsr, 2),
    psr = round(psr, 2), margin_psr = round(margin_psr, 2))], 20), row.names = FALSE)

cat(sprintf("\nOSR vs DSR correlation: %.3f\n",
  cor(latest$osr, latest$dsr, use = "complete.obs")))

# Best offensive players
cat("\n--- Top 10 by v2 OSR ---\n")
print(head(latest[order(-osr),
  .(player_name, pos_group, osr = round(osr, 2), dsr = round(dsr, 2))], 10),
  row.names = FALSE)

# Best defensive players
cat("\n--- Top 10 by v2 DSR ---\n")
print(head(latest[order(-dsr),
  .(player_name, pos_group, osr = round(osr, 2), dsr = round(dsr, 2))], 10),
  row.names = FALSE)

# 6. Compare v1 vs v2 vs EPR ----
cli::cli_h1("v1 vs v2 vs EPR comparison")

# Load v1 model (skip comparison if not available)
v1_path <- file.path(cache_dir, "psr_model.rds")
has_v1 <- file.exists(v1_path)
if (!has_v1) cli::cli_warn("v1 model not found at {v1_path} — skipping v1 comparison")
v1_out <- if (has_v1) readRDS(v1_path) else NULL

# v1 diff features (rebuild quickly)
if (has_v1) {
  diff_cols <- paste0(all_skill_names[paste0(all_skill_names, "_skill") %in% skill_cols], "_diff")
  X_diff_raw <- as.matrix(match_df[, home_cols, with = FALSE]) -
                as.matrix(match_df[, away_cols, with = FALSE])
  colnames(X_diff_raw) <- diff_cols
  X_diff <- sweep(X_diff_raw, 2, v1_out$train_sds, "/")
  X_diff_test <- X_diff[test_idx, ]

  pred_v1_psr <- as.numeric(predict(v1_out$model, X_diff_test))
  pred_v1_off <- as.numeric(predict(v1_out$off_model, X_diff_test))
  pred_v1_def <- as.numeric(predict(v1_out$def_model, X_diff_test))
  pred_v1_psr_combined <- pred_v1_off - pred_v1_def
}

# EPR
epr_df <- as.data.table(load_torp_ratings())
epr_df[, player_id := as.character(player_id)]
epr_cols <- c("epr", "recv_epr", "disp_epr", "spoil_epr", "hitout_epr")
epr_join <- epr_df[, c("player_id", "season", "round", epr_cols), with = FALSE]
epr_join[, season := as.integer(season)]
epr_join[, round := as.integer(round)]

teams2 <- as.data.table(load_teams(TRUE))
teams2 <- teams2[is.na(position) | (position != "EMERG" & position != "SUB")]
teams2[, round := as.integer(round_number)]
teams2[, player_id := as.character(player_id)]
teams2[, season := as.integer(season)]

merged_epr <- merge(teams2, epr_join, by = c("player_id", "season", "round"), all.x = TRUE)
for (tc in epr_cols) {
  na_idx <- which(is.na(merged_epr[[tc]]))
  if (length(na_idx) > 0) merged_epr[na_idx, (tc) := median(merged_epr[[tc]], na.rm = TRUE)]
}

merged_epr[, .epr_total := fifelse(is.na(epr), 0, epr)]
team_epr <- merged_epr[order(-.epr_total)][
  , head(.SD, 22), by = .(match_id, team_id)
][, lapply(.SD, sum, na.rm = TRUE), by = .(match_id, team_id, season, round), .SDcols = epr_cols]

team_epr <- merge(team_epr,
  fixtures[, .(match_id, home_team_id, away_team_id)],
  by = "match_id", all.x = TRUE)
team_epr[, team_type := fifelse(team_id == home_team_id, "home", "away")]

home_t <- team_epr[team_type == "home"]
away_t <- team_epr[team_type == "away"]
setnames(home_t, epr_cols, paste0(epr_cols, "_home"))
setnames(away_t, epr_cols, paste0(epr_cols, "_away"))
match_epr <- merge(
  home_t[, c("match_id", "season", "round", paste0(epr_cols, "_home")), with = FALSE],
  away_t[, c("match_id", paste0(epr_cols, "_away")), with = FALSE],
  by = "match_id")
for (tc in epr_cols) {
  match_epr[, paste0(tc, "_diff") := get(paste0(tc, "_home")) - get(paste0(tc, "_away"))]
}
match_epr <- merge(match_epr, fixtures_margin[, .(match_id, home_margin, match_date)], by = "match_id")
epr_train <- match_epr[season < 2025]
epr_test <- match_epr[season >= 2025]
epr_train[, weightz := exp(as.numeric(-(anchor_date - match_date)) / 1000)]
epr_train[, weightz := weightz / mean(weightz, na.rm = TRUE)]
fit_epr <- lm(home_margin ~ epr_diff, data = epr_train, weights = weightz)
pred_epr <- predict(fit_epr, epr_test)

# Align
common_ids <- intersect(match_df$match_id[test_idx], epr_test$match_id)
psr_ci <- match(common_ids, match_df$match_id[test_idx])
epr_ci <- match(common_ids, epr_test$match_id)
y_common <- y_margin_test[psr_ci]
baseline <- mean(y_margin_train)

cat(sprintf("\nTest set: %d matches\n\n", length(common_ids)))
cat(sprintf("%-35s  %8s  %8s  %6s\n", "Model", "RMSE", "MAE", "R²"))
cat(paste(rep("-", 63), collapse = ""), "\n")

results <- list(
  "Baseline (predict mean)" = rep(baseline, length(common_ids)),
  "EPR: epr_diff OLS" = pred_epr[epr_ci],
  "v2 PSR (home+away)" = margin_fit$pred_test[psr_ci],
  "v2 PSR combined (home+away)" = pred_psr_v2[psr_ci],
  "Ensemble (EPR + v2 PSR)" = (pred_epr[epr_ci] + pred_psr_v2[psr_ci]) / 2
)
if (has_v1) {
  results[["v1 PSR (diffs)"]] <- pred_v1_psr[psr_ci]
  results[["v1 PSR combined (diffs)"]] <- pred_v1_psr_combined[psr_ci]
}

for (nm in names(results)) {
  p <- results[[nm]]
  cat(sprintf("%-35s  %8.2f  %8.2f  %6.3f\n",
    nm, rmse(y_common, p), mae(y_common, p), r2(y_common, p)))
}

# 7. Save v2 outputs ----
cli::cli_h1("Saving v2 outputs")

write.csv(osr_coef_df, file.path(cache_dir, "osr_v2_coefficients.csv"), row.names = FALSE)
write.csv(dsr_coef_df, file.path(cache_dir, "dsr_v2_coefficients.csv"), row.names = FALSE)
write.csv(psr_coef_df, file.path(cache_dir, "psr_v2_coefficients.csv"), row.names = FALSE)

v2_out <- list(
  margin_model = margin_fit$model, off_model = off_fit$model, def_model = def_fit$model,
  psr_coef_df = psr_coef_df, osr_coef_df = osr_coef_df, dsr_coef_df = dsr_coef_df,
  margin_intercept = margin_fit$coefs[1, 1],
  off_intercept = off_fit$coefs[1, 1], def_intercept = def_fit$coefs[1, 1],
  train_sds = train_sds, home_sds = home_sds,
  skill_cols = skill_cols,
  train_seasons = sort(unique(train_seasons)),
  test_rmse_margin = margin_fit$test_rmse,
  test_rmse_psr = rmse(y_margin_test, pred_psr_v2)
)
saveRDS(v2_out, file.path(cache_dir, "psr_v2_model.rds"))

cli::cli_alert_success("Saved v2 outputs")
cli::cli_alert_success("Done!")
