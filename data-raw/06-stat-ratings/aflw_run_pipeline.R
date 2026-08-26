# aflw_run_pipeline.R
# ====================
# AFLW analog of 01_compute_match_stats.R + 03_estimate_stat_ratings.R +
# 06_train_psr_model.R combined into one script for the first validation
# pass (docs/plans/AFLW-MIGRATION-PLAN.md Phase 3, build-order step 2).
#
# Does NOT touch the men's cache-stat-ratings/ files or inst/extdata/ — writes
# to its own aflw-suffixed files so nothing men's-side is at risk of being
# overwritten by this run.

devtools::load_all()
library(glmnet)
library(data.table)

cache_dir <- file.path("data-raw", "cache-stat-ratings")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

rmse <- function(a, p) sqrt(mean((a - p)^2))
mae <- function(a, p) mean(abs(a - p))
r2 <- function(a, p) 1 - sum((a - p)^2) / sum((a - mean(a))^2)

# 1. Build stat_rating_data (AFLW substitute for 01_compute_match_stats.R) ----
cli::cli_h1("Step 1: AFLW stat rating data")
stat_rating_data <- torp:::.prepare_aflw_stat_rating_data(TRUE)
cli::cli_inform("stat_rating_data: {nrow(stat_rating_data)} rows, {length(unique(stat_rating_data$player_id))} players")
saveRDS(stat_rating_data, file.path(cache_dir, "aflw_01_stat_rating_data.rds"))

# 2. Per-round Bayesian stat rating estimation (mirrors 03_estimate_stat_ratings.R) ----
cli::cli_h1("Step 2: per-round stat ratings")

fixtures <- as.data.table(load_fixtures(TRUE, comp = "AFLW"))
seasons <- sort(unique(stat_rating_data$season))
# PLAYED rounds only -- load_fixtures() returns the full scheduled list, and
# estimating at a future ref_date produces drifting phantom rounds that reorder
# any leaderboard read off max(round). See torp:::.played_round_ref_dates().
ref_date_map <- torp:::.played_round_ref_dates(fixtures, seasons = seasons)

# Cross-feed check -- see the men's 03_estimate_stat_ratings.R for the full
# reasoning. The map comes from fixture scores; the rated rows come from the
# player-stats feed, and a lag between them silently drops the newest round.
torp:::.assert_ref_date_coverage(ref_date_map, stat_rating_data, label = "AFLW")

cli::cli_inform("Processing {nrow(ref_date_map)} season-round combinations")

params <- default_stat_rating_params()
t0 <- proc.time()
batch_results <- .estimate_stat_ratings_batch(
  stat_rating_data,
  ref_dates = ref_date_map$ref_date,
  params = params,
  compute_ci = FALSE
)
cli::cli_inform("Batch estimation completed in {round((proc.time()-t0)[['elapsed']], 1)}s")

all_results <- vector("list", nrow(ref_date_map))
counter <- 0
for (i in seq_len(nrow(ref_date_map))) {
  rd_key <- as.character(ref_date_map$ref_date[i])
  if (rd_key %in% names(batch_results) && nrow(batch_results[[rd_key]]) > 0) {
    res <- batch_results[[rd_key]]
    res[, `:=`(season = ref_date_map$season[i], round = ref_date_map$round[i])]
    counter <- counter + 1
    all_results[[counter]] <- res
  }
}
n_failures <- nrow(ref_date_map) - counter
cli::cli_inform("{counter}/{nrow(ref_date_map)} season-rounds estimated ({n_failures} skipped)")
if (counter == 0) cli::cli_abort("All round estimations failed.")

# Per-season breakdown, mirroring the men's script. Without this, a season
# dropped entirely by the checkpoint filter is invisible here: every count
# above is post-filter, and the abort only fires on TOTAL emptiness.
aflw_empty_seasons <- character(0)
for (szn in seasons) {
  n_rnds <- ref_date_map[season == szn, .N]
  n_ok <- sum(vapply(all_results[seq_len(counter)], function(r) {
    any(r$season == szn)
  }, logical(1)))
  if (n_rnds == 0) {
    aflw_empty_seasons <- c(aflw_empty_seasons, as.character(szn))
    next
  }
  cli::cli_inform("Completed {szn} ({n_ok}/{n_rnds} rounds)")
}
if (length(aflw_empty_seasons) > 0) {
  cli::cli_abort(c(
    "{length(aflw_empty_seasons)} AFLW season{?s} with rating data produced ZERO checkpoints: {.val {aflw_empty_seasons}}.",
    "x" = "Aborting rather than publishing an artifact that silently omits them."
  ))
}

all_stat_ratings <- rbindlist(all_results[seq_len(counter)], fill = TRUE)
cli::cli_inform("Total: {nrow(all_stat_ratings)} player-round rows")
saveRDS(all_stat_ratings, file.path(cache_dir, "aflw_03_player_stat_ratings.rds"))

# Degenerate-fit sanity check: any rating column with ~zero variance is a
# stat that couldn't be meaningfully estimated on AFLW's smaller sample.
rating_cols_all <- grep("_rating$", names(all_stat_ratings), value = TRUE)
sds_check <- vapply(rating_cols_all, function(cn) sd(all_stat_ratings[[cn]], na.rm = TRUE), numeric(1))
degenerate <- names(sds_check)[is.na(sds_check) | sds_check < 1e-6]
cli::cli_inform("Rating columns: {length(rating_cols_all)}. Degenerate (near-zero sd): {length(degenerate)}")
if (length(degenerate) > 0) cli::cli_inform("Degenerate: {paste(degenerate, collapse = ', ')}")

# 3. Train margin/OSR/DSR glmnet models (mirrors 06_train_psr_model.R) ----
cli::cli_h1("Step 3: PSR/OSR/DSR training")

stat_ratings <- copy(all_stat_ratings)
teams <- as.data.table(load_teams(TRUE, comp = "AFLW"))

fixtures_margin <- fixtures[
  !is.na(home_score) & !is.na(away_score),
  .(match_id, season = as.integer(season), round = as.integer(round_number),
    home_score = as.numeric(home_score), away_score = as.numeric(away_score),
    home_margin = home_score - away_score,
    match_date = as.Date(substr(utc_start_time, 1, 10)))
]

teams <- teams[is.na(lineup_position) | (lineup_position != "EMERG" & lineup_position != "SUB")]
teams[, round := as.integer(round_number)]
teams[, player_id := as.character(player_id)]
teams[, season := as.integer(season)]
stat_ratings[, player_id := as.character(player_id)]
stat_ratings[, season := as.integer(season)]
stat_ratings[, round := as.integer(round)]

stat_defs <- stat_rating_definitions()
exclude_stats <- c("cond_tog", "squad_selection", "dream_team_points", "rating_points",
                    "centre_bounce_attendances", "ruck_contests", "kickins", "bounces")
all_rating_names <- setdiff(stat_defs$stat_name, exclude_stats)

adj_rating_cols <- intersect(paste0(all_rating_names, "_adj_rating"), names(stat_ratings))
raw_rating_cols <- intersect(paste0(all_rating_names, "_rating"), names(stat_ratings))
if (length(adj_rating_cols) >= length(raw_rating_cols) * 0.8) {
  for (i in seq_along(adj_rating_cols)) {
    raw_col <- sub("_adj_rating$", "_rating", adj_rating_cols[i])
    if (raw_col %in% names(stat_ratings)) stat_ratings[, (raw_col) := get(adj_rating_cols[i])]
  }
  cli::cli_inform("Using opponent-adjusted stat ratings ({length(adj_rating_cols)} stats)")
}
rating_cols <- intersect(paste0(all_rating_names, "_rating"), names(stat_ratings))
cli::cli_inform("{length(rating_cols)} stat rating columns for PSR training")

ratings_join <- stat_ratings[, c("player_id", "season", "round", "pos_group", rating_cols), with = FALSE]
merged <- merge(teams, ratings_join, by = c("player_id", "season", "round"), all.x = TRUE)

pos_means <- merged[!is.na(pos_group), lapply(.SD, mean, na.rm = TRUE), by = pos_group, .SDcols = rating_cols]
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

merged[, .total_rating := rowSums(.SD, na.rm = TRUE), .SDcols = rating_cols]
team_ratings <- merged[order(-.total_rating)][
  , head(.SD, 22), by = .(match_id, team_id)
][, {
  out <- list(n_players = .N)
  for (sc in rating_cols) out[[sc]] <- sum(get(sc), na.rm = TRUE)
  out
}, by = .(match_id, team_id, season, round)]

team_ratings <- merge(team_ratings, fixtures[, .(match_id, home_team_id, away_team_id)], by = "match_id", all.x = TRUE)
team_ratings[, team_type := fifelse(team_id == home_team_id, "home", "away")]

home <- team_ratings[team_type == "home"]
away <- team_ratings[team_type == "away"]
home_cols <- paste0("home_", rating_cols)
away_cols <- paste0("away_", rating_cols)
setnames(home, rating_cols, home_cols)
setnames(away, rating_cols, away_cols)

match_df <- merge(home[, c("match_id", "season", "round", home_cols), with = FALSE],
                   away[, c("match_id", away_cols), with = FALSE], by = "match_id")
match_df <- merge(match_df, fixtures_margin[, .(match_id, home_score, away_score, home_margin, match_date)], by = "match_id")
cli::cli_inform("Match rows: {nrow(match_df)}, Features: {length(home_cols) + length(away_cols)}")

anchor_date <- max(match_df$match_date, na.rm = TRUE)
match_df[, weightz := exp(as.numeric(-(anchor_date - match_date)) / MATCH_WEIGHT_DECAY_DAYS)]
match_df[, weightz := weightz / mean(weightz, na.rm = TRUE)]

train_idx <- which(match_df$season < 2025)
test_idx <- which(match_df$season >= 2025)
cli::cli_inform("Train: {length(train_idx)} matches (seasons {paste(sort(unique(match_df$season[train_idx])), collapse=',')}), Test: {length(test_idx)} matches (season 2025)")

all_feat_cols <- c(home_cols, away_cols)
X_raw <- as.matrix(match_df[, all_feat_cols, with = FALSE])
train_sds <- apply(X_raw[train_idx, , drop = FALSE], 2, sd)
train_sds[train_sds == 0 | is.na(train_sds)] <- 1
X <- sweep(X_raw, 2, train_sds, "/")
X[is.na(X)] <- 0

X_train <- X[train_idx, , drop = FALSE]
X_test <- X[test_idx, , drop = FALSE]
w_train <- match_df$weightz[train_idx]
y_margin_train <- match_df$home_margin[train_idx]
y_margin_test <- match_df$home_margin[test_idx]
y_off_train <- match_df$home_score[train_idx]
y_off_test <- match_df$home_score[test_idx]
y_def_train <- match_df$away_score[train_idx]
y_def_test <- match_df$away_score[test_idx]

train_seasons_vec <- match_df$season[train_idx]
foldid <- as.integer(factor(train_seasons_vec, levels = sort(unique(train_seasons_vec))))
n_folds <- length(unique(foldid))
cli::cli_inform("CV folds: {n_folds} (by season)")

scoring_stats <- c("goals", "behinds", "shots_at_goal", "score_involvements", "goal_assists", "goal_accuracy", "score_launches")
defensive_stats <- c("tackles", "spoils", "intercepts", "one_percenters", "intercept_marks", "tackles_inside50")
osr_exclude_cols <- which(colnames(X) %in% c(paste0("home_", defensive_stats, "_rating"), paste0("away_", defensive_stats, "_rating")))
dsr_exclude_cols <- which(colnames(X) %in% c(paste0("home_", scoring_stats, "_rating"), paste0("away_", scoring_stats, "_rating")))

X_train_osr <- X_train[, -osr_exclude_cols, drop = FALSE]
X_test_osr <- X_test[, -osr_exclude_cols, drop = FALSE]
X_train_dsr <- X_train[, -dsr_exclude_cols, drop = FALSE]
X_test_dsr <- X_test[, -dsr_exclude_cols, drop = FALSE]

# Alpha stays CV-SELECTED over the full grid. Recorded here because it was
# investigated properly on 2026-08-26 and the obvious "improvement" is a trap.
#
# Context: widening the training window from 2021-24 to 2018-24 appeared to buy
# -0.93 test RMSE. It did not. Each arm was CV-selecting its own alpha and they
# landed on different ones (old 1, new 0), so most of that gap was the penalty
# family, not the extra seasons. Holding alpha fixed and sweeping it -- which
# isolates the window as the only varying axis -- gives the real window effect:
#
#   alpha  RMSE old->new   MAE old->new    paired p
#   0.00   29.50 -> 29.38  23.41 -> 23.53  0.639   <- MAE goes the WRONG way
#   0.25   30.24 -> 29.96  24.07 -> 24.08  0.979
#   0.50   30.27 -> 29.98  24.15 -> 24.10  0.910
#   0.75   30.31 -> 29.96  24.22 -> 24.07  0.788
#   1.00   30.31 -> 29.31  24.23 -> 23.36  0.286
#
# So the window is worth about -0.4 RMSE on average, consistent in direction at
# every alpha but never close to significant, and it costs MAE at low alpha.
#
# TWO reasons this grid was left alone rather than pinned to the best-looking row:
#
# 1. Picking alpha off that table would be selecting on the TEST set. The sweep is
#    a valid attribution tool -- it says how much of the -0.93 was the window --
#    but it is not a valid selection rule. CV picks on training error, which is
#    the honest thing to do even when it picks the arm that scores worse on 2025.
#
# 2. alpha=1 was tried and FAILS the anchor checks despite the best test metrics.
#    Lasso at this lambda hollows the model out: PSR nonzero betas 33 -> 9 of 48,
#    93% of |beta*sd| on three stats, and OSR `goals` driven to EXACTLY 0 -- an
#    offensive rating that assigns no weight to goals. The top drivers change
#    identity (metres_gained/disposals/kicks -> score_involvements/inside50s/
#    clearances) and Spearman against published ratings drops 0.992 -> 0.829.
#    A -1.00 RMSE bought by deleting three quarters of the model is not a gain.
#
# Set TORP_AFLW_ALPHA_GRID (comma-separated) to re-sweep when re-examining; the
# default is the full grid, matching what produced the published coefficients.
alpha_grid <- as.numeric(strsplit(Sys.getenv("TORP_AFLW_ALPHA_GRID", "0,0.25,0.5,0.75,1"), ",")[[1]])
stopifnot(length(alpha_grid) >= 1, !anyNA(alpha_grid),
          all(alpha_grid >= 0), all(alpha_grid <= 1))
cli::cli_inform("Penalty alpha grid: {paste(alpha_grid, collapse = ', ')}{if (length(alpha_grid) == 1) ' (pinned)' else ' (CV-selected)'}")

fit_model <- function(X_tr, X_te, y_tr, y_te, label) {
  best_cvm <- Inf; best_fit <- NULL; best_a <- NULL
  nf <- length(unique(foldid))
  for (a in alpha_grid) {
    set.seed(42)
    cv_f <- tryCatch(
      cv.glmnet(X_tr, y_tr, weights = w_train, alpha = a, foldid = foldid,
                type.measure = "mse", standardize = FALSE),
      error = function(e) NULL
    )
    if (is.null(cv_f)) next
    if (min(cv_f$cvm) < best_cvm) { best_cvm <- min(cv_f$cvm); best_fit <- cv_f; best_a <- a }
  }
  if (is.null(best_fit)) cli::cli_abort("{label}: all alpha fits failed")
  mdl <- glmnet(X_tr, y_tr, weights = w_train, alpha = best_a, lambda = best_fit$lambda.min, standardize = FALSE)
  p_tr <- as.numeric(predict(mdl, X_tr))
  p_te <- as.numeric(predict(mdl, X_te))
  cli::cli_inform("{label}: alpha={best_a}, folds={nf}, CV RMSE={round(sqrt(best_cvm), 2)}, Train RMSE={round(rmse(y_tr, p_tr), 2)}, Test RMSE={round(rmse(y_te, p_te), 2)}, Test R2={round(r2(y_te, p_te), 3)}")
  list(model = mdl, coefs = as.matrix(coef(mdl)), best_alpha = best_a, pred_test = p_te,
       test_rmse = rmse(y_te, p_te), feature_cols = colnames(X_tr))
}

margin_fit <- fit_model(X_train, X_test, y_margin_train, y_margin_test, "Margin (PSR)")
off_fit <- fit_model(X_train_osr, X_test_osr, y_off_train, y_off_test, "Offense (OSR)")
def_fit <- fit_model(X_train_dsr, X_test_dsr, y_def_train, y_def_test, "Defense (DSR)")

pred_psr_combined <- off_fit$pred_test - def_fit$pred_test
cat(sprintf("\nPSR (off - def): Test RMSE=%.2f, MAE=%.2f, R2=%.3f\n",
  rmse(y_margin_test, pred_psr_combined), mae(y_margin_test, pred_psr_combined), r2(y_margin_test, pred_psr_combined)))

# 4. Baselines: naive home-margin-mean, and Elo-alone ----
cli::cli_h1("Step 4: baselines")

naive_pred <- rep(mean(y_margin_train), length(y_margin_test))
cat(sprintf("Naive (train mean margin=%.2f): Test RMSE=%.2f, MAE=%.2f, R2=%.3f\n",
  mean(y_margin_train), rmse(y_margin_test, naive_pred), mae(y_margin_test, naive_pred), r2(y_margin_test, naive_pred)))

elo_result <- torp:::build_aflw_team_elo(TRUE)
elo_by_match <- elo_result$by_match
test_match_ids <- match_df$match_id[test_idx]
test_home_team <- home[match(test_match_ids, home$match_id), ] # not directly team name; use fixtures
fx_test <- fixtures[match(test_match_ids, match_id), .(match_id, home_team_name, away_team_name)]
home_elo <- elo_by_match[match(paste(fx_test$match_id, fx_test$home_team_name), paste(elo_by_match$match_id, elo_by_match$team_name)), elo_pre]
away_elo <- elo_by_match[match(paste(fx_test$match_id, fx_test$away_team_name), paste(elo_by_match$match_id, elo_by_match$team_name)), elo_pre]
elo_diff <- (home_elo + AFLW_ELO_HGA) - away_elo
# Simple linear calibration of elo_diff -> margin, fit on TRAIN elo_diff/margin pairs
fx_train <- fixtures[match(match_df$match_id[train_idx], match_id), .(match_id, home_team_name, away_team_name)]
home_elo_tr <- elo_by_match[match(paste(fx_train$match_id, fx_train$home_team_name), paste(elo_by_match$match_id, elo_by_match$team_name)), elo_pre]
away_elo_tr <- elo_by_match[match(paste(fx_train$match_id, fx_train$away_team_name), paste(elo_by_match$match_id, elo_by_match$team_name)), elo_pre]
elo_diff_tr <- (home_elo_tr + AFLW_ELO_HGA) - away_elo_tr
elo_cal <- lm(y_margin_train ~ elo_diff_tr)
elo_pred_test <- predict(elo_cal, newdata = data.frame(elo_diff_tr = elo_diff))
ok <- !is.na(elo_pred_test) & !is.na(y_margin_test)
cat(sprintf("Elo-alone (n=%d): Test RMSE=%.2f, MAE=%.2f, R2=%.3f\n",
  sum(ok), rmse(y_margin_test[ok], elo_pred_test[ok]), mae(y_margin_test[ok], elo_pred_test[ok]), r2(y_margin_test[ok], elo_pred_test[ok])))

# 5. Write AFLW coefficient CSVs (comp-suffixed, NOT touching men's files) ----
cli::cli_h1("Step 5: writing AFLW coefficients")

extract_betas <- function(coefs, prefix, rc) {
  full_names <- paste0(prefix, rc)
  betas <- numeric(length(full_names)); names(betas) <- full_names
  cn <- rownames(coefs)
  for (i in seq_along(full_names)) if (full_names[i] %in% cn) betas[i] <- coefs[full_names[i], 1]
  betas
}
off_home_beta <- extract_betas(off_fit$coefs, "home_", rating_cols)
off_away_beta <- extract_betas(off_fit$coefs, "away_", rating_cols)
def_home_beta <- extract_betas(def_fit$coefs, "home_", rating_cols)
def_away_beta <- extract_betas(def_fit$coefs, "away_", rating_cols)
osr_beta <- (off_home_beta + def_away_beta) / 2
dsr_beta <- -(def_home_beta + off_away_beta) / 2
# intersect(), not a bare name assign: defensive_stats/scoring_stats include
# stats AFLW's narrower box score doesn't have (e.g. intercept_marks), and
# `x[name] <- 0` for a name NOT already in x APPENDS a new element rather
# than no-op-ing -- this is what actually grew osr_beta/dsr_beta from 42 to
# 43 elements on the first run (not a rating_cols duplicate, which a
# separate check confirmed doesn't exist).
osr_beta[intersect(paste0("home_", defensive_stats, "_rating"), names(osr_beta))] <- 0
dsr_beta[intersect(paste0("home_", scoring_stats, "_rating"), names(dsr_beta))] <- 0
# match(), not `[` by name: guarantees exactly length(rating_cols) values
# regardless of name uniqueness in train_sds/margin_fit$coefs.
home_sds <- train_sds[match(paste0("home_", rating_cols), names(train_sds))]

osr_coef_df <- data.frame(stat_name = sub("_rating$", "", rating_cols), beta = as.numeric(osr_beta), sd = as.numeric(home_sds))
dsr_coef_df <- data.frame(stat_name = sub("_rating$", "", rating_cols), beta = as.numeric(dsr_beta), sd = as.numeric(home_sds))
margin_home_beta <- margin_fit$coefs[match(paste0("home_", rating_cols), rownames(margin_fit$coefs)), 1]
margin_away_beta <- margin_fit$coefs[match(paste0("away_", rating_cols), rownames(margin_fit$coefs)), 1]
psr_beta <- (margin_home_beta - margin_away_beta) / 2
psr_coef_df <- data.frame(stat_name = sub("_rating$", "", rating_cols), beta = as.numeric(psr_beta), sd = as.numeric(home_sds))

write.csv(osr_coef_df, file.path(cache_dir, "osr_coefficients_aflw.csv"), row.names = FALSE)
write.csv(dsr_coef_df, file.path(cache_dir, "dsr_coefficients_aflw.csv"), row.names = FALSE)
write.csv(psr_coef_df, file.path(cache_dir, "psr_coefficients_aflw.csv"), row.names = FALSE)
write.csv(osr_coef_df, "inst/extdata/osr_coefficients_aflw.csv", row.names = FALSE)
write.csv(dsr_coef_df, "inst/extdata/dsr_coefficients_aflw.csv", row.names = FALSE)
write.csv(psr_coef_df, "inst/extdata/psr_coefficients_aflw.csv", row.names = FALSE)

cat("\n--- PSR Top 10 Coefficients (AFLW) ---\n")
print(head(psr_coef_df[order(-abs(psr_coef_df$beta)), ], 10), row.names = FALSE)

cli::cli_alert_success("Done. AFLW coefficients written to inst/extdata/*_aflw.csv")
