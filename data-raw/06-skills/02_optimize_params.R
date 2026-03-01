# 02_optimize_params.R
# ====================
# Hyperparameter optimization for skill estimation.
# Optimizes prior strength and decay rate (lambda) per stat category.
#
# Objective: minimize TOG-weighted MSE of next-match rate prediction.
# For rate stats, works in per-full-game rate space (analogous to panna's
# per-90 approach). Uses cumsum trick for O(n) per player.
#
# Input:  cache-skills/01_skill_data.rds
# Output: cache-skills/02_optimized_params.rds

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-skills")
min_player_games <- 5  # Min games for a player to be in optimization set
sample_n <- 500         # Max players to sample (for speed)
seed <- 42

# Load data ----
cli::cli_h1("Loading skill data")

skill_data <- readRDS(file.path(cache_dir, "01_skill_data.rds"))
data.table::setDT(skill_data)
cli::cli_inform("Loaded {nrow(skill_data)} rows, {length(unique(skill_data$player_id))} players")

# Sort by player + date
data.table::setorder(skill_data, player_id, match_date_skill)

# Precompute per-player data ----
cli::cli_h1("Precomputing per-player splits")

# Sample players for speed
player_game_counts <- skill_data[, .(n = data.table::uniqueN(match_id)), by = player_id]
eligible_players <- player_game_counts[n >= min_player_games]$player_id
set.seed(seed)
if (length(eligible_players) > sample_n) {
  sampled_players <- sample(eligible_players, sample_n)
} else {
  sampled_players <- eligible_players
}
dt <- skill_data[player_id %in% sampled_players]
cli::cli_inform("Using {length(sampled_players)} players (of {length(eligible_players)} eligible)")

# Build per-player lists with precomputed date/tog arrays
player_splits <- lapply(sampled_players, function(pid) {
  pdf <- dt[player_id == pid]
  dates_num <- as.numeric(pdf$match_date_skill)
  togs <- as.numeric(pdf$tog)
  togs[is.na(togs)] <- 1

  ps <- as.list(pdf)
  ps$.n <- nrow(pdf)
  ps$.d_rel <- dates_num - dates_num[1]  # Days relative to first game
  ps$.tog <- togs
  ps
})
names(player_splits) <- as.character(sampled_players)

# Compute grand means per stat (TOG-weighted) ----
stat_defs <- skill_stat_definitions()
rate_defs <- stat_defs[stat_defs$type == "rate", ]

grand_means <- list()
for (i in seq_len(nrow(rate_defs))) {
  src <- rate_defs$source_col[i]
  if (!src %in% names(dt)) next
  vals <- as.numeric(dt[[src]])
  vals[is.na(vals)] <- 0
  togs <- as.numeric(dt$tog)
  togs[is.na(togs)] <- 1
  total_exposure <- sum(togs)
  grand_means[[src]] <- if (total_exposure > 0) sum(vals) / total_exposure else 0
}


# Optimization objective ----
# Following panna's approach:
#   - events = raw_count (what was observed)
#   - exposure = tog (fraction of game played)
#   - Gamma-Poisson posterior mean = (alpha0 + w_events) / (beta0 + w_exposure)
#   - This gives per-full-game RATE (not raw count)
#   - Actual = raw_count / tog (per-full-game rate for that game)
#   - MSE computed on rates, weighted by tog (downweight noisy low-TOG games)
#   - Uses cumsum trick: w_i = exp(-lam*(d_j - d_i))
#     = exp(-lam*d_j) * exp(lam*d_i), so cumulative sums are O(n)

compute_rate_mse <- function(par, stat_col) {
  lambda <- par[1]
  prior_strength <- par[2]

  mu0 <- grand_means[[stat_col]]
  if (is.null(mu0)) return(1e6)
  alpha0 <- mu0 * prior_strength
  beta0 <- prior_strength

  total_loss <- 0
  total_wt <- 0

  for (ps in player_splits) {
    if (ps$.n <= min_player_games) next

    vals <- as.numeric(ps[[stat_col]])
    vals[is.na(vals)] <- 0
    tog <- ps$.tog
    d_rel <- ps$.d_rel

    # events = raw counts, exposure = tog
    events <- vals
    exposure <- tog

    # Vectorized decay via cumsum trick (cap exponent to avoid overflow)
    exp_pos <- exp(pmin(lambda * d_rel, 500))
    exp_neg <- exp(pmax(-lambda * d_rel, -500))

    cum_events <- cumsum(exp_pos * events)
    cum_exposure <- cumsum(exp_pos * exposure)

    # Predict games (min_player_games+1) through n
    j_idx <- (min_player_games + 1):ps$.n

    w_events <- exp_neg[j_idx] * cum_events[j_idx - 1]
    w_exposure <- exp_neg[j_idx] * cum_exposure[j_idx - 1]

    # Posterior rate = (alpha0 + w_events) / (beta0 + w_exposure)
    predicted_rate <- (alpha0 + w_events) / (beta0 + w_exposure)

    # Actual per-full-game rate
    actual_rate <- vals[j_idx] / pmax(tog[j_idx], 0.1)

    # TOG-weighted MSE (downweight low-TOG games)
    w <- tog[j_idx]
    ok <- w > 0

    total_loss <- total_loss + sum(w[ok] * (predicted_rate[ok] - actual_rate[ok])^2)
    total_wt <- total_wt + sum(w[ok])
  }

  if (total_wt == 0) return(1e6)
  result <- total_loss / total_wt
  if (is.nan(result) || is.infinite(result)) 1e6 else result
}


# Optimize per stat category ----
cli::cli_h1("Optimizing parameters")

categories <- unique(rate_defs$category)
results <- list()

for (cat in categories) {
  cat_stats <- rate_defs[rate_defs$category == cat, ]
  rep_stat <- cat_stats$source_col[1]

  if (!rep_stat %in% names(dt)) {
    cli::cli_warn("Skipping category {cat}: column {rep_stat} not found")
    next
  }

  cli::cli_inform("Optimizing {cat} (representative: {rep_stat})")

  opt <- tryCatch(
    stats::optim(
      par = c(0.002, 5),  # Initial: lambda=0.002, prior=5
      fn = compute_rate_mse,
      stat_col = rep_stat,
      method = "L-BFGS-B",
      lower = c(0.0001, 0.5),
      upper = c(0.02, 50)
    ),
    error = function(e) {
      cli::cli_warn("Optimization failed for {cat}: {conditionMessage(e)}")
      NULL
    }
  )

  if (!is.null(opt)) {
    lambda <- opt$par[1]
    prior <- opt$par[2]
    half_life <- log(2) / lambda

    results[[cat]] <- list(
      category = cat,
      lambda = lambda,
      prior_strength = prior,
      half_life_days = half_life,
      mse = opt$value
    )

    cli::cli_inform("  lambda={round(lambda, 5)} (half-life={round(half_life)}d), prior={round(prior, 2)}, MSE={round(opt$value, 4)}")
  }
}

# Build optimized params ----
cli::cli_h1("Building optimized parameter set")

opt_params <- default_skill_params()

# Use median lambda across categories as default
if (length(results) > 0) {
  lambdas <- vapply(results, function(x) x$lambda, numeric(1))
  priors <- vapply(results, function(x) x$prior_strength, numeric(1))

  opt_params$lambda_rate <- median(lambdas)
  opt_params$prior_games <- median(priors)

  # Per-category overrides
  opt_params$category_params <- results

  cli::cli_inform("Median lambda_rate: {round(opt_params$lambda_rate, 5)} (half-life: {round(log(2)/opt_params$lambda_rate)}d)")
  cli::cli_inform("Median prior_games: {round(opt_params$prior_games, 2)}")
}

# Save ----
saveRDS(opt_params, file.path(cache_dir, "02_optimized_params.rds"))
cli::cli_alert_success("Saved optimized params to {file.path(cache_dir, '02_optimized_params.rds')}")

# Summary table ----
if (length(results) > 0) {
  summary_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      category = x$category,
      lambda = round(x$lambda, 5),
      half_life_days = round(x$half_life_days),
      prior_strength = round(x$prior_strength, 2),
      mse = round(x$mse, 4),
      stringsAsFactors = FALSE
    )
  }))
  cli::cli_h2("Optimization Summary")
  print(summary_df, row.names = FALSE)
}
