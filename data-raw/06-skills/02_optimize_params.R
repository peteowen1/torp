# 02_optimize_params.R
# ====================
# Hyperparameter optimization for skill estimation.
# Optimizes prior strength and decay rate (lambda) for rate stat categories.
# Efficiency stat hyperparameters are left at defaults.
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

# Compute grand means per stat ----
stat_defs <- skill_stat_definitions()
rate_defs <- stat_defs[stat_defs$type == "rate", ]
eff_defs <- stat_defs[stat_defs$type == "efficiency", ]

# Rate stats: TOG-weighted grand mean (per-full-game rate)
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

# Efficiency stats: grand mean proportion (successes / attempts)
# Also precompute per-player successes and attempts arrays
.compute_denom <- function(dt_row, spec) {
  if (grepl("\\+", spec)) {
    parts <- strsplit(spec, "\\+")[[1]]
    result <- rep(0, nrow(dt_row))
    for (p in parts) {
      if (p %in% names(dt_row)) {
        v <- as.numeric(dt_row[[p]])
        v[is.na(v)] <- 0
        result <- result + v
      }
    }
    return(result)
  }
  if (spec %in% names(dt_row)) {
    v <- as.numeric(dt_row[[spec]])
    v[is.na(v)] <- 0
    return(v)
  }
  rep(0, nrow(dt_row))
}

grand_props <- list()
eff_player_data <- list()

for (i in seq_len(nrow(eff_defs))) {
  stat_nm <- eff_defs$stat_name[i]
  success_spec <- eff_defs$success_col[i]
  attempts_spec <- eff_defs$attempts_col[i]
  if (is.na(success_spec) || is.na(attempts_spec)) next

  # Compute successes and attempts for full dataset
  if (success_spec %in% names(dt)) {
    all_succ <- as.numeric(dt[[success_spec]])
  } else {
    all_succ <- .compute_denom(dt, success_spec)
  }
  all_succ[is.na(all_succ)] <- 0
  all_att <- .compute_denom(dt, attempts_spec)
  all_att[is.na(all_att)] <- 0
  all_succ <- pmin(all_succ, all_att)

  total_att <- sum(all_att)
  grand_props[[stat_nm]] <- if (total_att > 0) sum(all_succ) / total_att else 0.5

  # Build per-player arrays for efficiency optimization
  eff_player_data[[stat_nm]] <- lapply(player_splits, function(ps) {
    pid_rows <- which(dt$player_id == ps$player_id[1])
    list(
      successes = all_succ[pid_rows],
      attempts = all_att[pid_rows]
    )
  })
  names(eff_player_data[[stat_nm]]) <- names(player_splits)
}


# Optimization objectives ----
# Rate stats: TOG-weighted MSE (Gamma-Poisson)
# Efficiency stats: attempt-weighted log-loss (Beta-Binomial)
#
# Both use cumsum trick: w_i = exp(-lam*(d_j - d_i))
#   = exp(-lam*d_j) * exp(lam*d_i), so cumulative sums are O(n)

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

    events <- vals
    exposure <- tog

    exp_pos <- exp(pmin(lambda * d_rel, 500))
    exp_neg <- exp(pmax(-lambda * d_rel, -500))

    cum_events <- cumsum(exp_pos * events)
    cum_exposure <- cumsum(exp_pos * exposure)

    j_idx <- (min_player_games + 1):ps$.n

    w_events <- exp_neg[j_idx] * cum_events[j_idx - 1]
    w_exposure <- exp_neg[j_idx] * cum_exposure[j_idx - 1]

    predicted_rate <- (alpha0 + w_events) / (beta0 + w_exposure)
    actual_rate <- vals[j_idx] / pmax(tog[j_idx], 0.1)

    w <- tog[j_idx]
    ok <- w > 0

    total_loss <- total_loss + sum(w[ok] * (predicted_rate[ok] - actual_rate[ok])^2)
    total_wt <- total_wt + sum(w[ok])
  }

  if (total_wt == 0) return(1e6)
  result <- total_loss / total_wt
  if (is.nan(result) || is.infinite(result)) 1e6 else result
}

compute_efficiency_logloss <- function(par, stat_nm) {
  lambda <- par[1]
  prior_strength <- par[2]

  mu0 <- grand_props[[stat_nm]]
  if (is.null(mu0)) return(1e6)
  mu0 <- max(min(mu0, 1 - 1e-6), 1e-6)
  alpha0 <- mu0 * prior_strength
  beta0 <- (1 - mu0) * prior_strength

  eff_data <- eff_player_data[[stat_nm]]
  total_loss <- 0
  total_wt <- 0

  for (k in seq_along(player_splits)) {
    ps <- player_splits[[k]]
    if (ps$.n <= min_player_games) next

    ed <- eff_data[[k]]
    succ <- ed$successes
    att <- ed$attempts
    d_rel <- ps$.d_rel

    exp_pos <- exp(pmin(lambda * d_rel, 500))
    exp_neg <- exp(pmax(-lambda * d_rel, -500))

    cum_succ <- cumsum(exp_pos * succ)
    cum_att <- cumsum(exp_pos * att)

    j_idx <- (min_player_games + 1):ps$.n

    w_succ <- exp_neg[j_idx] * cum_succ[j_idx - 1]
    w_att <- exp_neg[j_idx] * cum_att[j_idx - 1]

    # Beta-Binomial posterior mean
    predicted <- (alpha0 + w_succ) / (alpha0 + beta0 + w_att)
    predicted <- pmax(pmin(predicted, 1 - 1e-8), 1e-8)

    # Actual efficiency in next match (successes / attempts)
    actual <- ifelse(att[j_idx] > 0, succ[j_idx] / att[j_idx], mu0)
    actual <- pmax(pmin(actual, 1 - 1e-8), 1e-8)

    # Binary cross-entropy, weighted by attempts
    loss <- -(actual * log(predicted) + (1 - actual) * log(1 - predicted))
    match_w <- att[j_idx]
    ok <- match_w > 0

    total_loss <- total_loss + sum(match_w[ok] * loss[ok])
    total_wt <- total_wt + sum(match_w[ok])
  }

  if (total_wt == 0) return(1e6)
  result <- total_loss / total_wt
  if (is.nan(result) || is.infinite(result)) 1e6 else result
}


# Multi-start optimizer helper ----
# Tries multiple initial values and keeps the best result.
# L-BFGS-B with a single start can get stuck near the initial value.
multi_start_optim <- function(fn, starts, lower, upper, ...) {
  best <- NULL
  for (s in starts) {
    opt <- tryCatch(
      stats::optim(
        par = s, fn = fn, method = "L-BFGS-B",
        lower = lower, upper = upper, ...
      ),
      error = function(e) NULL
    )
    if (!is.null(opt) && (is.null(best) || opt$value < best$value)) {
      best <- opt
    }
  }
  best
}

# Optimize per stat ----
cli::cli_h1("Optimizing rate stats (MSE)")

stat_results <- list()

# Multi-start grid: diverse lambda x prior_strength combinations
rate_starts <- list(
  c(0.001, 1),   c(0.001, 5),   c(0.001, 20),
  c(0.003, 1),   c(0.003, 5),   c(0.003, 20),
  c(0.005, 0.5), c(0.005, 3),   c(0.005, 10),
  c(0.01,  1),   c(0.01,  10),  c(0.01,  30)
)

for (i in seq_len(nrow(rate_defs))) {
  stat_nm <- rate_defs$stat_name[i]
  src_col <- rate_defs$source_col[i]
  stat_cat <- rate_defs$category[i]

  if (!src_col %in% names(dt)) {
    cli::cli_warn("Skipping {stat_nm}: column {src_col} not found")
    next
  }

  cli::cli_inform("Optimizing {stat_nm} ({stat_cat})")

  opt <- multi_start_optim(
    fn = compute_rate_mse,
    starts = rate_starts,
    lower = c(0.0001, 0.1),
    upper = c(0.02, 100),
    stat_col = src_col
  )

  if (!is.null(opt)) {
    lambda <- opt$par[1]
    prior <- opt$par[2]
    half_life <- log(2) / lambda

    stat_results[[stat_nm]] <- list(
      stat_name = stat_nm,
      type = "rate",
      category = stat_cat,
      lambda = lambda,
      prior_strength = prior,
      half_life_days = half_life,
      loss = opt$value,
      loss_type = "mse"
    )

    cli::cli_inform("  lambda={round(lambda, 5)} (half-life={round(half_life)}d), prior={round(prior, 2)}, MSE={round(opt$value, 4)}")
  }
}

# Optimize efficiency stats (log-loss) ----
cli::cli_h1("Optimizing efficiency stats (log-loss)")

eff_starts <- list(
  c(0.001, 5),   c(0.001, 30),  c(0.001, 100),
  c(0.003, 5),   c(0.003, 30),  c(0.003, 100),
  c(0.005, 10),  c(0.005, 50),  c(0.005, 200),
  c(0.01,  20),  c(0.01,  100), c(0.01,  300)
)

for (i in seq_len(nrow(eff_defs))) {
  stat_nm <- eff_defs$stat_name[i]
  stat_cat <- eff_defs$category[i]

  if (!stat_nm %in% names(grand_props)) {
    cli::cli_warn("Skipping {stat_nm}: could not compute grand proportion")
    next
  }

  cli::cli_inform("Optimizing {stat_nm} ({stat_cat})")

  opt <- multi_start_optim(
    fn = compute_efficiency_logloss,
    starts = eff_starts,
    lower = c(0.0001, 1),
    upper = c(0.02, 500),
    stat_nm = stat_nm
  )

  if (!is.null(opt)) {
    lambda <- opt$par[1]
    prior <- opt$par[2]
    half_life <- log(2) / lambda

    stat_results[[stat_nm]] <- list(
      stat_name = stat_nm,
      type = "efficiency",
      category = stat_cat,
      lambda = lambda,
      prior_strength = prior,
      half_life_days = half_life,
      loss = opt$value,
      loss_type = "logloss"
    )

    cli::cli_inform("  lambda={round(lambda, 5)} (half-life={round(half_life)}d), prior={round(prior, 2)}, logloss={round(opt$value, 4)}")
  }
}

# Build optimized params ----
cli::cli_h1("Building optimized parameter set")

opt_params <- default_skill_params()

if (length(stat_results) > 0) {
  lambdas <- vapply(stat_results, function(x) x$lambda, numeric(1))
  priors <- vapply(stat_results, function(x) x$prior_strength, numeric(1))

  opt_params$lambda_rate <- median(lambdas)
  opt_params$prior_games <- median(priors)

  # Per-stat overrides
  opt_params$stat_params <- stat_results

  cli::cli_inform("Median lambda_rate: {round(opt_params$lambda_rate, 5)} (half-life: {round(log(2)/opt_params$lambda_rate)}d)")
  cli::cli_inform("Median prior_games: {round(opt_params$prior_games, 2)}")
}

# Save ----
saveRDS(opt_params, file.path(cache_dir, "02_optimized_params.rds"))
cli::cli_alert_success("Saved optimized params to {file.path(cache_dir, '02_optimized_params.rds')}")

# Summary table ----
if (length(stat_results) > 0) {
  summary_df <- do.call(rbind, lapply(stat_results, function(x) {
    data.frame(
      stat = x$stat_name,
      type = x$type,
      category = x$category,
      lambda = round(x$lambda, 5),
      half_life_days = round(x$half_life_days),
      prior_strength = round(x$prior_strength, 2),
      loss = round(x$loss, 4),
      loss_type = x$loss_type,
      stringsAsFactors = FALSE
    )
  }))
  data.table::setDT(summary_df)
  data.table::setorder(summary_df, half_life_days)
  cli::cli_h2("Optimization Summary")
  print(summary_df, row.names = FALSE)
}
