# 02_optimize_stat_rating_params.R
# =================================
# Hyperparameter optimization for stat rating estimation.
# Optimizes prior strength and decay rate (lambda) for rate stat categories.
# Efficiency stat hyperparameters are left at defaults.
#
# Objective: minimize TOG-weighted MSE of next-match rate prediction.
# For rate stats, works in per-full-game rate space (analogous to panna's
# per-90 approach). Uses cumsum trick for O(n) per player.
#
# Input:  cache-stat-ratings/01_stat_rating_data.rds
# Output: cache-stat-ratings/02_optimized_params.rds

# Setup ----
devtools::load_all()

# Config ----
cache_dir <- file.path("data-raw", "cache-stat-ratings")
min_player_games <- 5  # Min games for a player to be in optimization set
sample_n <- 500         # Max players to sample (for speed)
seed <- 42

# Load data ----
cli::cli_h1("Loading stat rating data")

stat_rating_data <- readRDS(file.path(cache_dir, "01_stat_rating_data.rds"))
data.table::setDT(stat_rating_data)
cli::cli_inform("Loaded {nrow(stat_rating_data)} rows, {length(unique(stat_rating_data$player_id))} players")

# Sort by player + date
data.table::setorder(stat_rating_data, player_id, match_date_rating)

# Precompute per-player data ----
cli::cli_h1("Precomputing per-player splits")

# Sample players for speed
player_game_counts <- stat_rating_data[, .(n = data.table::uniqueN(match_id)), by = player_id]
eligible_players <- player_game_counts[n >= min_player_games]$player_id
set.seed(seed)
if (length(eligible_players) > sample_n) {
  sampled_players <- sample(eligible_players, sample_n)
} else {
  sampled_players <- eligible_players
}
dt <- stat_rating_data[player_id %in% sampled_players]
cli::cli_inform("Using {length(sampled_players)} players (of {length(eligible_players)} eligible)")

# Resolve position groups (should already exist from prepare_stat_rating_data)
if (!"pos_group" %in% names(dt)) {
  .resolve_skill_positions(dt)
}

# Build vectorized optimization structures ----
# Stack all player data into flat vectors for O(n) objective evaluation
# (eliminates R-level per-player loop that dominated runtime)
dt[, player_num := .GRP, by = player_id]
dt[, game_num := seq_len(.N), by = player_num]
dt[, d_rel := as.numeric(match_date_rating) - as.numeric(match_date_rating[1]),
   by = player_num]

n_total <- nrow(dt)
all_d_rel <- dt$d_rel
all_tog <- as.numeric(dt$tog)
all_tog[is.na(all_tog)] <- 1
all_group <- dt$player_num
group_start <- dt[, .I[1], by = player_num]$V1

# Prediction mask: players with > min_player_games AND game_num > min_player_games
player_ns <- dt[, .N, by = player_num]
elig_players <- player_ns[N > min_player_games]$player_num
pred_mask <- dt$player_num %in% elig_players & dt$game_num > min_player_games
pred_idx <- which(pred_mask)
prev_idx <- pred_idx - 1L  # cumsum at previous game (safe: no boundary crossing)

# Modal position per player
player_modal_pos <- dt[, {
  pg <- pos_group[!is.na(pos_group)]
  tt <- if (length(pg) > 0) table(pg) else character(0)
  list(modal_pos = if (length(tt) > 0) names(tt)[which.max(tt)] else "MIDFIELDER")
}, by = player_num]
pred_pos <- player_modal_pos$modal_pos[all_group[pred_idx]]

# Grouped cumsum: O(n) via single cumsum + boundary offset subtraction
grouped_cumsum <- function(x, grp, grp_start) {
  cx <- cumsum(x)
  offsets <- c(0, cx[grp_start[-1] - 1L])
  cx - offsets[grp]
}

cli::cli_inform("Vectorized: {n_total} rows, {length(group_start)} players, {length(pred_idx)} prediction games")

# Compute grand means per stat ----
stat_defs <- stat_rating_definitions()
rate_defs <- stat_defs[stat_defs$type == "rate", ]
eff_defs <- stat_defs[stat_defs$type == "efficiency", ]

# Rate stats: TOG-weighted grand mean (per-full-game rate)
# For tog_adjusted=FALSE stats, use games (exposure=1) instead of TOG
# Compute both global and per-position means (matching estimate_player_stat_ratings)
grand_means <- list()
pos_grand_means <- list()
stat_tog_adjusted <- list()  # Track per source_col whether to use TOG
pos_groups <- names(stat_rating_position_map())

for (i in seq_len(nrow(rate_defs))) {
  src <- rate_defs$source_col[i]
  if (!src %in% names(dt)) next
  is_tog_adj <- is.na(rate_defs$tog_adjusted[i]) || isTRUE(rate_defs$tog_adjusted[i])
  stat_tog_adjusted[[src]] <- is_tog_adj
  vals <- as.numeric(dt[[src]])
  vals[is.na(vals)] <- 0
  if (is_tog_adj) {
    togs <- as.numeric(dt$tog)
    togs[is.na(togs)] <- 1
  } else {
    togs <- rep(1, nrow(dt))
  }
  total_exposure <- sum(togs)
  gm <- if (total_exposure > 0) sum(vals) / total_exposure else 0
  grand_means[[src]] <- gm

  # Per-position means (panna-style positional priors)
  pm <- stats::setNames(rep(gm, length(pos_groups)), pos_groups)
  for (pg in pos_groups) {
    idx <- which(dt$pos_group == pg)
    if (length(idx) > 0) {
      pos_exp <- sum(togs[idx])
      if (pos_exp > 0) pm[pg] <- sum(vals[idx]) / pos_exp
    }
  }
  pos_grand_means[[src]] <- pm
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
pos_grand_props <- list()
eff_flat_data <- list()

for (i in seq_len(nrow(eff_defs))) {
  stat_nm <- eff_defs$stat_name[i]
  success_spec <- eff_defs$success_col[i]
  attempts_spec <- eff_defs$attempts_col[i]
  if (is.na(success_spec) || is.na(attempts_spec)) next

  # For played_only stats, compute grand means from played rows only
  is_po <- !is.na(eff_defs$played_only[i]) && isTRUE(eff_defs$played_only[i])
  dt_eff <- if (is_po && "avail_only" %in% names(dt)) dt[avail_only == FALSE] else dt

  # Compute successes and attempts
  if (success_spec %in% names(dt_eff)) {
    all_succ <- as.numeric(dt_eff[[success_spec]])
  } else {
    all_succ <- .compute_denom(dt_eff, success_spec)
  }
  all_succ[is.na(all_succ)] <- 0
  all_att <- .compute_denom(dt_eff, attempts_spec)
  all_att[is.na(all_att)] <- 0
  all_succ <- pmin(all_succ, all_att)

  total_att <- sum(all_att)
  gp <- if (total_att > 0) sum(all_succ) / total_att else 0.5
  grand_props[[stat_nm]] <- gp

  # Per-position proportions
  pp <- stats::setNames(rep(gp, length(pos_groups)), pos_groups)
  for (pg in pos_groups) {
    idx <- which(dt_eff$pos_group == pg)
    if (length(idx) > 0) {
      pos_att <- sum(all_att[idx])
      if (pos_att > 0) pp[pg] <- sum(all_succ[idx]) / pos_att
    }
  }
  pos_grand_props[[stat_nm]] <- pp

  # Store flat vectors for vectorized optimization (full dt for non-played_only)
  if (!is_po) {
    eff_flat_data[[stat_nm]] <- list(successes = all_succ, attempts = all_att)
  }
  # played_only stats compute their vectors directly in the task-building loop
}


# Worker function for parallel optimization ----
# Self-contained: includes grouped_cumsum + multi_start_optim so workers
# don't need closures or exported globals.
optimize_single_stat <- function(task, shared) {
  grouped_cumsum <- function(x, grp, grp_start) {
    cx <- cumsum(x)
    offsets <- c(0, cx[grp_start[-1] - 1L])
    cx - offsets[grp]
  }

  multi_start_optim <- function(fn, starts, lower, upper, top_n = 5L) {
    # Phase 1: evaluate all starts cheaply to find promising regions
    start_vals <- vapply(starts, fn, numeric(1))
    # Phase 2: run BOBYQA from the top_n best starting points
    top_idx <- order(start_vals)[seq_len(min(top_n, length(starts)))]
    best <- NULL
    for (i in top_idx) {
      opt <- tryCatch(
        nloptr::bobyqa(x0 = starts[[i]], fn = fn,
                       lower = lower, upper = upper,
                       control = list(maxeval = 2000, xtol_rel = 1e-10)),
        error = function(e) NULL
      )
      if (!is.null(opt)) {
        # Normalize to optim()-compatible output
        opt_out <- list(par = opt$par, value = opt$value)
        if (is.null(best) || opt_out$value < best$value) {
          best <- opt_out
        }
      }
    }
    best
  }

  # Use per-stat overrides if present (for played_only stats),

  # otherwise use shared structures
  s <- if (!is.null(task$override_shared)) task$override_shared else shared
  all_d_rel <- s$all_d_rel
  all_group <- s$all_group
  group_start <- s$group_start
  pred_idx <- s$pred_idx
  prev_idx <- s$prev_idx

  if (task$type == "rate") {
    fn <- function(par) {
      lambda <- par[1]; prior_strength <- par[2]
      exp_pos <- exp(pmin(lambda * all_d_rel, 500))
      exp_neg <- exp(pmax(-lambda * all_d_rel, -500))
      cum_ev <- grouped_cumsum(exp_pos * task$stat_events, all_group, group_start)
      cum_ex <- grouped_cumsum(exp_pos * task$stat_exposure, all_group, group_start)
      w_ev <- exp_neg[pred_idx] * cum_ev[prev_idx]
      w_ex <- exp_neg[pred_idx] * cum_ex[prev_idx]
      predicted <- (task$mu0_vec * prior_strength + w_ev) / (prior_strength + w_ex)
      residual <- (predicted[task$pred_ok] - task$pred_actual[task$pred_ok])^2
      result <- sum(task$pred_wt[task$pred_ok] * residual) / task$total_wt
      if (is.nan(result) || is.infinite(result)) 1e6 else result
    }
  } else {
    fn <- function(par) {
      lambda <- par[1]; prior_strength <- par[2]
      exp_pos <- exp(pmin(lambda * all_d_rel, 500))
      exp_neg <- exp(pmax(-lambda * all_d_rel, -500))
      cum_s <- grouped_cumsum(exp_pos * task$stat_succ, all_group, group_start)
      cum_a <- grouped_cumsum(exp_pos * task$stat_att, all_group, group_start)
      w_s <- exp_neg[pred_idx] * cum_s[prev_idx]
      w_a <- exp_neg[pred_idx] * cum_a[prev_idx]
      alpha0 <- task$mu0_vec * prior_strength
      beta0 <- (1 - task$mu0_vec) * prior_strength
      predicted <- pmax(pmin((alpha0 + w_s) / (alpha0 + beta0 + w_a), 1 - 1e-8), 1e-8)
      loss <- -(task$eff_actual * log(predicted) + (1 - task$eff_actual) * log(1 - predicted))
      result <- sum(task$pred_att[task$eff_pred_ok] * loss[task$eff_pred_ok]) / task$eff_total_wt
      if (is.nan(result) || is.infinite(result)) 1e6 else result
    }
  }

  # Evaluate loss at previous baked-in params (if available)
  prev_loss <- if (!is.null(task$old_par)) {
    tryCatch(fn(task$old_par), error = function(e) NA_real_)
  } else {
    NA_real_
  }

  opt <- multi_start_optim(fn, task$starts, task$lower, task$upper)

  if (!is.null(opt)) {
    list(
      stat_name = task$stat_nm,
      type = task$type,
      category = task$stat_cat,
      lambda = opt$par[1],
      prior_strength = opt$par[2],
      half_life_days = log(2) / opt$par[1],
      loss = opt$value,
      prev_loss = prev_loss,
      loss_type = task$loss_type
    )
  } else {
    NULL
  }
}

# Build optimization task list ----
cli::cli_h1("Building optimization tasks")

# Load current baked-in params for comparison
old_stat_params <- .stat_rating_params()

## Multi-start grids ----
rate_starts <- list(
  c(0.001, 0.05), c(0.001, 1),   c(0.001, 5),   c(0.001, 20),
  c(0.003, 0.05), c(0.003, 1),   c(0.003, 5),   c(0.003, 20),
  c(0.005, 0.05), c(0.005, 0.5), c(0.005, 3),   c(0.005, 10),
  c(0.01,  0.05), c(0.01,  1),   c(0.01,  10),  c(0.01,  30),
  c(0.02,  0.05), c(0.02,  0.5), c(0.03,  0.05), c(0.03,  0.5)
)

eff_starts <- list(
  c(0.00005, 10), c(0.00005, 50), c(0.00005, 200),
  c(0.001, 0.5),  c(0.001, 5),    c(0.001, 30),  c(0.001, 100),
  c(0.003, 0.5),  c(0.003, 5),    c(0.003, 30),  c(0.003, 100),
  c(0.005, 10),   c(0.005, 50),   c(0.005, 200),
  c(0.01,  20),   c(0.01,  100),  c(0.01,  300),
  c(0.02,  0.1),  c(0.02,  1),    c(0.03,  0.5), c(0.04,  0.2)
)

tasks <- list()

## Rate stat tasks ----
for (i in seq_len(nrow(rate_defs))) {
  stat_nm <- rate_defs$stat_name[i]
  src_col <- rate_defs$source_col[i]
  stat_cat <- rate_defs$category[i]

  if (!src_col %in% names(dt)) {
    cli::cli_warn("Skipping {stat_nm}: column {src_col} not found")
    next
  }

  is_tog_adj <- isTRUE(stat_tog_adjusted[[src_col]])

  stat_events <- as.numeric(dt[[src_col]])
  stat_events[is.na(stat_events)] <- 0
  stat_exposure <- if (is_tog_adj) all_tog else rep(1, n_total)

  mu0_global <- grand_means[[src_col]]
  mu0_pos_map <- pos_grand_means[[src_col]]
  mu0_vec <- mu0_pos_map[pred_pos]
  mu0_vec[is.na(mu0_vec)] <- mu0_global

  stat_pred_wt <- stat_exposure[pred_idx]
  stat_pred_ok <- stat_pred_wt > 0
  stat_pred_actual <- stat_events[pred_idx] / pmax(stat_pred_wt, 0.1)
  stat_total_wt <- sum(stat_pred_wt[stat_pred_ok])
  if (stat_total_wt == 0) next

  old_p <- old_stat_params[[stat_nm]]
  old_par <- if (!is.null(old_p)) c(old_p$lambda, old_p$prior_strength) else NULL

  tasks[[stat_nm]] <- list(
    stat_nm = stat_nm, type = "rate", stat_cat = stat_cat,
    loss_type = "mse",
    stat_events = stat_events, stat_exposure = stat_exposure,
    mu0_vec = mu0_vec,
    pred_wt = stat_pred_wt, pred_ok = stat_pred_ok,
    pred_actual = stat_pred_actual, total_wt = stat_total_wt,
    starts = rate_starts, lower = c(0.0001, 0.01), upper = c(0.04, 100),
    old_par = old_par
  )
}

## Efficiency stat tasks ----
# For played_only stats (e.g. cond_tog), we need filtered vectorized structures
# that exclude avail_only rows, matching estimate_player_stat_ratings() behavior.
use_played_only_flag <- !is.na(eff_defs$played_only) & eff_defs$played_only == TRUE
has_avail_only <- "avail_only" %in% names(dt)

# Pre-build filtered structures for played_only stats (computed once, shared)
if (any(use_played_only_flag) && has_avail_only) {
  dt_played <- dt[avail_only == FALSE]
  dt_played[, player_num_po := .GRP, by = player_id]
  dt_played[, game_num_po := seq_len(.N), by = player_num_po]
  dt_played[, d_rel_po := as.numeric(match_date_rating) - as.numeric(match_date_rating[1]),
            by = player_num_po]

  po_d_rel <- dt_played$d_rel_po
  po_group <- dt_played$player_num_po
  po_group_start <- dt_played[, .I[1], by = player_num_po]$V1

  po_player_ns <- dt_played[, .N, by = player_num_po]
  po_elig <- po_player_ns[N > min_player_games]$player_num_po
  po_pred_mask <- dt_played$player_num_po %in% po_elig & dt_played$game_num_po > min_player_games
  po_pred_idx <- which(po_pred_mask)
  po_prev_idx <- po_pred_idx - 1L

  po_modal_pos <- dt_played[, {
    pg <- pos_group[!is.na(pos_group)]
    tt <- if (length(pg) > 0) table(pg) else character(0)
    list(modal_pos = if (length(tt) > 0) names(tt)[which.max(tt)] else "MIDFIELDER")
  }, by = player_num_po]
  po_pred_pos <- po_modal_pos$modal_pos[po_group[po_pred_idx]]
}

for (i in seq_len(nrow(eff_defs))) {
  stat_nm <- eff_defs$stat_name[i]
  stat_cat <- eff_defs$category[i]

  if (!stat_nm %in% names(grand_props)) {
    cli::cli_warn("Skipping {stat_nm}: could not compute grand proportion")
    next
  }

  # Determine if this stat uses played_only filtering
  is_played_only <- use_played_only_flag[i] && has_avail_only

  if (is_played_only) {
    # Use filtered data (only actual games, no avail_only rows)
    success_spec <- eff_defs$success_col[i]
    attempts_spec <- eff_defs$attempts_col[i]
    if (success_spec %in% names(dt_played)) {
      stat_succ <- as.numeric(dt_played[[success_spec]])
    } else {
      stat_succ <- .compute_denom(dt_played, success_spec)
    }
    stat_succ[is.na(stat_succ)] <- 0
    stat_att <- .compute_denom(dt_played, attempts_spec)
    stat_att[is.na(stat_att)] <- 0
    stat_succ <- pmin(stat_succ, stat_att)

    use_pred_idx <- po_pred_idx
    use_pred_pos <- po_pred_pos
  } else {
    eff_d <- eff_flat_data[[stat_nm]]
    stat_succ <- eff_d$successes
    stat_att <- eff_d$attempts
    use_pred_idx <- pred_idx
    use_pred_pos <- pred_pos
  }

  mu0_global <- max(min(grand_props[[stat_nm]], 1 - 1e-6), 1e-6)
  mu0_pos_map <- pos_grand_props[[stat_nm]]
  mu0_vec <- mu0_pos_map[use_pred_pos]
  mu0_vec[is.na(mu0_vec)] <- mu0_global
  mu0_vec <- pmax(pmin(mu0_vec, 1 - 1e-6), 1e-6)

  pred_succ <- stat_succ[use_pred_idx]
  pred_att <- stat_att[use_pred_idx]
  eff_pred_ok <- pred_att > 0
  eff_actual <- ifelse(pred_att > 0, pred_succ / pred_att, mu0_global)
  eff_actual <- pmax(pmin(eff_actual, 1 - 1e-8), 1e-8)
  eff_total_wt <- sum(pred_att[eff_pred_ok])
  if (eff_total_wt == 0) next

  old_p <- old_stat_params[[stat_nm]]
  old_par <- if (!is.null(old_p)) c(old_p$lambda, old_p$prior_strength) else NULL

  task_entry <- list(
    stat_nm = stat_nm, type = "efficiency", stat_cat = stat_cat,
    loss_type = "logloss",
    stat_succ = stat_succ, stat_att = stat_att,
    mu0_vec = mu0_vec,
    pred_att = pred_att, eff_pred_ok = eff_pred_ok,
    eff_actual = eff_actual, eff_total_wt = eff_total_wt,
    starts = eff_starts,
    lower = c(0.00001, 0.1),
    upper = c(0.05, 500),
    old_par = old_par
  )

  # For played_only stats, override shared vectorized structures
  if (is_played_only) {
    task_entry$override_shared <- list(
      all_d_rel = po_d_rel,
      all_group = po_group,
      group_start = po_group_start,
      pred_idx = po_pred_idx,
      prev_idx = po_prev_idx
    )
  }

  tasks[[stat_nm]] <- task_entry
}

cli::cli_inform("Built {length(tasks)} optimization tasks ({sum(vapply(tasks, function(t) t$type == 'rate', logical(1)))} rate, {sum(vapply(tasks, function(t) t$type == 'efficiency', logical(1)))} efficiency)")

# Run optimizations in parallel ----
shared_data <- list(
  all_d_rel = all_d_rel,
  all_group = all_group,
  group_start = group_start,
  pred_idx = pred_idx,
  prev_idx = prev_idx
)

n_cores <- max(1L, parallel::detectCores() - 2L)
cli::cli_h1("Optimizing {length(tasks)} stats on {n_cores} cores")
t0 <- proc.time()

# Close stale connections from prior pipeline phases to avoid exhausting
# R's connection pool (limit ~128) when creating parallel cluster workers
gc()
stale <- showConnections(all = FALSE)
if (nrow(stale) > 3) {
  cli::cli_inform("Closing {nrow(stale) - 3} stale connections before cluster creation")
  for (conn_id in rownames(stale)[-(1:3)]) {
    tryCatch(close(getConnection(as.integer(conn_id))), error = function(e) NULL)
  }
}

cl <- parallel::makeCluster(n_cores)
raw_results <- tryCatch({
  parallel::parLapply(cl, tasks, optimize_single_stat, shared = shared_data)
}, finally = {
  parallel::stopCluster(cl)
})

elapsed <- (proc.time() - t0)["elapsed"]
cli::cli_inform("Parallel optimization done in {round(elapsed, 1)}s")

# Collect results ----
stat_results <- list()
for (res in raw_results) {
  if (!is.null(res)) {
    stat_results[[res$stat_name]] <- res
    cli::cli_inform("{res$stat_name} ({res$category}): lambda={round(res$lambda, 5)} (half-life={round(res$half_life_days)}d), prior={round(res$prior_strength, 2)}, {res$loss_type}={round(res$loss, 4)}")
  }
}

# Build optimized params ----
cli::cli_h1("Building optimized parameter set")

opt_params <- default_stat_rating_params()

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
    prev <- if (!is.null(x$prev_loss) && !is.na(x$prev_loss)) round(x$prev_loss, 4) else NA_real_
    delta_pct <- if (!is.na(prev) && prev > 0) round((x$loss - prev) / prev * 100, 2) else NA_real_
    data.frame(
      stat = x$stat_name,
      type = x$type,
      category = x$category,
      lambda = round(x$lambda, 5),
      half_life_days = round(x$half_life_days),
      prior_strength = round(x$prior_strength, 2),
      prev_loss = prev,
      loss = round(x$loss, 4),
      delta_pct = delta_pct,
      loss_type = x$loss_type,
      stringsAsFactors = FALSE
    )
  }))
  data.table::setDT(summary_df)
  data.table::setorder(summary_df, half_life_days)
  cli::cli_h2("Optimization Summary")
  print(summary_df, row.names = FALSE)

  # Bound-proximity warnings ----
  # Rate bounds: lambda [0.0001, 0.04], prior [0.01, 100]
  # Efficiency bounds: lambda [0.00001, 0.02], prior [0.5, 500]
  # Use log-scale proximity: flag if within 2x of lower or upper bound
  bound_warnings <- character(0)
  for (x in stat_results) {
    if (x$type == "rate") {
      lam_lo <- 0.0001; lam_hi <- 0.04; pri_lo <- 0.01; pri_hi <- 100
    } else {
      lam_lo <- 0.00001; lam_hi <- 0.05; pri_lo <- 0.1; pri_hi <- 500
    }
    if (x$lambda <= lam_lo * 2) {
      bound_warnings <- c(bound_warnings, paste0("  ", x$stat_name, ": lambda=", round(x$lambda, 6), " near lower bound (", lam_lo, ")"))
    }
    if (x$lambda >= lam_hi / 2) {
      bound_warnings <- c(bound_warnings, paste0("  ", x$stat_name, ": lambda=", round(x$lambda, 6), " near upper bound (", lam_hi, ")"))
    }
    if (x$prior_strength <= pri_lo * 2) {
      bound_warnings <- c(bound_warnings, paste0("  ", x$stat_name, ": prior=", round(x$prior_strength, 3), " near lower bound (", pri_lo, ")"))
    }
    if (x$prior_strength >= pri_hi / 2) {
      bound_warnings <- c(bound_warnings, paste0("  ", x$stat_name, ": prior=", round(x$prior_strength, 3), " near upper bound (", pri_hi, ")"))
    }
  }
  if (length(bound_warnings) > 0) {
    cli::cli_h2("Bound Proximity Warnings")
    cli::cli_warn("Parameters near optimization bounds (may need wider bounds):")
    for (w in bound_warnings) cli::cli_inform(w)
  } else {
    cli::cli_alert_success("No parameters stuck at optimization bounds")
  }
}

# Update skill_config.R ----
# Writes optimized values directly into .stat_rating_params() so they're
# baked into the package without manual copy-paste.
if (length(stat_results) > 0) {
  cli::cli_h1("Updating skill_config.R")

  config_path <- file.path("R", "skill_config.R")
  config_lines <- readLines(config_path)

  # Find the function body boundaries
  fn_start <- grep("^\\.stat_rating_params <- function\\(\\)", config_lines)
  if (length(fn_start) != 1) {
    cli::cli_warn("Could not find .stat_rating_params in {config_path}, skipping auto-update")
  } else {
    # Find matching closing brace — track brace depth from fn_start
    depth <- 0
    fn_end <- NA_integer_
    for (i in fn_start:length(config_lines)) {
      depth <- depth + nchar(gsub("[^{]", "", config_lines[i])) -
                        nchar(gsub("[^}]", "", config_lines[i]))
      if (depth == 0 && i > fn_start) {
        fn_end <- i
        break
      }
    }

    if (is.na(fn_end)) {
      cli::cli_warn("Could not find end of .stat_rating_params, skipping auto-update")
    } else {
      # Get stat definitions to separate rate vs efficiency
      defs <- stat_rating_definitions()

      # Build the replacement function body
      # Pad stat names to align = signs
      all_names <- names(stat_results)
      max_len <- max(nchar(all_names))

      rate_lines <- character(0)
      eff_lines <- character(0)

      for (nm in all_names) {
        res <- stat_results[[nm]]
        padded <- formatC(nm, width = -max_len, flag = "-")
        # Format lambda: use scientific for very small values
        lam_str <- if (res$lambda < 0.0001) {
          formatC(res$lambda, format = "e", digits = 0)
        } else {
          formatC(res$lambda, format = "f", digits = 5)
        }
        pri_str <- formatC(res$prior_strength, format = "f", digits = 2)
        line <- paste0("    ", padded, " = list(lambda = ", lam_str, ", prior_strength = ", pri_str, ")")

        if (res$type == "rate") {
          rate_lines <- c(rate_lines, line)
        } else {
          eff_lines <- c(eff_lines, line)
        }
      }

      # Join with commas (all but last in each section get a comma)
      all_param_lines <- c(
        "    # Rate stats (Gamma-Poisson, optimized via multi-start MSE)",
        paste0(rate_lines, ","),
        "    # Efficiency stats (Beta-Binomial, optimized via multi-start log-loss)",
        paste0(eff_lines, ",")
      )
      # Remove trailing comma from the very last entry
      all_param_lines[length(all_param_lines)] <- sub(",$", "", all_param_lines[length(all_param_lines)])

      new_fn <- c(
        ".stat_rating_params <- function() {",
        "  list(",
        all_param_lines,
        "  )",
        "}"
      )

      # Replace the old function
      trailing <- if (fn_end < length(config_lines)) {
        config_lines[(fn_end + 1):length(config_lines)]
      } else {
        character(0)
      }
      config_lines <- c(
        config_lines[1:(fn_start - 1)],
        new_fn,
        trailing
      )

      writeLines(config_lines, config_path)
      cli::cli_alert_success("Updated .stat_rating_params() in {config_path} with {length(stat_results)} optimized stats")
    }
  }
}
