# Player Stat Rating Estimation
# ==============================
# Bayesian conjugate prior estimation of per-stat player ratings,
# with exponential time decay and position-specific priors.
#
# Rate stats use Gamma-Poisson conjugate model (counts per game).
# Efficiency stats use Beta-Binomial conjugate model (proportions).
#
# Data prep is in player_skills_data.R; profiles in player_skills_profile.R.

# ============================================================================
# Core stat rating estimation
# ============================================================================

#' Estimate player stat ratings using Bayesian conjugate priors
#'
#' For each player, estimates "true stat rating" at a reference date using all prior
#' matches. Uses conjugate Bayesian updating with exponential time decay.
#'
#' \strong{Rate stats (per-game):} Gamma-Poisson model. Raw event counts are
#' decay-weighted; the Gamma prior is centered on the position mean with
#' strength controlled by \code{prior_games}. Posterior mean gives the
#' estimated per-game rate (full-TOG adjusted).
#'
#' \strong{Efficiency stats (proportions):} Beta-Binomial model. Successes
#' and attempts are decay-weighted; the Beta prior is centered on the
#' position mean with strength controlled by \code{prior_attempts}.
#'
#' @param stat_rating_data A data.table from \code{.prepare_stat_rating_data()}.
#' @param ref_date Date to estimate skills as of. Only matches before this
#'   date are used. If NULL, includes all available matches (sets ref_date
#'   to one day after the latest match in the data).
#' @param params Named list of hyperparameters from \code{default_stat_rating_params()}.
#' @param stat_defs Output of \code{stat_rating_definitions()}. If NULL, uses default.
#' @param compute_ci Logical. If TRUE (default), compute credible intervals
#'   (\code{_lower}/\code{_upper} columns) using qgamma/qbeta. Set to FALSE
#'   to skip interval computation for faster batch processing.
#' @param adjust_opponents Logical. If TRUE, applies opponent quality adjustment
#'   via \code{\link{adjust_stat_ratings_for_opponents}} after estimation,
#'   adding \code{{stat}_adj_rating} columns. Default FALSE.
#'
#' @return A data.table with one row per player containing:
#'   \code{player_id}, \code{player_name}, \code{pos_group},
#'   \code{n_games}, \code{wt_games}, \code{ref_date},
#'   and for each stat: \code{{stat}_rating}, \code{{stat}_rating_lower}, \code{{stat}_rating_upper}.
#'
#' @importFrom data.table as.data.table copy
#' @importFrom stats qgamma qbeta
#' @export
estimate_player_stat_ratings <- function(stat_rating_data, ref_date = NULL,
                                    params = NULL, stat_defs = NULL,
                                    compute_ci = TRUE,
                                    adjust_opponents = FALSE) {
  if (is.null(params)) params <- default_stat_rating_params()
  if (is.null(stat_defs)) stat_defs <- stat_rating_definitions()

  dt <- data.table::as.data.table(stat_rating_data)

  # Ensure date column
  if (!inherits(dt$match_date_rating, "Date")) {
    dt[, match_date_rating := as.Date(match_date_rating)]
  }

  # Determine reference date
  if (is.null(ref_date)) {
    ref_date <- max(dt$match_date_rating, na.rm = TRUE) + 1L
  }
  ref_date <- as.Date(ref_date)

  # Filter to matches before ref_date (creates a copy — no prior copy needed)
  dt <- dt[match_date_rating < ref_date]

  if (nrow(dt) == 0) {
    cli::cli_warn("No match data available before ref_date.")
    return(data.table::data.table())
  }

  # Compute days since and decay weight (using rate lambda for game counting)
  dt[, days_since := as.numeric(ref_date - match_date_rating)]

  # Ensure avail_only flag exists (rows from .prepare_stat_rating_data zero-TOG expansion)
  if (!"avail_only" %in% names(dt)) dt[, avail_only := FALSE]
  dt[is.na(avail_only), avail_only := FALSE]

  # Weighted games count and TOG-adjusted "80s" (full-game equivalents)
  dt[, .w_rate := exp(-params$lambda_rate * days_since)]
  dt[, .tog_safe := data.table::fifelse(is.na(tog), 0, as.numeric(tog))]

  # Player metadata and game counts: exclude availability-only rows
  dt_played <- dt[avail_only == FALSE]
  player_meta <- dt_played[, .(
    player_name = data.table::last(player_name),
    pos_group = {
      pg <- pos_group[!is.na(pos_group)]
      if (length(pg) == 0) NA_character_
      else { tt <- table(pg); names(tt)[which.max(tt)] }
    }
  ), by = player_id]

  game_counts <- dt_played[, {
    first <- !duplicated(match_id)
    .(n_games = sum(first),
      wt_games = sum(.w_rate[first], na.rm = TRUE),
      n_80s = sum(.tog_safe[first], na.rm = TRUE),
      wt_80s = sum((.w_rate * .tog_safe)[first], na.rm = TRUE))
  }, by = player_id]

  # Credible interval quantiles
  ci_alpha <- (1 - params$credible_level) / 2

  # Process each stat
  rate_defs <- stat_defs[stat_defs$type == "rate", ]
  eff_defs <- stat_defs[stat_defs$type == "efficiency", ]

  skill_results <- list()
  skipped_stats <- character(0)

  # Resolve per-stat lambda/prior: stat_params > category_params > global fallback
  stat_params <- params$stat_params
  cat_params <- params$category_params
  .resolve_rate_params <- function(stat_name, category) {
    if (!is.null(stat_params) && stat_name %in% names(stat_params)) {
      sp <- stat_params[[stat_name]]
      return(list(lambda = sp$lambda, prior = sp$prior_strength))
    }
    if (!is.null(cat_params) && category %in% names(cat_params)) {
      cp <- cat_params[[category]]
      return(list(lambda = cp$lambda, prior = cp$prior_strength))
    }
    list(lambda = params$lambda_rate, prior = params$prior_games)
  }

  # --- Rate stats (Gamma-Poisson) ---
  for (i in seq_len(nrow(rate_defs))) {
    stat_nm <- rate_defs$stat_name[i]
    src_col <- rate_defs$source_col[i]
    stat_cat <- rate_defs$category[i]

    if (!src_col %in% names(dt)) {
      skipped_stats <- c(skipped_stats, stat_nm)
      next
    }

    # Use opponent-adjusted values when available (e.g. disposals_oadj)
    oadj_col <- paste0(src_col, "_oadj")
    use_col <- if (oadj_col %in% names(dt)) oadj_col else src_col

    # Per-stat lambda and prior strength
    rp <- .resolve_rate_params(stat_nm, stat_cat)
    stat_lambda <- rp$lambda
    prior_str <- rp$prior

    vals <- as.numeric(dt[[use_col]])
    vals[is.na(vals)] <- 0

    # Use TOG as exposure denominator unless stat is not TOG-adjusted
    is_tog_adj <- is.na(rate_defs$tog_adjusted[i]) || isTRUE(rate_defs$tog_adjusted[i])
    if (is_tog_adj) {
      tog_vals <- as.numeric(dt$tog)
      tog_vals[is.na(tog_vals)] <- 1
    } else {
      tog_vals <- rep(1, nrow(dt))
    }

    w_vec <- exp(-stat_lambda * dt$days_since)

    data.table::set(dt, j = ".wnum", value = w_vec * vals)
    data.table::set(dt, j = ".wden", value = w_vec * tog_vals)
    data.table::set(dt, j = ".raw_vals", value = vals)
    data.table::set(dt, j = ".raw_tog", value = tog_vals)

    agg <- dt[, .(w_num = sum(.wnum, na.rm = TRUE),
                   w_den = sum(.wden, na.rm = TRUE),
                   .raw_num = sum(.raw_vals, na.rm = TRUE),
                   .raw_den = sum(.raw_tog, na.rm = TRUE)), by = player_id]

    # Position-specific prior: weighted mean within each position group
    agg[player_meta, pos_group := i.pos_group, on = "player_id"]

    # Compute grand mean per game (minutes-weighted)
    total_exposure <- sum(w_vec * tog_vals, na.rm = TRUE)
    grand_mean <- if (total_exposure > 0) sum(w_vec * vals, na.rm = TRUE) / total_exposure else 0

    # Position-specific prior means (weighted mean within each position group)
    pos_groups <- names(stat_rating_position_map())
    pos_means_dt <- dt[!is.na(pos_group),
      .(pos_mean = if (sum(.wden, na.rm = TRUE) > 0)
                     sum(.wnum, na.rm = TRUE) / sum(.wden, na.rm = TRUE)
                   else grand_mean),
      by = pos_group]
    pos_means <- stats::setNames(rep(grand_mean, length(pos_groups)), pos_groups)
    pos_means[pos_means_dt$pos_group] <- pos_means_dt$pos_mean

    # Compute posterior: vectorized position lookup (no R-level loop)
    agg[, alpha0 := {
      mu <- pos_means[pos_group]
      mu[is.na(mu)] <- grand_mean
      mu * prior_str
    }]

    agg[, `:=`(
      alpha_post = alpha0 + w_num,
      beta_post = prior_str + w_den
    )]

    # Posterior mean = alpha / beta (per-game rate, full-TOG adjusted)
    rating_col <- paste0(stat_nm, "_rating")
    lower_col <- paste0(stat_nm, "_rating_lower")
    upper_col <- paste0(stat_nm, "_rating_upper")

    agg[, (rating_col) := alpha_post / beta_post]
    if (compute_ci) {
      agg[, (lower_col) := stats::qgamma(ci_alpha, shape = alpha_post, rate = beta_post)]
      agg[, (upper_col) := stats::qgamma(1 - ci_alpha, shape = alpha_post, rate = beta_post)]
    }

    raw_col <- paste0(stat_nm, "_raw")
    n80_col <- paste0(stat_nm, "_n80s")
    wt80_col <- paste0(stat_nm, "_wt80s")
    agg[, (raw_col) := data.table::fifelse(.raw_den > 0, .raw_num / .raw_den, NA_real_)]
    agg[, (n80_col) := .raw_den]
    agg[, (wt80_col) := w_den]

    keep_cols <- c("player_id", rating_col, raw_col, n80_col, wt80_col)
    if (compute_ci) keep_cols <- c(keep_cols, lower_col, upper_col)
    skill_results[[stat_nm]] <- agg[, keep_cols, with = FALSE]
  }

  # Resolve per-stat efficiency params
  .resolve_eff_params <- function(stat_name) {
    if (!is.null(stat_params) && stat_name %in% names(stat_params)) {
      sp <- stat_params[[stat_name]]
      return(list(lambda = sp$lambda, prior = sp$prior_strength))
    }
    list(lambda = params$lambda_efficiency, prior = params$prior_attempts)
  }

  # --- Efficiency stats (Beta-Binomial) ---
  for (i in seq_len(nrow(eff_defs))) {
    stat_nm <- eff_defs$stat_name[i]
    success_spec <- eff_defs$success_col[i]
    attempts_spec <- eff_defs$attempts_col[i]

    if (is.na(success_spec) || is.na(attempts_spec)) next

    # Filter rows based on played_only flag
    use_played_only <- !is.na(eff_defs$played_only[i]) && isTRUE(eff_defs$played_only[i])
    dt_eff <- if (use_played_only) dt[avail_only == FALSE] else dt

    # Per-stat lambda and prior strength
    ep <- .resolve_eff_params(stat_nm)
    stat_lambda <- ep$lambda
    prior_str <- ep$prior

    # Get success and attempt vectors
    if (success_spec %in% names(dt_eff)) {
      successes <- as.numeric(dt_eff[[success_spec]])
    } else {
      successes <- .compute_stat_rating_denominator(dt_eff, success_spec)
    }
    successes[is.na(successes)] <- 0

    attempts <- .compute_stat_rating_denominator(dt_eff, attempts_spec)
    attempts[is.na(attempts)] <- 0

    # Ensure successes <= attempts
    successes <- pmin(successes, attempts)

    w_vec <- exp(-stat_lambda * dt_eff$days_since)

    data.table::set(dt_eff, j = ".wnum", value = w_vec * successes)
    data.table::set(dt_eff, j = ".wden", value = w_vec * attempts)
    data.table::set(dt_eff, j = ".eff_successes", value = successes)
    data.table::set(dt_eff, j = ".eff_attempts", value = attempts)
    data.table::set(dt_eff, j = ".eff_w", value = w_vec)

    agg <- dt_eff[, .(w_num = sum(.wnum, na.rm = TRUE),
                   w_den = sum(.wden, na.rm = TRUE),
                   .raw_succ = sum(.eff_successes, na.rm = TRUE),
                   .raw_att = sum(.eff_attempts, na.rm = TRUE),
                   .wt_att = sum(.eff_w * .eff_attempts, na.rm = TRUE)), by = player_id]

    agg[player_meta, pos_group := i.pos_group, on = "player_id"]

    # Grand mean proportion
    total_attempts <- sum(w_vec * attempts, na.rm = TRUE)
    grand_prop <- if (total_attempts > 0) sum(w_vec * successes, na.rm = TRUE) / total_attempts else 0.5
    grand_prop <- max(min(grand_prop, 1 - 1e-6), 1e-6)

    # Position-specific proportions (vectorized lookup instead of per-group loop)
    use_pos <- is.na(eff_defs$pos_adjusted[i]) || isTRUE(eff_defs$pos_adjusted[i])
    pos_props <- stats::setNames(rep(grand_prop, length(pos_groups)), pos_groups)
    if (use_pos) {
      pos_props_dt <- dt_eff[!is.na(pos_group),
        .(pp = {
          pa <- sum(.wden, na.rm = TRUE)
          if (pa > 0) max(min(sum(.wnum, na.rm = TRUE) / pa, 1 - 1e-6), 1e-6)
          else grand_prop
        }),
        by = pos_group]
      pos_props[pos_props_dt$pos_group] <- pos_props_dt$pp
    }

    # Vectorized position lookup (no R-level loop)
    agg[, mu0 := {
      m <- pos_props[pos_group]
      m[is.na(m)] <- grand_prop
      pmax(pmin(m, 1 - 1e-6), 1e-6)
    }]

    agg[, `:=`(
      alpha_post = mu0 * prior_str + w_num,
      beta_post = (1 - mu0) * prior_str + w_den - w_num
    )]

    # Guard against invalid Beta parameters (can occur when weighted
    # successes exceed weighted attempts due to floating-point decay)
    n_clamped <- sum(agg$alpha_post <= 0 | agg$beta_post <= 0, na.rm = TRUE)
    if (n_clamped > 0) {
      cli::cli_warn("Efficiency stat {.val {stat_nm}}: {n_clamped} player(s) had invalid Beta parameters, clamping to 1e-4")
    }
    agg[alpha_post <= 0, alpha_post := 1e-4]
    agg[beta_post <= 0, beta_post := 1e-4]

    rating_col <- paste0(stat_nm, "_rating")
    lower_col <- paste0(stat_nm, "_rating_lower")
    upper_col <- paste0(stat_nm, "_rating_upper")

    agg[, (rating_col) := alpha_post / (alpha_post + beta_post)]
    if (compute_ci) {
      agg[, (lower_col) := stats::qbeta(ci_alpha, alpha_post, beta_post)]
      agg[, (upper_col) := stats::qbeta(1 - ci_alpha, alpha_post, beta_post)]
    }

    raw_col <- paste0(stat_nm, "_raw")
    att_col <- paste0(stat_nm, "_attempts")
    wt_att_col <- paste0(stat_nm, "_wt_attempts")
    agg[, (raw_col) := data.table::fifelse(.raw_att > 0, .raw_succ / .raw_att, NA_real_)]
    agg[, (att_col) := .raw_att]
    agg[, (wt_att_col) := .wt_att]

    keep_cols <- c("player_id", rating_col, raw_col, att_col, wt_att_col)
    if (compute_ci) keep_cols <- c(keep_cols, lower_col, upper_col)
    skill_results[[stat_nm]] <- agg[, keep_cols, with = FALSE]
  }

  if (length(skipped_stats) > 0) {
    cli::cli_warn("Skipped {length(skipped_stats)} stat(s) due to missing columns: {paste(skipped_stats, collapse = ', ')}")
  }

  # Clean up temp columns
  tmp_cols <- intersect(c(".wnum", ".wden", ".w_rate", ".tog_safe", ".raw_vals", ".raw_tog",
                          ".eff_successes", ".eff_attempts", ".eff_w"), names(dt))
  for (tc in tmp_cols) data.table::set(dt, j = tc, value = NULL)

  # Assemble result
  result <- data.table::copy(player_meta)
  result[game_counts, `:=`(n_games = i.n_games, wt_games = i.wt_games,
                            n_80s = i.n_80s, wt_80s = i.wt_80s), on = "player_id"]
  result[, ref_date := ref_date]

  for (stat_nm in names(skill_results)) {
    sr <- skill_results[[stat_nm]]
    idx <- match(result$player_id, sr$player_id)
    cols <- setdiff(names(sr), "player_id")
    for (col in cols) data.table::set(result, j = col, value = sr[[col]][idx])
  }

  # Filter to min games
  result <- result[wt_games >= params$min_games]

  # Opponent quality adjustment (post-processing on finished ratings)
  if (isTRUE(adjust_opponents)) {
    result <- adjust_stat_ratings_for_opponents(
      result, stat_rating_data, ref_date,
      stat_defs = stat_defs
    )
  }

  data.table::setorder(result, -wt_games)
  result
}


# ============================================================================
# Batch stat rating estimation
# ============================================================================

#' Estimate player stat ratings for multiple reference dates efficiently
#'
#' Uses cumulative-sum factorisation to avoid redundant aggregation across
#' dates.  Decay weight \code{exp(-lambda * (ref - match))} is split into
#' \code{exp(-lambda * ref) * exp(lambda * match)}.  The match-side factor
#' is fixed per row, so we pre-multiply values by it and take per-player
#' cumulative sums (sorted by date).
#' A data.table rolling join then looks up each ref_date in O(log n), and
#' the ref-side scalar completes the product.
#'
#' Complexity drops from O(dates * stats * rows) to O(stats * rows) for
#' the cumsums plus O(stats * players * dates) for the lookups.
#'
#' @param stat_rating_data A data.table from \code{.prepare_stat_rating_data()}.
#' @param ref_dates Date vector of reference dates.
#' @param params Named list of hyperparameters from \code{default_stat_rating_params()}.
#' @param stat_defs Output of \code{stat_rating_definitions()}. If NULL, uses default.
#' @param compute_ci Logical. If FALSE (default), skip credible interval computation.
#'
#' @return Named list of data.tables (keyed by ref_date as character), one per date.
#' @keywords internal
.estimate_stat_ratings_batch <- function(stat_rating_data, ref_dates, params = NULL,
                                   stat_defs = NULL, compute_ci = FALSE) {
  if (is.null(params)) params <- default_stat_rating_params()
  if (is.null(stat_defs)) stat_defs <- stat_rating_definitions()

  # === One-time setup ===
  t_batch_start <- proc.time()
  dt <- data.table::as.data.table(stat_rating_data)
  if (!inherits(dt$match_date_rating, "Date"))
    dt[, match_date_rating := as.Date(match_date_rating)]
  if (!"avail_only" %in% names(dt)) dt[, avail_only := FALSE]
  dt[is.na(avail_only), avail_only := FALSE]
  data.table::setorder(dt, match_date_rating)

  ref_dates_sorted <- sort(unique(as.Date(ref_dates)))
  n_dates <- length(ref_dates_sorted)

  # Numeric day offsets relative to earliest match (keeps exp() stable)
  d0 <- as.numeric(min(dt$match_date_rating))
  dt[, .d := as.numeric(match_date_rating) - d0]
  ref_d <- as.numeric(ref_dates_sorted) - d0

  # Player metadata: latest name from all data, position via cumsum per ref_date
  dt_played <- dt[avail_only == FALSE]
  player_meta <- dt_played[, .(
    player_name = data.table::last(player_name)
  ), by = player_id]

  all_pids <- player_meta$player_id
  pos_groups <- names(stat_rating_position_map())

  # Per-(player, ref_date) modal position via cumulative position counts.
  # Sort pos_groups alphabetically to match table()/which.max() tie-breaking
  # in the single-date function.
  pos_groups_alpha <- sort(pos_groups)
  pos_track <- dt_played[!is.na(pos_group), .(player_id, match_date_rating, pos_group)]
  for (pg in pos_groups_alpha)
    pos_track[, (pg) := as.integer(pos_group == pg)]
  data.table::setorder(pos_track, player_id, match_date_rating)
  for (pg in pos_groups_alpha)
    pos_track[, (pg) := cumsum(get(pg)), by = player_id]
  pos_cum <- pos_track[, c("player_id", "match_date_rating", pos_groups_alpha), with = FALSE]

  # Rolling join to get cumulative position counts at each ref_date per player
  pos_lookup <- data.table::CJ(player_id = all_pids, ref_idx = seq_len(n_dates))
  pos_lookup[, join_d := ref_dates_sorted[ref_idx] - 1L]
  data.table::setkeyv(pos_cum, c("player_id", "match_date_rating"))
  pos_joined <- pos_cum[pos_lookup,
    on = .(player_id, match_date_rating = join_d), roll = TRUE]
  pos_vals <- as.matrix(pos_joined[, pos_groups_alpha, with = FALSE])
  pos_vals[is.na(pos_vals)] <- 0L
  has_any <- rowSums(pos_vals) > 0
  modal_idx <- max.col(pos_vals, ties.method = "first")
  pos_joined[, `:=`(pos_group = NA_character_, ref_idx = pos_lookup$ref_idx)]
  pos_joined[has_any, pos_group := pos_groups_alpha[modal_idx[has_any]]]
  # pos_meta: (player_id, ref_idx, pos_group) — per-ref_date modal position
  pos_meta <- pos_joined[, .(player_id, ref_idx, pos_group)]

  # Resolve per-stat params helpers
  stat_params <- params$stat_params
  cat_params <- params$category_params
  .resolve_rate_params <- function(stat_name, category) {
    if (!is.null(stat_params) && stat_name %in% names(stat_params)) {
      sp <- stat_params[[stat_name]]
      return(list(lambda = sp$lambda, prior = sp$prior_strength))
    }
    if (!is.null(cat_params) && category %in% names(cat_params)) {
      cp <- cat_params[[category]]
      return(list(lambda = cp$lambda, prior = cp$prior_strength))
    }
    list(lambda = params$lambda_rate, prior = params$prior_games)
  }
  .resolve_eff_params <- function(stat_name) {
    if (!is.null(stat_params) && stat_name %in% names(stat_params)) {
      sp <- stat_params[[stat_name]]
      return(list(lambda = sp$lambda, prior = sp$prior_strength))
    }
    list(lambda = params$lambda_efficiency, prior = params$prior_attempts)
  }

  # === Rolling-join lookup helper ===
  # Given a cumsum table (grp, match_date_rating, cum_num, cum_den) sorted
  # by (grp, match_date_rating), look up values at each ref_date per group.
  # Uses ref_date - 1 for strict < semantics (Date is integer days).
  # Returns data.table(grp, ref_idx, cum_num, cum_den).
  .lookup_cumsums <- function(cs, grps, grp_col = "grp") {
    lookup <- data.table::CJ(grp = grps, ref_idx = seq_len(n_dates))
    lookup[, join_d := ref_dates_sorted[ref_idx] - 1L]
    data.table::setnames(cs, grp_col, "grp", skip_absent = TRUE)
    data.table::setkeyv(cs, c("grp", "match_date_rating"))
    joined <- cs[lookup, .(grp = i.grp, ref_idx = i.ref_idx,
                           cum_num = x.cum_num, cum_den = x.cum_den),
                 on = .(grp, match_date_rating = join_d), roll = TRUE]
    joined[is.na(cum_num), `:=`(cum_num = 0, cum_den = 0)]
    joined
  }

  # === Game counts via cumsum ===
  # Deduplicate to one row per (player, match), using lambda_rate decay
  dt_match <- dt_played[!duplicated(paste(player_id, match_id))]
  dt_match[, .tog_safe := data.table::fifelse(is.na(tog), 0, as.numeric(tog))]
  lam_rate <- params$lambda_rate
  dt_match[, .base_w := exp(lam_rate * .d)]

  gc_cs <- dt_match[, .(
    grp = player_id, match_date_rating,
    cum_num = .base_w,            # each unique match counts as 1 * base_w
    cum_den = .base_w * .tog_safe # weighted 80s
  )]
  data.table::setorder(gc_cs, grp, match_date_rating)
  gc_cs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]

  # Raw (unweighted) game counts — cumulative count and cumulative TOG
  gc_raw_cs <- dt_match[, .(grp = player_id, match_date_rating,
                            cum_num = 1, cum_den = .tog_safe)]
  data.table::setorder(gc_raw_cs, grp, match_date_rating)
  gc_raw_cs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]

  gc_look <- .lookup_cumsums(gc_cs, all_pids)
  gc_raw_look <- .lookup_cumsums(gc_raw_cs, all_pids)

  # Build skeleton result: (player × ref_date)
  res <- data.table::CJ(player_id = all_pids, ref_idx = seq_len(n_dates))
  res[player_meta, player_name := i.player_name, on = "player_id"]
  res[pos_meta, pos_group := i.pos_group, on = .(player_id, ref_idx)]
  res[, ref_date := ref_dates_sorted[ref_idx]]

  # Merge game counts (pre-compute final values on gc_look, then join)
  gc_look[, ref_d_val := ref_d[ref_idx]]
  gc_look[, `:=`(
    wt_games = cum_num * exp(-lam_rate * ref_d_val),
    wt_80s   = cum_den * exp(-lam_rate * ref_d_val)
  )]
  res[gc_look, `:=`(wt_games = i.wt_games, wt_80s = i.wt_80s),
      on = .(player_id = grp, ref_idx)]

  gc_raw_look[, ref_d_val := ref_d[ref_idx]]
  res[gc_raw_look, `:=`(n_games = i.cum_num, n_80s = i.cum_den),
      on = .(player_id = grp, ref_idx)]

  # Fill NAs (players with no data before a ref_date)
  for (col in c("wt_games", "wt_80s", "n_games", "n_80s"))
    data.table::set(res, which(is.na(res[[col]])), col, 0)

  # CI quantiles
  ci_alpha <- (1 - params$credible_level) / 2

  rate_defs <- stat_defs[stat_defs$type == "rate", ]
  eff_defs  <- stat_defs[stat_defs$type == "efficiency", ]
  skipped_stats <- character(0)
  t_stats <- proc.time()

  n_total_stats <- nrow(rate_defs) + nrow(eff_defs)
  cli::cli_progress_bar("Estimating stat ratings", total = n_total_stats,
                        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} stats [{cli::pb_elapsed}]")

  # === Rate stats (Gamma-Poisson) via cumsum ===
  for (i in seq_len(nrow(rate_defs))) {
    stat_nm  <- rate_defs$stat_name[i]
    src_col  <- rate_defs$source_col[i]
    stat_cat <- rate_defs$category[i]

    if (!src_col %in% names(dt)) { skipped_stats <- c(skipped_stats, stat_nm); next }

    rp <- .resolve_rate_params(stat_nm, stat_cat)
    lam <- rp$lambda
    prior_str <- rp$prior

    vals <- as.numeric(dt[[src_col]]); vals[is.na(vals)] <- 0
    is_tog_adj <- is.na(rate_defs$tog_adjusted[i]) || isTRUE(rate_defs$tog_adjusted[i])
    tog_vals <- if (is_tog_adj) { tv <- as.numeric(dt$tog); tv[is.na(tv)] <- 1; tv } else rep(1, nrow(dt))

    base_w <- exp(lam * dt$.d)

    # --- Per-player cumsums ---
    pcs <- data.table::data.table(
      grp = dt$player_id, match_date_rating = dt$match_date_rating,
      cum_num = base_w * vals, cum_den = base_w * tog_vals
    )
    data.table::setorder(pcs, grp, match_date_rating)
    pcs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]
    p_look <- .lookup_cumsums(pcs, all_pids)
    p_look[, ref_d_val := ref_d[ref_idx]]
    p_look[, `:=`(w_num = cum_num * exp(-lam * ref_d_val),
                  w_den = cum_den * exp(-lam * ref_d_val))]

    # --- Per-position cumsums (for priors) ---
    # exp(-lam*ref) cancels in the ratio, so pos_mean = cum_num_pos / cum_den_pos
    pos_cs <- data.table::data.table(
      grp = dt$pos_group, match_date_rating = dt$match_date_rating,
      cum_num = base_w * vals, cum_den = base_w * tog_vals
    )
    pos_cs <- pos_cs[!is.na(grp)]
    data.table::setorder(pos_cs, grp, match_date_rating)
    pos_cs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]
    pos_look <- .lookup_cumsums(pos_cs, pos_groups)

    # Global cumsums (for fallback grand mean — ratio so scaling cancels)
    glob_cs <- data.table::data.table(
      grp = "ALL", match_date_rating = dt$match_date_rating,
      cum_num = base_w * vals, cum_den = base_w * tog_vals
    )
    data.table::setorder(glob_cs, grp, match_date_rating)
    glob_cs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]
    g_look <- .lookup_cumsums(glob_cs, "ALL")
    g_look[, grand_mean := data.table::fifelse(cum_den > 0, cum_num / cum_den, 0)]

    # Build position mean lookup: (pos_group, ref_idx) -> pos_mean
    pos_look[, pos_mean := data.table::fifelse(cum_den > 0, cum_num / cum_den, NA_real_)]
    # Merge grand_mean as fallback
    pos_look[g_look, grand_mean := i.grand_mean, on = "ref_idx"]
    pos_look[is.na(pos_mean), pos_mean := grand_mean]

    # Merge into per-player lookup: need per-ref_date pos_group
    p_look[pos_meta, pos_group := i.pos_group, on = .(grp = player_id, ref_idx)]
    p_look[pos_look, pos_mean := i.pos_mean, on = .(pos_group = grp, ref_idx)]
    # Fallback for unknown position
    p_look[g_look, grand_mean := i.grand_mean, on = "ref_idx"]
    p_look[is.na(pos_mean), pos_mean := grand_mean]

    # Posterior
    p_look[, alpha0 := pos_mean * prior_str]
    p_look[, `:=`(alpha_post = alpha0 + w_num, beta_post = prior_str + w_den)]

    rating_col <- paste0(stat_nm, "_rating")
    raw_col    <- paste0(stat_nm, "_raw")
    n80_col    <- paste0(stat_nm, "_n80s")
    wt80_col   <- paste0(stat_nm, "_wt80s")

    p_look[, (rating_col) := alpha_post / beta_post]

    # Raw rate (unweighted) — need raw cumsums separately
    raw_cs <- data.table::data.table(
      grp = dt$player_id, match_date_rating = dt$match_date_rating,
      cum_num = vals, cum_den = tog_vals
    )
    data.table::setorder(raw_cs, grp, match_date_rating)
    raw_cs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]
    raw_look <- .lookup_cumsums(raw_cs, all_pids)
    p_look[raw_look, `:=`(.raw_num = i.cum_num, .raw_den = i.cum_den),
           on = .(grp, ref_idx)]
    p_look[, (raw_col) := data.table::fifelse(.raw_den > 0, .raw_num / .raw_den, NA_real_)]
    p_look[, (n80_col) := .raw_den]
    p_look[, (wt80_col) := w_den]

    # CI
    if (compute_ci) {
      lower_col <- paste0(stat_nm, "_rating_lower")
      upper_col <- paste0(stat_nm, "_rating_upper")
      p_look[, (lower_col) := stats::qgamma(ci_alpha, shape = alpha_post, rate = beta_post)]
      p_look[, (upper_col) := stats::qgamma(1 - ci_alpha, shape = alpha_post, rate = beta_post)]
      merge_cols <- c(rating_col, raw_col, n80_col, wt80_col, lower_col, upper_col)
    } else {
      merge_cols <- c(rating_col, raw_col, n80_col, wt80_col)
    }

    # Merge into result via keyed join
    data.table::setkeyv(p_look, c("grp", "ref_idx"))
    for (mc in merge_cols)
      res[p_look, (mc) := get(paste0("i.", mc)), on = .(player_id = grp, ref_idx)]
    cli::cli_progress_update()
  }

  # === Efficiency stats (Beta-Binomial) via cumsum ===
  for (i in seq_len(nrow(eff_defs))) {
    stat_nm       <- eff_defs$stat_name[i]
    success_spec  <- eff_defs$success_col[i]
    attempts_spec <- eff_defs$attempts_col[i]
    if (is.na(success_spec) || is.na(attempts_spec)) next

    use_played_only <- !is.na(eff_defs$played_only[i]) && isTRUE(eff_defs$played_only[i])
    dt_eff <- if (use_played_only) dt[avail_only == FALSE] else dt

    ep <- .resolve_eff_params(stat_nm)
    lam <- ep$lambda
    prior_str <- ep$prior

    # Successes and attempts
    successes <- if (success_spec %in% names(dt_eff)) {
      as.numeric(dt_eff[[success_spec]])
    } else {
      .compute_stat_rating_denominator(dt_eff, success_spec)
    }
    successes[is.na(successes)] <- 0

    attempts <- .compute_stat_rating_denominator(dt_eff, attempts_spec)
    attempts[is.na(attempts)] <- 0
    successes <- pmin(successes, attempts)

    base_w <- exp(lam * dt_eff$.d)

    # Per-player cumsums (weighted successes and weighted attempts)
    pcs <- data.table::data.table(
      grp = dt_eff$player_id, match_date_rating = dt_eff$match_date_rating,
      cum_num = base_w * successes, cum_den = base_w * attempts
    )
    data.table::setorder(pcs, grp, match_date_rating)
    pcs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]
    p_look <- .lookup_cumsums(pcs, all_pids)
    p_look[, ref_d_val := ref_d[ref_idx]]
    p_look[, `:=`(w_num = cum_num * exp(-lam * ref_d_val),
                  w_den = cum_den * exp(-lam * ref_d_val))]

    # Position priors (ratio — scaling cancels)
    use_pos <- is.na(eff_defs$pos_adjusted[i]) || isTRUE(eff_defs$pos_adjusted[i])
    if (use_pos) {
      pos_cs <- data.table::data.table(
        grp = dt_eff$pos_group, match_date_rating = dt_eff$match_date_rating,
        cum_num = base_w * successes, cum_den = base_w * attempts
      )
      pos_cs <- pos_cs[!is.na(grp)]
      data.table::setorder(pos_cs, grp, match_date_rating)
      pos_cs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]
      pos_look <- .lookup_cumsums(pos_cs, pos_groups)
      pos_look[, pos_prop := {
        pp <- data.table::fifelse(cum_den > 0, cum_num / cum_den, NA_real_)
        pmax(pmin(pp, 1 - 1e-6), 1e-6)
      }]
    }

    # Grand proportion
    glob_cs <- data.table::data.table(
      grp = "ALL", match_date_rating = dt_eff$match_date_rating,
      cum_num = base_w * successes, cum_den = base_w * attempts
    )
    data.table::setorder(glob_cs, grp, match_date_rating)
    glob_cs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]
    g_look <- .lookup_cumsums(glob_cs, "ALL")
    g_look[, grand_prop := {
      gp <- data.table::fifelse(cum_den > 0, cum_num / cum_den, 0.5)
      pmax(pmin(gp, 1 - 1e-6), 1e-6)
    }]

    # Merge position/grand priors into player lookup
    p_look[pos_meta, pos_group := i.pos_group, on = .(grp = player_id, ref_idx)]
    if (use_pos) {
      p_look[pos_look, pos_prop := i.pos_prop, on = .(pos_group = grp, ref_idx)]
    }
    p_look[g_look, grand_prop := i.grand_prop, on = "ref_idx"]
    if (use_pos) {
      p_look[is.na(pos_prop), pos_prop := grand_prop]
      p_look[, mu0 := pos_prop]
    } else {
      p_look[, mu0 := grand_prop]
    }
    p_look[is.na(mu0), mu0 := 0.5]

    # Posterior
    p_look[, `:=`(
      alpha_post = mu0 * prior_str + w_num,
      beta_post  = (1 - mu0) * prior_str + w_den - w_num
    )]
    # Guard against invalid Beta params
    p_look[alpha_post <= 0, alpha_post := 1e-4]
    p_look[beta_post  <= 0, beta_post  := 1e-4]

    rating_col <- paste0(stat_nm, "_rating")
    raw_col    <- paste0(stat_nm, "_raw")
    att_col    <- paste0(stat_nm, "_attempts")
    wt_att_col <- paste0(stat_nm, "_wt_attempts")

    p_look[, (rating_col) := alpha_post / (alpha_post + beta_post)]

    # Raw (unweighted) cumsums for raw rate and attempt counts
    raw_cs <- data.table::data.table(
      grp = dt_eff$player_id, match_date_rating = dt_eff$match_date_rating,
      cum_num = successes, cum_den = attempts
    )
    data.table::setorder(raw_cs, grp, match_date_rating)
    raw_cs[, `:=`(cum_num = cumsum(cum_num), cum_den = cumsum(cum_den)), by = grp]
    raw_look <- .lookup_cumsums(raw_cs, all_pids)
    p_look[raw_look, `:=`(.raw_succ = i.cum_num, .raw_att = i.cum_den),
           on = .(grp, ref_idx)]
    p_look[, (raw_col) := data.table::fifelse(.raw_att > 0, .raw_succ / .raw_att, NA_real_)]
    p_look[, (att_col) := .raw_att]
    p_look[, (wt_att_col) := w_den]

    # CI
    if (compute_ci) {
      lower_col <- paste0(stat_nm, "_rating_lower")
      upper_col <- paste0(stat_nm, "_rating_upper")
      p_look[, (lower_col) := stats::qbeta(ci_alpha, alpha_post, beta_post)]
      p_look[, (upper_col) := stats::qbeta(1 - ci_alpha, alpha_post, beta_post)]
      merge_cols <- c(rating_col, raw_col, att_col, wt_att_col, lower_col, upper_col)
    } else {
      merge_cols <- c(rating_col, raw_col, att_col, wt_att_col)
    }

    # Merge into result via keyed join
    data.table::setkeyv(p_look, c("grp", "ref_idx"))
    for (mc in merge_cols)
      res[p_look, (mc) := get(paste0("i.", mc)), on = .(player_id = grp, ref_idx)]
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  stats_elapsed <- (proc.time() - t_stats)[["elapsed"]]

  if (length(skipped_stats) > 0)
    cli::cli_warn("Skipped {length(skipped_stats)} stat(s) due to missing columns: {paste(skipped_stats, collapse = ', ')}")

  # === Filter and split into per-ref_date results ===
  res <- res[wt_games >= params$min_games]
  data.table::setorder(res, ref_idx, -wt_games)
  res[, ref_idx := NULL]

  # Split into named list keyed by ref_date as character (same format as before)
  if (nrow(res) == 0) return(list())
  result_list <- split(res, by = "ref_date", keep.by = TRUE)
  # split() names by ref_date character values — matches original output format
  result_list <- result_list[vapply(result_list, function(x) nrow(x) > 0, logical(1))]

  total_elapsed <- (proc.time() - t_batch_start)[["elapsed"]]
  setup_elapsed <- total_elapsed - stats_elapsed
  cli::cli_inform(paste0(
    "Batch estimation: {length(result_list)} ref_dates, ",
    "{n_total_stats} stats in {round(total_elapsed, 1)}s ",
    "(setup {round(setup_elapsed, 1)}s + stats {round(stats_elapsed, 1)}s)"
  ))
  result_list
}


# Backward compatibility aliases
#' @rdname estimate_player_stat_ratings
#' @export
estimate_player_skills <- estimate_player_stat_ratings

#' @rdname .estimate_stat_ratings_batch
#' @keywords internal
.estimate_skills_batch <- .estimate_stat_ratings_batch
