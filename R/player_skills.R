# Player Skill Estimation
# ========================
# Bayesian conjugate prior estimation of per-stat player skills,
# with exponential time decay and position-specific priors.
#
# Rate stats use Gamma-Poisson conjugate model (counts per game).
# Efficiency stats use Beta-Binomial conjugate model (proportions).


# ============================================================================
# Position helpers
# ============================================================================

#' Map AFL positions to simplified position groups
#'
#' @param pos Character vector of AFL position strings.
#' @return Character vector of position groups (DEF/MID/FWD/RUCK), NA for unknown.
#' @keywords internal
.map_position_group <- function(pos) {
  pm <- skill_position_map()
  # Build reverse lookup: position -> group
  lookup <- character(0)
  for (grp in names(pm)) {
    for (p in pm[[grp]]) {
      lookup[p] <- grp
    }
  }
  unname(lookup[pos])
}


#' Resolve position groups, filling NAs with each player's modal position
#'
#' @param dt A data.table with player_id and position columns.
#' @return The data.table with added `pos_group` column.
#' @keywords internal
.resolve_skill_positions <- function(dt) {
  # Use 'pos' column (listed position: KEY_DEFENDER, MIDFIELDER, etc.)
  # Fallback to 'position' if 'pos' not available
  pos_col <- if ("pos" %in% names(dt)) "pos" else "position"
  dt[, pos_group := .map_position_group(get(pos_col))]

  na_idx <- which(is.na(dt$pos_group))
  if (length(na_idx) > 0) {
    # Compute modal position per player from non-NA rows
    valid <- dt[!is.na(pos_group)]
    if (nrow(valid) > 0) {
      modal_pos <- valid[, {
        tt <- table(pos_group)
        list(modal_pos = if (length(tt) > 0) names(tt)[which.max(tt)] else NA_character_)
      }, by = player_id]

      dt[modal_pos, on = "player_id", modal_pos := i.modal_pos]
      dt[is.na(pos_group) & !is.na(modal_pos), pos_group := modal_pos]
      dt[, modal_pos := NULL]
    }
  }

  dt
}


# ============================================================================
# Data preparation
# ============================================================================

#' Prepare data for skill estimation
#'
#' Joins player game data with player stats to produce a single table with
#' all required columns for the skill estimation pipeline.
#'
#' @param player_game_data Player game data from \code{load_player_game_data(TRUE)}.
#' @param player_stats Player stats from \code{load_player_stats(TRUE)}.
#' @param rosters Optional roster data. If NULL, loads from torpdata.
#' @param fixtures Optional fixture data. If NULL, loads from torpdata.
#'
#' @return A data.table with one row per player-match containing:
#'   identifiers (player_id, match_id, player_name, season, round, team),
#'   match_date_skill (Date), tog (time on ground as fraction), position,
#'   and all stat columns referenced by \code{skill_stat_definitions()}.
#'
#' @importFrom data.table as.data.table
#' @export
prepare_skill_data <- function(player_game_data, player_stats, rosters = NULL,
                               fixtures = NULL) {
  pgd <- data.table::as.data.table(player_game_data)
  ps <- data.table::as.data.table(player_stats)

  # Compute match_date from utc_start_time
  pgd[, match_date_skill := as.Date(utc_start_time)]

  # Compute TOG as fraction and constant denominator for Beta-Binomial model
  pgd[, tog := time_on_ground_percentage / 100]
  pgd[, tog_denominator := 1]
  pgd[, played := 1L]

  # Identify player_id and match_id columns in player_stats
  # player_stats uses: player_player_player_player_id (player) + provider_id (match)
  pid_col <- intersect(c("player_id", "player_player_player_player_id"), names(ps))
  mid_col <- intersect(c("match_id", "provider_id"), names(ps))
  if (length(pid_col) == 0 || length(mid_col) == 0) {
    cli::cli_warn(c(
      "Cannot find player/match ID columns in player_stats.",
      "i" = "Some skills will rely entirely on the prior."
    ))
    pgd[, disposal_efficiency_pct_x_disposals := NA_real_]
  } else {
    pid_col <- pid_col[1]
    mid_col <- mid_col[1]

    # Find all stat columns needed by skill definitions but missing from pgd
    stat_defs_merge <- skill_stat_definitions()
    needed_cols <- unique(c(
      stats::na.omit(stat_defs_merge$source_col),
      stats::na.omit(stat_defs_merge$success_col),
      stats::na.omit(stat_defs_merge$attempts_col)
    ))
    needed_cols <- unique(unlist(strsplit(needed_cols, "\\+")))
    missing_from_pgd <- setdiff(needed_cols, names(pgd))
    # Also always bring in disposal_efficiency for the derived column
    missing_from_pgd <- unique(c(missing_from_pgd, "disposal_efficiency"))
    available_in_ps <- intersect(missing_from_pgd, names(ps))

    if (length(available_in_ps) > 0) {
      ps_slim <- ps[, c(pid_col, mid_col, available_in_ps), with = FALSE]
      data.table::setnames(ps_slim, c(pid_col, mid_col), c("player_id", "match_id"), skip_absent = TRUE)
      ps_slim <- unique(ps_slim, by = c("player_id", "match_id"))

      n_before <- nrow(pgd)
      pgd <- merge(pgd, ps_slim, by = c("player_id", "match_id"), all.x = TRUE, suffixes = c("", "_ps"))
      if (nrow(pgd) != n_before) {
        cli::cli_warn("Merge with player_stats changed row count from {n_before} to {nrow(pgd)}")
      }

      # Handle suffixed columns from name conflicts
      for (col in available_in_ps) {
        ps_col <- paste0(col, "_ps")
        if (ps_col %in% names(pgd)) {
          if (!col %in% names(pgd)) {
            data.table::setnames(pgd, ps_col, col)
          } else {
            pgd[, (ps_col) := NULL]
          }
        }
      }
    }

    # Compute derived disposal_efficiency column for Beta-Binomial model
    if ("disposal_efficiency" %in% names(pgd)) {
      pgd[, disposal_efficiency_pct_x_disposals :=
        data.table::fifelse(is.na(disposal_efficiency), 0, as.numeric(disposal_efficiency)) / 100 * disposals]
    } else {
      cli::cli_warn("disposal_efficiency column not found; skill will rely on prior")
      pgd[, disposal_efficiency_pct_x_disposals := NA_real_]
    }
  }

  # Map positions to groups
  pgd <- .resolve_skill_positions(pgd)

  # Construct player_name if not present
  if (!"player_name" %in% names(pgd) && "plyr_nm" %in% names(pgd)) {
    pgd[, player_name := plyr_nm]
  }

  # Select essential columns + all stat source columns
  stat_defs <- skill_stat_definitions()
  rate_cols <- stats::na.omit(unique(stat_defs$source_col))
  # For efficiency stats, parse columns from success_col and attempts_col
  eff_cols <- unique(c(
    stats::na.omit(stat_defs$success_col),
    stats::na.omit(stat_defs$attempts_col)
  ))
  # Handle "col1+col2" specs
  eff_cols <- unlist(strsplit(eff_cols, "\\+"))

  all_stat_cols <- unique(c(rate_cols, eff_cols))
  keep_cols <- unique(c(
    "player_id", "match_id", "player_name", "season", "round",
    "match_date_skill", "tog", "pos_group", "position",
    intersect(all_stat_cols, names(pgd)),
    "disposal_efficiency_pct_x_disposals"
  ))

  # Only keep columns that exist
  keep_cols <- intersect(keep_cols, names(pgd))
  out <- pgd[, ..keep_cols]

  # Add team column if available
  if ("tm" %in% names(pgd) && !"team" %in% keep_cols) {
    out[, team := pgd$tm]
  }

  # Expand with zero-TOG rows for rostered players who didn't play.
  # This lets the Beta-Binomial TOG model account for selection probability:
  # missed rounds contribute (tog=0, tog_denominator=1) to the denominator,
  # pulling estimates down for players who miss games.
  # Other stats are unaffected: rate stats use tog as exposure (0 * w = 0),
  # efficiency stats have 0 successes and 0 attempts (0 * w = 0).
  out[, avail_only := FALSE]

  round_cal <- out[, .(match_date_skill = min(match_date_skill)),
                   by = .(season, round)]

  if (!is.null(rosters)) {
    # Roster-based expansion: every rostered player × every round their team played
    roster_dt <- data.table::as.data.table(rosters)
    if ("providerId" %in% names(roster_dt)) {
      data.table::setnames(roster_dt, "providerId", "player_id", skip_absent = TRUE)
    }

    if (!is.null(fixtures)) {
      # Build team-round calendar from fixtures (handles byes + finals correctly)
      fix_dt <- data.table::as.data.table(fixtures)
      home <- fix_dt[, .(season = compSeason.year, round = round.roundNumber,
                         team = home.team.name)]
      away <- fix_dt[, .(season = compSeason.year, round = round.roundNumber,
                         team = away.team.name)]
      team_rounds <- unique(data.table::rbindlist(list(home, away)))
      team_rounds <- merge(team_rounds, round_cal, by = c("season", "round"))

      # Join roster players (with team) to team-round calendar
      roster_players <- unique(roster_dt[, .(player_id, season, team)])
      all_combos <- merge(roster_players, team_rounds,
                          by = c("season", "team"), allow.cartesian = TRUE)
      all_combos[, team := NULL]
    } else {
      # Fallback without fixtures: every round in their season (old behavior)
      roster_players <- unique(roster_dt[, .(player_id, season)])
      all_combos <- merge(roster_players, round_cal, by = "season",
                          allow.cartesian = TRUE)
    }
  } else {
    # Fallback: expand from each player's first game to the latest round
    player_first <- out[, .(first_season = min(season),
                            first_round = min(round[season == min(season)])),
                        by = player_id]
    all_combos <- data.table::CJ(player_id = player_first$player_id,
                                  round_idx = seq_len(nrow(round_cal)))
    all_combos[, c("season", "round", "match_date_skill") :=
                 round_cal[round_idx, .(season, round, match_date_skill)]]
    all_combos[, round_idx := NULL]
    all_combos <- merge(all_combos, player_first, by = "player_id")
    all_combos <- all_combos[season > first_season |
                             (season == first_season & round >= first_round)]
    all_combos[, c("first_season", "first_round") := NULL]
  }

  played <- unique(out[, .(player_id, season, round)])
  played[, .played := TRUE]
  all_combos <- merge(all_combos, played,
                      by = c("player_id", "season", "round"), all.x = TRUE)
  missed <- all_combos[is.na(.played)]

  if (nrow(missed) > 0) {
    zero_rows <- missed[, .(
      player_id, season, round, match_date_skill,
      match_id = paste0("AVAIL_", season, "_", sprintf("%02d", round)),
      tog = 0, tog_denominator = 1, played = 0L, avail_only = TRUE
    )]

    # Assign pos_group: use modal position from games if available,
    # otherwise use roster position for never-played players
    player_pos <- out[avail_only == FALSE & !is.na(pos_group),
                      .(pos_group = names(which.max(table(pos_group)))), by = player_id]
    if (!is.null(rosters)) {
      roster_pos <- unique(roster_dt[, .(player_id, position)])
      roster_pos[, roster_pos_group := .map_position_group(position)]
      roster_pos <- roster_pos[!is.na(roster_pos_group), .(player_id, roster_pos_group)]
      roster_pos <- unique(roster_pos, by = "player_id")
      # Merge: prefer game-based pos, fallback to roster pos
      player_pos <- merge(roster_pos, player_pos, by = "player_id", all.x = TRUE)
      player_pos[is.na(pos_group), pos_group := roster_pos_group]
      player_pos[, roster_pos_group := NULL]
    }
    zero_rows <- merge(zero_rows, player_pos, by = "player_id", all.x = TRUE)

    out <- data.table::rbindlist(list(out, zero_rows), fill = TRUE)
  }

  out
}


# ============================================================================
# Denominator helper for efficiency stats
# ============================================================================

#' Compute denominator vector from a column spec string
#'
#' Supports "col1+col2" for summing multiple columns.
#'
#' @param dt A data.table.
#' @param denom_spec A string like "col" or "col1+col2".
#' @return Numeric vector of denominators.
#' @keywords internal
.compute_skill_denominator <- function(dt, denom_spec) {
  if (grepl("\\+", denom_spec)) {
    parts <- strsplit(denom_spec, "\\+")[[1]]
    result <- rep(0, nrow(dt))
    for (p in parts) {
      if (p %in% names(dt)) {
        v <- as.numeric(dt[[p]])
        v[is.na(v)] <- 0
        result <- result + v
      }
    }
    return(result)
  }
  if (denom_spec %in% names(dt)) {
    v <- as.numeric(dt[[denom_spec]])
    v[is.na(v)] <- 0
    return(v)
  }
  rep(0, nrow(dt))
}


# ============================================================================
# Core skill estimation
# ============================================================================

#' Estimate player skills using Bayesian conjugate priors
#'
#' For each player, estimates "true skill" at a reference date using all prior
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
#' @param skill_data A data.table from \code{prepare_skill_data()}.
#' @param ref_date Date to estimate skills as of. Only matches before this
#'   date are used. If NULL, includes all available matches (sets ref_date
#'   to one day after the latest match in the data).
#' @param params Named list of hyperparameters from \code{default_skill_params()}.
#' @param stat_defs Output of \code{skill_stat_definitions()}. If NULL, uses default.
#' @param compute_ci Logical. If TRUE (default), compute credible intervals
#'   (\code{_lower}/\code{_upper} columns) using qgamma/qbeta. Set to FALSE
#'   to skip interval computation for faster batch processing.
#'
#' @return A data.table with one row per player containing:
#'   \code{player_id}, \code{player_name}, \code{pos_group},
#'   \code{n_games}, \code{wt_games}, \code{ref_date},
#'   and for each stat: \code{{stat}_skill}, \code{{stat}_lower}, \code{{stat}_upper}.
#'
#' @importFrom data.table as.data.table copy
#' @importFrom stats qgamma qbeta
#' @export
estimate_player_skills <- function(skill_data, ref_date = NULL,
                                    params = NULL, stat_defs = NULL,
                                    compute_ci = TRUE) {
  if (is.null(params)) params <- default_skill_params()
  if (is.null(stat_defs)) stat_defs <- skill_stat_definitions()

  dt <- data.table::as.data.table(skill_data)

  # Ensure date column
  if (!inherits(dt$match_date_skill, "Date")) {
    dt[, match_date_skill := as.Date(match_date_skill)]
  }

  # Determine reference date
  if (is.null(ref_date)) {
    ref_date <- max(dt$match_date_skill, na.rm = TRUE) + 1L
  }
  ref_date <- as.Date(ref_date)

  # Filter to matches before ref_date (creates a copy — no prior copy needed)
  dt <- dt[match_date_skill < ref_date]

  if (nrow(dt) == 0) {
    cli::cli_warn("No match data available before ref_date.")
    return(data.table::data.table())
  }

  # Compute days since and decay weight (using rate lambda for game counting)
  dt[, days_since := as.numeric(ref_date - match_date_skill)]

  # Ensure avail_only flag exists (rows from prepare_skill_data zero-TOG expansion)
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

    # Per-stat lambda and prior strength
    rp <- .resolve_rate_params(stat_nm, stat_cat)
    stat_lambda <- rp$lambda
    prior_str <- rp$prior

    vals <- as.numeric(dt[[src_col]])
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

    # Position-specific prior means (vectorized lookup instead of per-group loop)
    pos_groups <- names(skill_position_map())
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
    skill_col <- paste0(stat_nm, "_skill")
    lower_col <- paste0(stat_nm, "_lower")
    upper_col <- paste0(stat_nm, "_upper")

    agg[, (skill_col) := alpha_post / beta_post]
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

    keep_cols <- c("player_id", skill_col, raw_col, n80_col, wt80_col)
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
      successes <- .compute_skill_denominator(dt_eff, success_spec)
    }
    successes[is.na(successes)] <- 0

    attempts <- .compute_skill_denominator(dt_eff, attempts_spec)
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

    skill_col <- paste0(stat_nm, "_skill")
    lower_col <- paste0(stat_nm, "_lower")
    upper_col <- paste0(stat_nm, "_upper")

    agg[, (skill_col) := alpha_post / (alpha_post + beta_post)]
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

    keep_cols <- c("player_id", skill_col, raw_col, att_col, wt_att_col)
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

  data.table::setorder(result, -wt_games)
  result
}


# ============================================================================
# Batch skill estimation
# ============================================================================

#' Estimate player skills for multiple reference dates efficiently
#'
#' Internal function that eliminates redundant work when estimating skills
#' across many dates: date conversion and avail_only setup are done once,
#' and the data is sorted so each iteration filters an ascending subset.
#'
#' @param skill_data A data.table from \code{prepare_skill_data()}.
#' @param ref_dates Date vector of reference dates.
#' @param params Named list of hyperparameters from \code{default_skill_params()}.
#' @param stat_defs Output of \code{skill_stat_definitions()}. If NULL, uses default.
#' @param compute_ci Logical. If FALSE (default), skip credible interval computation.
#'
#' @return Named list of data.tables (keyed by ref_date as character), one per date.
#' @keywords internal
.estimate_skills_batch <- function(skill_data, ref_dates, params = NULL,
                                   stat_defs = NULL, compute_ci = FALSE) {
  if (is.null(params)) params <- default_skill_params()
  if (is.null(stat_defs)) stat_defs <- skill_stat_definitions()

  # One-time setup: convert to data.table, ensure types, sort by date
  dt_base <- data.table::as.data.table(skill_data)
  if (!inherits(dt_base$match_date_skill, "Date")) {
    dt_base[, match_date_skill := as.Date(match_date_skill)]
  }
  if (!"avail_only" %in% names(dt_base)) dt_base[, avail_only := FALSE]
  dt_base[is.na(avail_only), avail_only := FALSE]
  data.table::setorder(dt_base, match_date_skill)

  # Process ref_dates in chronological order
  ref_dates_sorted <- sort(unique(as.Date(ref_dates)))
  results <- vector("list", length(ref_dates_sorted))

  for (i in seq_along(ref_dates_sorted)) {
    rd <- ref_dates_sorted[i]
    # Subset via filter on sorted data — creates a copy (no prior copy needed)
    dt_sub <- dt_base[match_date_skill < rd]
    if (nrow(dt_sub) == 0) next
    results[[i]] <- tryCatch(
      estimate_player_skills(dt_sub, ref_date = rd, params = params,
                             stat_defs = stat_defs, compute_ci = compute_ci),
      error = function(e) {
        cli::cli_warn("Batch estimation failed for {rd}: {conditionMessage(e)}")
        NULL
      }
    )
  }

  names(results) <- as.character(ref_dates_sorted)
  results[!vapply(results, is.null, logical(1))]
}


# ============================================================================
# Profile helpers
# ============================================================================

#' Extract column values from a single-row data.table, defaulting to NA
#' @keywords internal
.extract_player_cols <- function(player_row, col_names) {
  vapply(col_names, function(col) {
    if (col %in% names(player_row)) as.numeric(player_row[[col]]) else NA_real_
  }, numeric(1))
}

#' Compute column means across a data.table
#' @keywords internal
.col_means <- function(dt, cols) {
  if (nrow(dt) == 0) return(rep(NA_real_, length(cols)))
  vapply(cols, function(col) mean(dt[[col]], na.rm = TRUE), numeric(1))
}

#' Compute percentile rank of a player's values within a reference data.table
#' @keywords internal
.col_pctiles <- function(dt, cols, player_row) {
  if (nrow(dt) == 0) return(rep(NA_real_, length(cols)))
  vapply(cols, function(col) {
    player_val <- player_row[[col]]
    if (is.na(player_val)) return(NA_real_)
    mean(dt[[col]] <= player_val, na.rm = TRUE) * 100
  }, numeric(1))
}


# ============================================================================
# Player skill profile
# ============================================================================

#' Get a player's skill profile with percentile ranks
#'
#' Resolves a player by name (partial match OK), estimates skills for all
#' players, then returns the target player's row with within-position
#' percentile ranks appended.
#'
#' @param player_name A character string of the player's name (partial OK).
#' @param ref_date Date to estimate skills as of. Default is today.
#' @param seasons Seasons to include. Numeric vector or TRUE for all.
#' @param params Hyperparameters from \code{default_skill_params()}.
#' @param skills Optional pre-computed skills data (e.g. from
#'   \code{load_player_skills(TRUE)}). If provided, skips the expensive
#'   data loading and estimation steps. If NULL (default), computes from scratch.
#'
#' @return A list of class \code{torp_skill_profile} with elements:
#'   \describe{
#'     \item{player_info}{Player ID, name, team, position.}
#'     \item{skills}{Data.frame of skill estimates with percentile ranks.}
#'     \item{ref_date}{Reference date used.}
#'   }
#'
#' @export
player_skill_profile <- function(player_name, ref_date = Sys.Date(),
                                  seasons = TRUE, params = NULL,
                                  skills = NULL) {
  player <- resolve_player(player_name, seasons = seasons)
  pid <- player$player_id

  if (!is.null(skills)) {
    # Fast path: use pre-computed skills
    all_skills <- data.table::as.data.table(skills)
    # If multiple snapshots per player, keep latest at or before ref_date
    if ("ref_date" %in% names(all_skills)) {
      all_skills <- all_skills[all_skills$ref_date <= ref_date]
      all_skills <- all_skills[all_skills[, .I[which.max(ref_date)], by = player_id]$V1]
    }
  } else {
    # Slow path: compute from scratch
    pgd <- load_player_game_data(seasons, use_disk_cache = TRUE)
    ps <- load_player_stats(seasons, use_disk_cache = TRUE)
    skill_data <- prepare_skill_data(pgd, ps)
    all_skills <- estimate_player_skills(skill_data, ref_date = ref_date, params = params)
  }

  if (nrow(all_skills) == 0 || !pid %in% all_skills$player_id) {
    cli::cli_abort("Player {.val {player_name}} not found in skill estimates (may have fewer than {SKILL_MIN_GAMES} weighted games)")
  }

  # Extract target player row (ensure single row)
  player_row <- all_skills[player_id == pid]
  if (nrow(player_row) > 1) {
    cli::cli_warn("Multiple skill rows found for player {.val {player_name}}, using latest")
    player_row <- player_row[which.max(ref_date)]
  }
  player_pos <- player_row$pos_group[1]

  # Subsets for percentile computation
  if (is.na(player_pos)) {
    cli::cli_warn("Player {.val {player_name}} has no position group; position-based comparisons will be NA")
    pos_subset <- all_skills[0]
  } else {
    pos_subset <- all_skills[pos_group == player_pos]
  }
  stat_defs <- skill_stat_definitions()
  skill_cols <- paste0(stat_defs$stat_name, "_skill")
  skill_cols <- intersect(skill_cols, names(all_skills))
  stat_names <- sub("_skill$", "", skill_cols)

  # Build profile data.frame
  profile <- data.frame(stat = stat_names, stringsAsFactors = FALSE)
  profile$skill <- as.numeric(player_row[, ..skill_cols])

  # Raw average (unsmoothed career average, NA if column missing)
  raw_cols <- paste0(stat_names, "_raw")
  if (!any(raw_cols %in% names(player_row))) {
    cli::cli_inform(c("i" = "Pre-computed skills data is missing raw average columns.",
                       "i" = "Re-run {.fn estimate_player_skills} for raw averages."))
  }
  profile$raw_avg <- .extract_player_cols(player_row, raw_cols)

  # League-wide and position-group comparisons
  profile$league_avg <- .col_means(all_skills, skill_cols)
  profile$league_pct <- .col_pctiles(all_skills, skill_cols, player_row)
  profile$pos_avg    <- .col_means(pos_subset, skill_cols)
  profile$pos_pct    <- .col_pctiles(pos_subset, skill_cols, player_row)

  # Exposure: per-stat 80s for rate stats, attempts for efficiency stats
  profile$n_80s  <- .extract_player_cols(player_row, paste0(stat_names, "_n80s"))
  profile$wt_80s <- .extract_player_cols(player_row, paste0(stat_names, "_wt80s"))
  profile$attempts    <- .extract_player_cols(player_row, paste0(stat_names, "_attempts"))
  profile$wt_attempts <- .extract_player_cols(player_row, paste0(stat_names, "_wt_attempts"))

  # Credible intervals
  lower_cols <- paste0(stat_names, "_lower")
  upper_cols <- paste0(stat_names, "_upper")
  lower_present <- intersect(lower_cols, names(player_row))
  upper_present <- intersect(upper_cols, names(player_row))
  if (length(lower_present) == nrow(profile) && length(upper_present) == nrow(profile)) {
    profile$lower <- as.numeric(player_row[, ..lower_present])
    profile$upper <- as.numeric(player_row[, ..upper_present])
  }

  # Add category/type from stat definitions
  profile <- merge(
    profile,
    stat_defs[, c("stat_name", "category", "type")],
    by.x = "stat", by.y = "stat_name", all.x = TRUE
  )

  # Reorder columns
  col_order <- c("category", "stat", "type", "skill", "raw_avg",
                  "league_avg", "league_pct", "pos_avg", "pos_pct",
                  "n_80s", "wt_80s", "attempts", "wt_attempts",
                  "lower", "upper")
  col_order <- intersect(col_order, names(profile))
  profile <- profile[, col_order]

  out <- list(
    player_info = data.frame(
      player_id = pid,
      name = player$player_name,
      team = player$team,
      position = player$position,
      pos_group = player_pos,
      stringsAsFactors = FALSE
    ),
    skills = profile[order(-profile$pos_pct), ],
    ref_date = ref_date,
    n_games = as.numeric(player_row$n_games),
    n_80s = as.numeric(player_row$n_80s),
    wt_80s = as.numeric(player_row$wt_80s)
  )
  class(out) <- "torp_skill_profile"
  out
}


#' Print a player skill profile
#'
#' @param x A \code{torp_skill_profile} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @export
print.torp_skill_profile <- function(x, ...) {
  info <- x$player_info
  sk <- x$skills

  # Header: games and 80s
  n_g <- if (!is.null(x$n_games)) x$n_games else NA
  n_80 <- if (!is.null(x$n_80s)) round(x$n_80s, 1) else NA
  wt_80 <- if (!is.null(x$wt_80s)) round(x$wt_80s, 1) else NA
  cat(paste0(
    "=== Skill Profile: ", info$name,
    " (", info$team, " - ", info$pos_group, ") ===\n",
    "As at: ", x$ref_date,
    "  |  Games: ", n_g,
    "  |  80s: ", n_80, " (wt: ", wt_80, ")\n\n"
  ))

  # Select display columns (exclude lower/upper)
  display_cols <- intersect(
    c("category", "stat", "type", "skill", "raw_avg",
      "league_avg", "league_pct", "pos_avg", "pos_pct",
      "n_80s", "wt_80s", "attempts", "wt_attempts"),
    names(sk)
  )
  display <- sk[, display_cols]

  # Format: 4 sig figs for estimates, 1 dp for percentiles and exposure
  fmt_sig4 <- function(v) {
    vapply(v, function(x) {
      if (is.na(x)) return("")
      format(signif(x, 4), scientific = FALSE, drop0trailing = TRUE, trim = TRUE)
    }, character(1), USE.NAMES = FALSE)
  }
  fmt_1dp <- function(v) {
    ifelse(is.na(v), "", formatC(round(v, 1), format = "f", digits = 1))
  }
  for (col in intersect(c("skill", "raw_avg", "league_avg", "pos_avg"), names(display))) {
    display[[col]] <- fmt_sig4(display[[col]])
  }
  for (col in intersect(c("league_pct", "pos_pct"), names(display))) {
    display[[col]] <- fmt_1dp(display[[col]])
  }
  for (col in intersect(c("n_80s", "wt_80s", "attempts", "wt_attempts"), names(display))) {
    display[[col]] <- fmt_1dp(display[[col]])
  }

  print(display, row.names = FALSE)
  invisible(x)
}


# ============================================================================
# Quick player skill lookup
# ============================================================================

#' Get player skill estimates from pre-computed data
#'
#' A fast convenience function that looks up skill estimates from the
#' pre-computed data stored in torpdata releases. When called with a player
#' name, returns one row for that player. When called without arguments,
#' returns the latest snapshot for every player.
#'
#' @param player_name A character string of the player's name (partial OK).
#'   If NULL (default), returns all players.
#' @param ref_date Optional date to filter to the latest snapshot at or before
#'   this date. If NULL, uses the latest available snapshot.
#' @param seasons Seasons to include. Numeric vector or TRUE for all.
#' @param current If TRUE (default), only return players on a current team
#'   (i.e. those with a skill estimate in the latest season). Set FALSE to
#'   include all historical players.
#'
#' @return A data.table of skill estimates — one row per player.
#'
#' @seealso [player_skill_profile()] for full profile with percentile ranks,
#'   [load_player_skills()] to load raw pre-computed data.
#'
#' @export
get_player_skills <- function(player_name = NULL, ref_date = NULL, seasons = TRUE,
                              current = TRUE) {
  skills <- load_player_skills(seasons, use_disk_cache = TRUE)
  dt <- data.table::as.data.table(skills)

  if (!is.null(ref_date)) {
    ref_date <- as.Date(ref_date)
    dt <- dt[dt$ref_date <= ref_date]
  }

  if (!is.null(player_name)) {
    player <- resolve_player(player_name, seasons = seasons)
    dt <- dt[player_id == player$player_id]
    if (nrow(dt) == 0) {
      cli::cli_abort("No skills found for {.val {player_name}}")
    }
  }

  if (nrow(dt) == 0) {
    cli::cli_warn("No skills found for the specified filters")
    return(dt)
  }

  # Keep latest snapshot per player
  dt <- dt[dt[, .I[which.max(ref_date)], by = player_id]$V1]

  # Filter to current players (those in the latest season)
  if (current && is.null(player_name)) {
    max_season <- max(dt$season, na.rm = TRUE)
    dt <- dt[season == max_season]
  }

  # Drop lower/upper interval columns
  drop_cols <- grep("_(lower|upper)$", names(dt), value = TRUE)
  if (length(drop_cols) > 0) dt[, (drop_cols) := NULL]

  dt
}


# ============================================================================
# Team skill aggregation
# ============================================================================

#' Aggregate player skills to team level
#'
#' For each team in a lineup, sums and averages the skill estimates of the
#' players in that team. Used to create team-level features for match
#' prediction models.
#'
#' @param skills A data.table from \code{estimate_player_skills()}.
#' @param team_lineups A data.table with columns \code{match_id},
#'   \code{team}, and \code{player_id} identifying the lineup for each
#'   match-team combination.
#' @param top_n Maximum number of players per team to include. Default 22.
#'
#' @return A data.table with one row per match-team, containing
#'   \code{match_id}, \code{team}, and for each stat:
#'   \code{{stat}_team_sum} and \code{{stat}_team_mean}.
#'
#' @importFrom data.table as.data.table
#' @export
aggregate_team_skills <- function(skills, team_lineups, top_n = 22) {
  skills_dt <- data.table::copy(data.table::as.data.table(skills))
  lineups_dt <- data.table::copy(data.table::as.data.table(team_lineups))

  # Ensure player_id columns match type
  if (is.character(skills_dt$player_id) && is.numeric(lineups_dt$player_id)) {
    lineups_dt[, player_id := as.character(player_id)]
  }

  # Drop columns from skills that would conflict with lineup columns
  conflict_cols <- intersect(names(skills_dt), names(lineups_dt))
  conflict_cols <- setdiff(conflict_cols, "player_id")
  if (length(conflict_cols) > 0) {
    skills_dt[, (conflict_cols) := NULL]
  }

  # Join skills to lineups
  merged <- merge(lineups_dt, skills_dt, by = "player_id", all.x = TRUE)

  # Find skill columns
  stat_defs <- skill_stat_definitions()
  skill_cols <- paste0(stat_defs$stat_name, "_skill")
  skill_cols <- intersect(skill_cols, names(merged))

  if (length(skill_cols) == 0) {
    cli::cli_warn("No skill columns found in merged data")
    return(data.table::data.table())
  }

  # For each match-team, take top_n players by total skill and aggregate
  merged[, .total_skill := rowSums(.SD, na.rm = TRUE), .SDcols = skill_cols]

  result <- merged[order(-`.total_skill`),
    head(.SD, top_n),
    by = .(match_id, team)
  ][, {
    out <- list(n_players = .N)
    for (sc in skill_cols) {
      stat_nm <- sub("_skill$", "", sc)
      vals <- get(sc)
      out[[paste0(stat_nm, "_team_sum")]] <- sum(vals, na.rm = TRUE)
      out[[paste0(stat_nm, "_team_mean")]] <- mean(vals, na.rm = TRUE)
    }
    out
  }, by = .(match_id, team)]

  # Clean up
  merged[, .total_skill := NULL]

  result
}
