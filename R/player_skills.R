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
#'
#' @return A data.table with one row per player-match containing:
#'   identifiers (player_id, match_id, player_name, season, round, team),
#'   match_date, tog (time on ground as fraction), position, and all
#'   stat columns referenced by \code{skill_stat_definitions()}.
#'
#' @importFrom data.table as.data.table
#' @export
prepare_skill_data <- function(player_game_data, player_stats) {
  pgd <- data.table::as.data.table(player_game_data)
  ps <- data.table::as.data.table(player_stats)

  # Compute match_date from utc_start_time
  pgd[, match_date_skill := as.Date(utc_start_time)]

  # Compute TOG as fraction
  pgd[, tog := time_on_ground_percentage / 100]

  # Identify player_id and match_id columns in player_stats
  # player_stats uses: player_player_player_player_id (player) + provider_id (match)
  pid_col <- intersect(c("player_id", "player_player_player_player_id"), names(ps))
  mid_col <- intersect(c("match_id", "provider_id"), names(ps))
  if (length(pid_col) == 0 || length(mid_col) == 0) {
    cli::cli_warn("Cannot find player/match ID columns in player_stats; skipping disposal_efficiency join")
    pgd[, disposal_efficiency_pct_x_disposals := 0]
  } else {
    pid_col <- pid_col[1]
    mid_col <- mid_col[1]

    # Bring in disposal_efficiency from player_stats (as percentage 0-100)
    if ("disposal_efficiency" %in% names(ps)) {
      ps_slim <- ps[, c(pid_col, mid_col, "disposal_efficiency"), with = FALSE]
      # Normalise column names to match pgd
      data.table::setnames(ps_slim, c(pid_col, mid_col), c("player_id", "match_id"), skip_absent = TRUE)

      pgd <- merge(pgd, ps_slim, by = c("player_id", "match_id"), all.x = TRUE, suffixes = c("", "_ps"))

      # Handle potential column name conflicts
      de_col <- if ("disposal_efficiency_ps" %in% names(pgd)) "disposal_efficiency_ps" else "disposal_efficiency"

      pgd[, disposal_efficiency_pct_x_disposals :=
        data.table::fifelse(is.na(get(de_col)), 0, as.numeric(get(de_col))) / 100 * disposals]

      # Remove the joined column if it was suffixed
      if ("disposal_efficiency_ps" %in% names(pgd)) {
        pgd[, disposal_efficiency_ps := NULL]
      }
    } else {
      pgd[, disposal_efficiency_pct_x_disposals := 0]
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
#'   date are used. If NULL, uses the max date in the data.
#' @param params Named list of hyperparameters from \code{default_skill_params()}.
#' @param stat_defs Output of \code{skill_stat_definitions()}. If NULL, uses default.
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
                                    params = NULL, stat_defs = NULL) {
  if (is.null(params)) params <- default_skill_params()
  if (is.null(stat_defs)) stat_defs <- skill_stat_definitions()

  dt <- data.table::copy(data.table::as.data.table(skill_data))

  # Ensure date column
  if (!inherits(dt$match_date_skill, "Date")) {
    dt[, match_date_skill := as.Date(match_date_skill)]
  }

  # Determine reference date
  if (is.null(ref_date)) {
    ref_date <- max(dt$match_date_skill, na.rm = TRUE) + 1L
  }
  ref_date <- as.Date(ref_date)

  # Filter to matches before ref_date

  dt <- dt[match_date_skill < ref_date]

  if (nrow(dt) == 0) {
    cli::cli_warn("No match data available before ref_date.")
    return(data.table::data.table())
  }

  # Compute days since and decay weight (using rate lambda for game counting)
  dt[, days_since := as.numeric(ref_date - match_date_skill)]

  # Player metadata
  player_meta <- dt[, .(
    player_name = data.table::last(player_name),
    pos_group = {
      pg <- pos_group[!is.na(pos_group)]
      if (length(pg) == 0) NA_character_
      else { tt <- table(pg); names(tt)[which.max(tt)] }
    }
  ), by = player_id]

  # Weighted games count (using rate lambda)
  dt[, .w_rate := exp(-params$lambda_rate * days_since)]
  game_counts <- dt[, .(
    n_games = data.table::uniqueN(match_id),
    wt_games = sum(.w_rate[!duplicated(match_id)], na.rm = TRUE)
  ), by = player_id]

  # Credible interval quantiles
  ci_alpha <- (1 - params$credible_level) / 2

  # Process each stat
  rate_defs <- stat_defs[stat_defs$type == "rate", ]
  eff_defs <- stat_defs[stat_defs$type == "efficiency", ]

  skill_results <- list()

  # --- Rate stats (Gamma-Poisson) ---
  for (i in seq_len(nrow(rate_defs))) {
    stat_nm <- rate_defs$stat_name[i]
    src_col <- rate_defs$source_col[i]

    if (!src_col %in% names(dt)) next

    vals <- as.numeric(dt[[src_col]])
    vals[is.na(vals)] <- 0
    tog_vals <- as.numeric(dt$tog)
    tog_vals[is.na(tog_vals)] <- 1

    w_vec <- exp(-params$lambda_rate * dt$days_since)

    data.table::set(dt, j = ".wnum", value = w_vec * vals)
    data.table::set(dt, j = ".wden", value = w_vec * tog_vals)

    agg <- dt[, .(w_num = sum(.wnum, na.rm = TRUE),
                   w_den = sum(.wden, na.rm = TRUE)), by = player_id]

    # Position-specific prior: weighted mean within each position group
    agg[player_meta, pos_group := i.pos_group, on = "player_id"]

    # Compute grand mean per game (minutes-weighted)
    total_exposure <- sum(w_vec * tog_vals, na.rm = TRUE)
    grand_mean <- if (total_exposure > 0) sum(w_vec * vals, na.rm = TRUE) / total_exposure else 0

    # Position multipliers
    pos_groups <- c("DEF", "MID", "FWD", "RUCK")
    pos_means <- stats::setNames(rep(grand_mean, 4), pos_groups)
    for (pg in pos_groups) {
      idx <- which(dt$pos_group == pg)
      if (length(idx) > 0) {
        pw <- w_vec[idx] * tog_vals[idx]
        pv <- vals[idx]
        if (sum(pw, na.rm = TRUE) > 0) {
          pos_means[pg] <- sum(w_vec[idx] * pv, na.rm = TRUE) / sum(pw, na.rm = TRUE)
        }
      }
    }

    # Compute posterior
    prior_str <- params$prior_games
    agg[, alpha0 := {
      m <- rep(grand_mean, .N)
      for (pg in pos_groups) {
        m[pos_group == pg] <- pos_means[pg]
      }
      m[is.na(m)] <- grand_mean
      m * prior_str
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
    agg[, (lower_col) := stats::qgamma(ci_alpha, shape = alpha_post, rate = beta_post)]
    agg[, (upper_col) := stats::qgamma(1 - ci_alpha, shape = alpha_post, rate = beta_post)]

    skill_results[[stat_nm]] <- agg[, c("player_id", skill_col, lower_col, upper_col), with = FALSE]
  }

  # --- Efficiency stats (Beta-Binomial) ---
  for (i in seq_len(nrow(eff_defs))) {
    stat_nm <- eff_defs$stat_name[i]
    success_spec <- eff_defs$success_col[i]
    attempts_spec <- eff_defs$attempts_col[i]

    if (is.na(success_spec) || is.na(attempts_spec)) next

    # Get success and attempt vectors
    if (success_spec %in% names(dt)) {
      successes <- as.numeric(dt[[success_spec]])
    } else {
      # For computed columns like disposal_efficiency_pct_x_disposals
      successes <- .compute_skill_denominator(dt, success_spec)
    }
    successes[is.na(successes)] <- 0

    attempts <- .compute_skill_denominator(dt, attempts_spec)
    attempts[is.na(attempts)] <- 0

    # Ensure successes <= attempts
    successes <- pmin(successes, attempts)

    w_vec <- exp(-params$lambda_efficiency * dt$days_since)

    data.table::set(dt, j = ".wnum", value = w_vec * successes)
    data.table::set(dt, j = ".wden", value = w_vec * attempts)

    agg <- dt[, .(w_num = sum(.wnum, na.rm = TRUE),
                   w_den = sum(.wden, na.rm = TRUE)), by = player_id]

    agg[player_meta, pos_group := i.pos_group, on = "player_id"]

    # Grand mean proportion
    total_attempts <- sum(w_vec * attempts, na.rm = TRUE)
    grand_prop <- if (total_attempts > 0) sum(w_vec * successes, na.rm = TRUE) / total_attempts else 0.5
    grand_prop <- max(min(grand_prop, 1 - 1e-6), 1e-6)

    # Position-specific proportions
    pos_props <- stats::setNames(rep(grand_prop, 4), pos_groups)
    for (pg in pos_groups) {
      idx <- which(dt$pos_group == pg)
      if (length(idx) > 0) {
        pa <- sum(w_vec[idx] * attempts[idx], na.rm = TRUE)
        if (pa > 0) {
          pp <- sum(w_vec[idx] * successes[idx], na.rm = TRUE) / pa
          pos_props[pg] <- max(min(pp, 1 - 1e-6), 1e-6)
        }
      }
    }

    prior_str <- params$prior_attempts
    agg[, mu0 := {
      m <- rep(grand_prop, .N)
      for (pg in pos_groups) {
        m[pos_group == pg] <- pos_props[pg]
      }
      m[is.na(m)] <- grand_prop
      pmax(pmin(m, 1 - 1e-6), 1e-6)
    }]

    agg[, `:=`(
      alpha_post = mu0 * prior_str + w_num,
      beta_post = (1 - mu0) * prior_str + w_den - w_num
    )]

    skill_col <- paste0(stat_nm, "_skill")
    lower_col <- paste0(stat_nm, "_lower")
    upper_col <- paste0(stat_nm, "_upper")

    agg[, (skill_col) := alpha_post / (alpha_post + beta_post)]
    agg[, (lower_col) := suppressWarnings(stats::qbeta(ci_alpha, alpha_post, beta_post))]
    agg[, (upper_col) := suppressWarnings(stats::qbeta(1 - ci_alpha, alpha_post, beta_post))]

    skill_results[[stat_nm]] <- agg[, c("player_id", skill_col, lower_col, upper_col), with = FALSE]
  }

  # Clean up temp columns
  tmp_cols <- intersect(c(".wnum", ".wden", ".w_rate"), names(dt))
  for (tc in tmp_cols) data.table::set(dt, j = tc, value = NULL)

  # Assemble result
  result <- data.table::copy(player_meta)
  result[game_counts, `:=`(n_games = i.n_games, wt_games = i.wt_games), on = "player_id"]
  result[, ref_date := ref_date]

  for (stat_nm in names(skill_results)) {
    sr <- skill_results[[stat_nm]]
    result <- merge(result, sr, by = "player_id", all.x = TRUE)
  }

  # Filter to min games
  result <- result[wt_games >= params$min_games]

  data.table::setorder(result, -wt_games)
  result
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

  # Extract target player row
  player_row <- all_skills[player_id == pid]
  player_pos <- player_row$pos_group[1]

  # Compute percentile ranks within position group
  pos_subset <- all_skills[pos_group == player_pos]
  stat_defs <- skill_stat_definitions()
  skill_cols <- paste0(stat_defs$stat_name, "_skill")
  skill_cols <- intersect(skill_cols, names(pos_subset))

  percentiles <- data.frame(
    stat = sub("_skill$", "", skill_cols),
    estimate = as.numeric(player_row[, ..skill_cols]),
    stringsAsFactors = FALSE
  )

  pctile_vals <- vapply(skill_cols, function(col) {
    vals <- pos_subset[[col]]
    player_val <- player_row[[col]]
    if (length(vals) == 0 || is.na(player_val)) return(NA_real_)
    mean(vals <= player_val, na.rm = TRUE) * 100
  }, numeric(1))

  percentiles$percentile <- unname(pctile_vals)

  # Add lower/upper
  lower_cols <- paste0(percentiles$stat, "_lower")
  upper_cols <- paste0(percentiles$stat, "_upper")
  lower_cols <- intersect(lower_cols, names(player_row))
  upper_cols <- intersect(upper_cols, names(player_row))

  if (length(lower_cols) == nrow(percentiles)) {
    percentiles$lower <- as.numeric(player_row[, ..lower_cols])
    percentiles$upper <- as.numeric(player_row[, ..upper_cols])
  }

  # Add category
  percentiles <- merge(
    percentiles,
    stat_defs[, c("stat_name", "category", "type")],
    by.x = "stat", by.y = "stat_name", all.x = TRUE
  )

  out <- list(
    player_info = data.frame(
      player_id = pid,
      name = player$player_name,
      team = player$team,
      position = player$position,
      pos_group = player_pos,
      stringsAsFactors = FALSE
    ),
    skills = percentiles[order(-percentiles$percentile), ],
    ref_date = ref_date
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
  cat(paste0(
    "=== Skill Profile: ", info$name,
    " (", info$team, " - ", info$pos_group, ") ===\n",
    "As at: ", x$ref_date, "\n\n"
  ))
  print(x$skills, row.names = FALSE)
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
  skills_dt <- data.table::as.data.table(skills)
  lineups_dt <- data.table::as.data.table(team_lineups)

  # Ensure player_id columns match type
  if (is.character(skills_dt$player_id) && is.numeric(lineups_dt$player_id)) {
    lineups_dt[, player_id := as.character(player_id)]
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
  # Use wt_games as tie-breaker for ordering
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
