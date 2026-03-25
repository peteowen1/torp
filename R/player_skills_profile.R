# Player Stat Rating Profiles & Aggregation
# ==========================================
# User-facing profile views, lookup convenience functions,
# and team-level aggregation of player stat ratings.

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
#'
#' @param dt Reference data.table for comparison.
#' @param cols Column names to compute percentiles for.
#' @param player_row Single-row data.table of the target player.
#' @param higher_is_better Optional logical vector (same length as cols).
#'   If FALSE for a stat, percentile is flipped (lower = better).
#' @keywords internal
.col_pctiles <- function(dt, cols, player_row, higher_is_better = NULL) {
  if (nrow(dt) == 0) return(rep(NA_real_, length(cols)))
  pcts <- vapply(cols, function(col) {
    player_val <- player_row[[col]]
    if (is.na(player_val)) return(NA_real_)
    mean(dt[[col]] <= player_val, na.rm = TRUE) * 100
  }, numeric(1))
  # Flip for negative stats
  if (!is.null(higher_is_better) && length(higher_is_better) == length(pcts)) {
    flip <- !is.na(higher_is_better) & !higher_is_better
    pcts[flip] <- 100 - pcts[flip]
  }
  pcts
}


# ============================================================================
# Player stat rating profile
# ============================================================================

#' Get a player's stat rating profile with percentile ranks
#'
#' Resolves a player by name (partial match OK), estimates stat ratings for all
#' players, then returns the target player's row with within-position
#' percentile ranks appended.
#'
#' @param player_name A character string of the player's name (partial OK).
#' @param ref_date Date to estimate stat ratings as of. Default is today.
#' @param seasons Seasons to include. Numeric vector or TRUE for all.
#' @param params Hyperparameters from \code{default_stat_rating_params()}.
#' @param skills Optional pre-computed stat ratings data (e.g. from
#'   \code{load_player_stat_ratings(TRUE)}). If provided, skips the expensive
#'   data loading and estimation steps. If NULL (default), computes from scratch.
#'
#' @return A list of class \code{torp_stat_rating_profile} with elements:
#'   \describe{
#'     \item{player_info}{Player ID, name, team, position.}
#'     \item{skills}{Data.frame of stat rating estimates with percentile ranks.}
#'     \item{ref_date}{Reference date used.}
#'   }
#'
#' @export
player_stat_rating_profile <- function(player_name, ref_date = Sys.Date(),
                                  seasons = TRUE, params = NULL,
                                  skills = NULL) {
  player <- resolve_player(player_name, seasons = seasons)
  pid <- player$player_id

  if (!is.null(skills)) {
    # Fast path: use pre-computed skills
    all_ratings <- data.table::as.data.table(skills)
    # If multiple snapshots per player, keep latest at or before ref_date
    if ("ref_date" %in% names(all_ratings)) {
      all_ratings <- all_ratings[all_ratings$ref_date <= ref_date]
      all_ratings <- all_ratings[all_ratings[, .I[which.max(ref_date)], by = player_id]$V1]
    }
  } else {
    # Slow path: compute from scratch
    pgd <- load_player_game_data(seasons, use_disk_cache = TRUE)
    ps <- load_player_stats(seasons, use_disk_cache = TRUE)
    stat_rating_data <- .prepare_stat_rating_data(pgd, ps)
    all_ratings <- estimate_player_stat_ratings(stat_rating_data, ref_date = ref_date, params = params)
  }

  if (nrow(all_ratings) == 0 || !pid %in% all_ratings$player_id) {
    cli::cli_abort("Player {.val {player_name}} not found in stat rating estimates (may have fewer than {STAT_RATING_MIN_GAMES} weighted games)")
  }

  # Extract target player row (ensure single row)
  player_row <- all_ratings[player_id == pid]
  if (nrow(player_row) > 1) {
    cli::cli_warn("Multiple skill rows found for player {.val {player_name}}, using latest")
    player_row <- player_row[which.max(ref_date)]
  }
  player_pos <- player_row$pos_group[1]

  # Subsets for percentile computation
  if (is.na(player_pos)) {
    cli::cli_warn("Player {.val {player_name}} has no position group; position-based comparisons will be NA")
    pos_subset <- all_ratings[0]
  } else {
    pos_subset <- all_ratings[pos_group == player_pos]
  }
  stat_defs <- stat_rating_definitions()
  rating_cols <- paste0(stat_defs$stat_name, "_rating")
  rating_cols <- intersect(rating_cols, names(all_ratings))
  stat_names <- sub("_rating$", "", rating_cols)

  # Build profile data.frame
  profile <- data.frame(stat = stat_names, stringsAsFactors = FALSE)
  profile$rating <- as.numeric(player_row[, ..rating_cols])

  # Raw average (unsmoothed career average, NA if column missing)
  raw_cols <- paste0(stat_names, "_raw")
  if (!any(raw_cols %in% names(player_row))) {
    cli::cli_inform(c("i" = "Pre-computed skills data is missing raw average columns.",
                       "i" = "Re-run {.fn estimate_player_stat_ratings} for raw averages."))
  }
  profile$raw_avg <- .extract_player_cols(player_row, raw_cols)

  # League-wide and position-group comparisons (flip percentiles for negative stats)
  hib <- stat_defs$higher_is_better[match(stat_names, stat_defs$stat_name)]
  profile$league_avg <- .col_means(all_ratings, rating_cols)
  profile$league_pct <- .col_pctiles(all_ratings, rating_cols, player_row, hib)
  profile$pos_avg    <- .col_means(pos_subset, rating_cols)
  profile$pos_pct    <- .col_pctiles(pos_subset, rating_cols, player_row, hib)

  # Exposure: per-stat 80s for rate stats, attempts for efficiency stats
  profile$n_80s  <- .extract_player_cols(player_row, paste0(stat_names, "_n80s"))
  profile$wt_80s <- .extract_player_cols(player_row, paste0(stat_names, "_wt80s"))
  profile$attempts    <- .extract_player_cols(player_row, paste0(stat_names, "_attempts"))
  profile$wt_attempts <- .extract_player_cols(player_row, paste0(stat_names, "_wt_attempts"))

  # Credible intervals — merge both naming conventions for transition period
  lower_cols_new <- paste0(stat_names, "_rating_lower")
  lower_cols_old <- paste0(stat_names, "_lower")
  upper_cols_new <- paste0(stat_names, "_rating_upper")
  upper_cols_old <- paste0(stat_names, "_upper")
  # Prefer _rating_lower, fall back to _lower per stat
  lower_cols <- ifelse(lower_cols_new %in% names(player_row), lower_cols_new, lower_cols_old)
  upper_cols <- ifelse(upper_cols_new %in% names(player_row), upper_cols_new, upper_cols_old)
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
  col_order <- c("category", "stat", "type", "rating", "raw_avg",
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
    stat_ratings = profile[order(-profile$pos_pct), ],
    ref_date = ref_date,
    n_games = as.numeric(player_row$n_games),
    n_80s = as.numeric(player_row$n_80s),
    wt_80s = as.numeric(player_row$wt_80s)
  )
  class(out) <- "torp_stat_rating_profile"
  out
}


#' Print a player stat rating profile
#'
#' @param x A \code{torp_stat_rating_profile} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @export
print.torp_stat_rating_profile <- function(x, ...) {
  info <- x$player_info
  sk <- x$stat_ratings

  # Header: games and 80s
  n_g <- if (!is.null(x$n_games)) x$n_games else NA
  n_80 <- if (!is.null(x$n_80s)) round(x$n_80s, 1) else NA
  wt_80 <- if (!is.null(x$wt_80s)) round(x$wt_80s, 1) else NA
  cat(paste0(
    "=== Stat Rating Profile: ", info$name,
    " (", info$team, " - ", info$pos_group, ") ===\n",
    "As at: ", x$ref_date,
    "  |  Games: ", n_g,
    "  |  80s: ", n_80, " (wt: ", wt_80, ")\n\n"
  ))

  # Select display columns (exclude lower/upper)
  display_cols <- intersect(
    c("category", "stat", "type", "rating", "raw_avg",
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
  for (col in intersect(c("rating", "raw_avg", "league_avg", "pos_avg"), names(display))) {
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
# Quick player stat rating lookup
# ============================================================================

#' Get player stat rating estimates from pre-computed data
#'
#' A fast convenience function that looks up stat rating estimates from the
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
#'   (i.e. those with a stat rating estimate in the latest season). Set FALSE to
#'   include all historical players.
#'
#' @return A data.table of stat rating estimates -- one row per player.
#'
#' @seealso [player_stat_rating_profile()] for full profile with percentile ranks,
#'   [load_player_stat_ratings()] to load raw pre-computed data.
#'
#' @export
get_player_stat_ratings <- function(player_name = NULL, ref_date = NULL, seasons = TRUE,
                              current = TRUE) {
  skills <- load_player_stat_ratings(seasons, use_disk_cache = TRUE)
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
# Team stat rating aggregation
# ============================================================================

#' Aggregate player stat ratings to team level
#'
#' For each team in a lineup, sums and averages the stat rating estimates of the
#' players in that team. Used to create team-level features for match
#' prediction models.
#'
#' @param skills A data.table from \code{estimate_player_stat_ratings()}.
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
aggregate_team_stat_ratings <- function(skills, team_lineups, top_n = 22) {
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
  stat_defs <- stat_rating_definitions()
  rating_cols <- paste0(stat_defs$stat_name, "_rating")
  rating_cols <- intersect(rating_cols, names(merged))

  if (length(rating_cols) == 0) {
    cli::cli_warn("No stat rating columns found in merged data")
    return(data.table::data.table())
  }

  # For each match-team, take top_n players by total stat rating and aggregate
  merged[, .total_rating := rowSums(.SD, na.rm = TRUE), .SDcols = rating_cols]

  result <- merged[order(-`.total_rating`),
    head(.SD, top_n),
    by = .(match_id, team)
  ][, {
    out <- list(n_players = .N)
    for (rc in rating_cols) {
      stat_nm <- sub("_rating$", "", rc)
      vals <- get(rc)
      out[[paste0(stat_nm, "_team_sum")]] <- sum(vals, na.rm = TRUE)
      out[[paste0(stat_nm, "_team_mean")]] <- mean(vals, na.rm = TRUE)
    }
    out
  }, by = .(match_id, team)]

  # Clean up
  merged[, .total_rating := NULL]

  result
}


# Backward compatibility aliases
#' @rdname player_stat_rating_profile
#' @export
player_skill_profile <- player_stat_rating_profile

#' @rdname print.torp_stat_rating_profile
#' @export
print.torp_skill_profile <- function(x, ...) print.torp_stat_rating_profile(x, ...)

#' @rdname get_player_stat_ratings
#' @export
get_player_skills <- get_player_stat_ratings

#' @rdname aggregate_team_stat_ratings
#' @export
aggregate_team_skills <- aggregate_team_stat_ratings
