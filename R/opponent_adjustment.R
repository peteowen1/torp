# Opponent-Adjusted Stat Ratings
# ===============================
# Adjusts Bayesian stat ratings by opponent defensive quality.
# Players who perform well against strong defences get boosted;
# those who pad stats against weak teams get deflated.

#' Adjust player stat ratings for opponent quality
#'
#' For each rate stat, computes an opponent adjustment factor based on
#' how much of that stat each opponent concedes relative to league average.
#' Players who faced tougher schedules get their ratings boosted.
#'
#' @param stat_ratings data.table of player stat ratings from
#'   [estimate_player_stat_ratings()], with `{stat}_rating` columns.
#' @param stat_rating_data data.table from `.prepare_stat_rating_data()`.
#'   Must include `team`, `opponent`, and `match_id` columns.
#' @param ref_date Date. Only matches before this date contribute to
#'   opponent defensive profiles.
#' @param lambda_decay Numeric. Decay rate per day for opponent profile
#'   recency weighting. Default uses `OPP_ADJ_LAMBDA_DECAY`.
#' @param cap Numeric vector of length 2: floor and ceiling for
#'   adjustment factors. Default uses `OPP_ADJ_FACTOR_CAP`.
#' @param stat_defs Output of [stat_rating_definitions()]. If NULL, uses default.
#'
#' @return The input `stat_ratings` with additional `{stat}_adj_rating`
#'   columns for each rate stat. Original columns are unchanged.
#'
#' @export
adjust_stat_ratings_for_opponents <- function(stat_ratings,
                                               stat_rating_data,
                                               ref_date,
                                               lambda_decay = OPP_ADJ_LAMBDA_DECAY,
                                               cap = OPP_ADJ_FACTOR_CAP,
                                               stat_defs = NULL) {
  if (is.null(stat_defs)) stat_defs <- stat_rating_definitions()

  dt <- data.table::as.data.table(stat_rating_data)
  ref_date <- as.Date(ref_date)

  # Validate required columns
  required_cols <- c("player_id", "match_id", "team", "opponent",
                     "match_date_rating", "tog")
  missing <- setdiff(required_cols, names(dt))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns in stat_rating_data: {paste(missing, collapse = ', ')}")
  }

  # Rate stats only (efficiency stats are player-intrinsic)
  rate_defs <- stat_defs[stat_defs$type == "rate", ]
  rate_stats <- rate_defs$stat_name
  rate_sources <- stats::setNames(rate_defs$source_col, rate_defs$stat_name)

  # Filter to played matches before ref_date
  if (!"avail_only" %in% names(dt)) dt[, avail_only := FALSE]
  dt_played <- dt[avail_only == FALSE & match_date_rating < ref_date]

  if (nrow(dt_played) == 0) {
    cli::cli_warn("No match data before ref_date for opponent adjustment")
    return(stat_ratings)
  }

  # Step 1: Compute team defensive profiles
  team_profiles <- .compute_team_defensive_profiles(
    dt_played, ref_date, lambda_decay, rate_sources, cap
  )

  # Step 2: Compute per-player opponent adjustment factors
  # For each player, weighted average of opponent adj_factors across matches
  dt_played[, days_since := as.numeric(ref_date - match_date_rating)]
  dt_played[, opp_wt := exp(-lambda_decay * days_since)]

  # Deduplicate to one row per player-match (take first row)
  player_matches <- unique(dt_played[, .(player_id, match_id, opponent, opp_wt)])

  # Join opponent profiles
  player_adj <- player_matches[team_profiles, on = .(opponent = team), nomatch = 0L]

  if (nrow(player_adj) == 0) {
    cli::cli_warn("No opponent matches found for adjustment")
    return(stat_ratings)
  }

  # Compute weighted average adj_factor per player per stat
  adj_cols <- paste0(rate_stats, "_adj_factor")
  adj_cols_present <- intersect(adj_cols, names(player_adj))

  player_factors <- player_adj[, {
    w <- opp_wt
    w_sum <- sum(w)
    result <- lapply(adj_cols_present, function(col) {
      sum(w * get(col)) / w_sum
    })
    stats::setNames(result, adj_cols_present)
  }, by = player_id]

  # Step 3: Apply adjustment to stat_ratings
  out <- data.table::copy(data.table::as.data.table(stat_ratings))

  # Join player factors
  out <- merge(out, player_factors, by = "player_id", all.x = TRUE)

  # For each rate stat, create {stat}_adj_rating = {stat}_rating * factor
  for (stat_nm in rate_stats) {
    rating_col <- paste0(stat_nm, "_rating")
    adj_factor_col <- paste0(stat_nm, "_adj_factor")
    adj_rating_col <- paste0(stat_nm, "_adj_rating")

    if (rating_col %in% names(out) && adj_factor_col %in% names(out)) {
      out[, (adj_rating_col) := get(rating_col) * data.table::fifelse(
        is.na(get(adj_factor_col)), 1.0, get(adj_factor_col)
      )]
    }
  }

  # Drop intermediate adj_factor columns
  factor_cols <- intersect(adj_cols_present, names(out))
  if (length(factor_cols) > 0) {
    out[, (factor_cols) := NULL]
  }

  out[]
}


#' Adjust per-game player stats for opponent quality
#'
#' For each player-game and each rate stat, computes an opponent adjustment
#' factor based on decay-weighted team defensive profiles.
#' Creates `{stat}_oadj` columns alongside the originals.
#'
#' @param player_stats data.table of player stats with per-game stat counts.
#'   Must have `team`, `opponent`, `match_id`, and either `match_date_rating`
#'   or `utc_start_time` columns. If `team`/`opponent` are missing, derives
#'   them from `home_team_name`, `away_team_name`, `team_status`.
#' @param lambda_decay Decay rate per day for opponent profiles.
#' @param prior_games Pseudo-games at league average for shrinkage.
#' @param cap Floor/ceiling for adjustment factors.
#' @param stat_defs Output of [stat_rating_definitions()]. If NULL, uses default.
#' @param rolling Logical. If TRUE, computes causal rolling profiles (each game
#'   uses only prior games for its opponent profile). If FALSE (default),
#'   computes a single snapshot profile at the latest date.
#' @return The input data with additional `{stat}_oadj` columns for each rate stat.
#' @export
adjust_stats_for_opponents <- function(player_stats,
                                        lambda_decay = OPP_ADJ_LAMBDA_DECAY,
                                        prior_games = 5,
                                        cap = OPP_ADJ_FACTOR_CAP,
                                        stat_defs = NULL,
                                        rolling = FALSE) {
  if (is.null(stat_defs)) stat_defs <- stat_rating_definitions()
  dt <- data.table::as.data.table(player_stats)

  # Derive team/opponent from home_team_name + away_team_name + team_status
  if (!"team" %in% names(dt) || !"opponent" %in% names(dt)) {
    if (!all(c("home_team_name", "away_team_name", "team_status") %in% names(dt))) {
      cli::cli_abort("Need either team/opponent or home_team_name/away_team_name/team_status columns")
    }
    dt[, team := data.table::fifelse(
      tolower(team_status) == "home", home_team_name, away_team_name)]
    dt[, opponent := data.table::fifelse(
      tolower(team_status) == "home", away_team_name, home_team_name)]
  }

  # Need a date column for rolling profiles
  if (!"match_date_rating" %in% names(dt)) {
    dt[, match_date_rating := as.Date(utc_start_time)]
  }

  rate_defs <- stat_defs[stat_defs$type == "rate", ]
  rate_sources <- stats::setNames(rate_defs$source_col, rate_defs$stat_name)

  # Only use stats that exist in the data
  available_stats <- rate_sources[rate_sources %in% names(dt)]
  if (length(available_stats) == 0) {
    cli::cli_warn("No rate stat columns found in player_stats")
    return(dt)
  }

  stat_cols <- intersect(unname(available_stats), names(dt))

  if (rolling) {
    cli::cli_inform("Computing rolling causal defensive profiles for per-game stat adjustment...")
    profiles <- .compute_rolling_stat_profiles(
      dt, lambda_decay, available_stats, cap, prior_games = prior_games
    )
  } else {
    cli::cli_inform("Computing snapshot defensive profiles for per-game stat adjustment...")
    ref_date <- max(dt$match_date_rating, na.rm = TRUE) + 1L
    team_profiles <- .compute_team_defensive_profiles(
      dt, ref_date, lambda_decay, available_stats, cap, prior_games = prior_games
    )
    if (nrow(team_profiles) == 0) {
      cli::cli_warn("No defensive profiles computed")
      return(dt)
    }
    # Expand snapshot to per-match: every match gets the same profile
    match_opponents <- unique(dt[, .(match_id, opponent)])
    profiles <- match_opponents[team_profiles, on = .(opponent = team),
                                 allow.cartesian = TRUE, nomatch = 0L]
  }

  if (nrow(profiles) == 0) {
    cli::cli_warn("No defensive profiles computed")
    return(dt)
  }

  # Join per-match opponent profiles
  factor_cols <- grep("_adj_factor$", names(profiles), value = TRUE)
  join_cols <- c("match_id", "opponent", factor_cols)
  dt <- merge(dt, profiles[, ..join_cols], by = c("match_id", "opponent"), all.x = TRUE)

  # Create _oadj columns: raw_stat * adj_factor
  for (stat_nm in names(available_stats)) {
    src_col <- available_stats[[stat_nm]]
    factor_col <- paste0(stat_nm, "_adj_factor")
    oadj_col <- paste0(src_col, "_oadj")

    if (src_col %in% names(dt) && factor_col %in% names(dt)) {
      dt[, (oadj_col) := get(src_col) * data.table::fifelse(
        is.na(get(factor_col)), 1.0, get(factor_col)
      )]
    }
  }

  # Clean up adj_factor columns
  adj_factor_cols <- grep("_adj_factor$", names(dt), value = TRUE)
  if (length(adj_factor_cols) > 0) dt[, (adj_factor_cols) := NULL]

  n_oadj <- sum(grepl("_oadj$", names(dt)))
  cli::cli_inform("Per-game stat adjustment: {n_oadj} _oadj columns added")

  dt[]
}


#' Compute rolling causal team defensive profiles for stats
#'
#' For each match, computes each team's decay-weighted defensive profile
#' using only games played strictly before that match (causal). This ensures
#' a 2021 game is adjusted by the opponent's 2021 profile, not their 2026 one.
#'
#' Mirrors the approach in `.compute_rolling_epv_profiles()`.
#'
#' @param dt data.table of player stats with team, opponent, match_id,
#'   match_date_rating, and stat columns.
#' @param lambda_decay Numeric. Decay rate per day.
#' @param rate_sources Named character vector: stat_name -> source_col.
#' @param cap Numeric vector of length 2: floor/ceiling for adj_factor.
#' @param prior_games Numeric. Pseudo-games at league average for shrinkage.
#' @return data.table with columns: match_id, opponent, and `{stat}_adj_factor`
#'   for each rate stat. One row per (match_id, opponent) pair.
#' @keywords internal
.compute_rolling_stat_profiles <- function(dt, lambda_decay, rate_sources,
                                            cap, prior_games = 5) {
  stat_cols <- intersect(unname(rate_sources), names(dt))

  # Step 1: Aggregate to team-match level (stats produced by each team)
  team_match <- dt[, lapply(.SD, sum, na.rm = TRUE),
                    .SDcols = stat_cols,
                    by = .(match_id, team, opponent, match_date_rating)]

  # "Stats allowed" = stats produced by the attacking team, attributed to the
  # defending team (opponent). For each row: team T scored these stats against
  # opponent O, so O conceded (allowed) them.
  allowed <- team_match[, lapply(.SD, sum, na.rm = TRUE),
                         .SDcols = stat_cols,
                         by = .(match_id, team_conceding = opponent,
                                match_date_rating)]
  data.table::setorder(allowed, match_date_rating)

  # Step 2: Unique matches in chronological order
  all_matches <- unique(allowed[, .(match_id, match_date_rating)])
  data.table::setorder(all_matches, match_date_rating)

  # Convert stat columns to matrix for vectorised computation
  stat_mat <- as.matrix(allowed[, stat_cols, with = FALSE])

  # Step 3: For each match, compute causal profiles using only prior games
  profiles_list <- vector("list", nrow(all_matches))

  for (i in seq_len(nrow(all_matches))) {
    mid <- all_matches$match_id[i]
    mdate <- all_matches$match_date_rating[i]

    # Strictly prior games (causal)
    prior_idx <- which(allowed$match_date_rating < mdate)

    if (length(prior_idx) == 0) {
      # No prior data -- use neutral adjustment (factor = 1.0) for all teams
      teams_this_match <- unique(allowed[match_id == mid, team_conceding])
      neutral <- data.table::data.table(
        match_id = rep(mid, length(teams_this_match)),
        team_conceding = teams_this_match
      )
      for (j in seq_along(stat_cols)) {
        stat_nm <- names(rate_sources)[rate_sources == stat_cols[j]]
        for (fc in paste0(stat_nm, "_adj_factor")) {
          data.table::set(neutral, j = fc, value = 1.0)
        }
      }
      profiles_list[[i]] <- neutral
      next
    }

    prior_teams <- allowed$team_conceding[prior_idx]
    prior_dates <- allowed$match_date_rating[prior_idx]
    prior_mat <- stat_mat[prior_idx, , drop = FALSE]
    decay_wts <- exp(-lambda_decay * as.numeric(mdate - prior_dates))

    # League average (weighted) across all prior games
    wt_total <- sum(decay_wts)
    league_avg <- colSums(prior_mat * decay_wts) / wt_total

    # Per-team weighted average with shrinkage
    unique_teams <- unique(prior_teams)
    n_teams <- length(unique_teams)
    team_factors <- data.table::data.table(
      match_id = rep(mid, n_teams),
      team_conceding = unique_teams
    )

    for (j in seq_along(stat_cols)) {
      col <- stat_cols[j]
      stat_nm <- names(rate_sources)[rate_sources == col]
      factor_col <- paste0(stat_nm, "_adj_factor")

      factors <- vapply(unique_teams, function(tm) {
        tm_idx <- which(prior_teams == tm)
        tm_wts <- decay_wts[tm_idx]
        wt_sum <- sum(tm_wts)
        if (wt_sum == 0) return(1.0)
        wt_mean <- sum(prior_mat[tm_idx, j] * tm_wts) / wt_sum
        shrunk <- (wt_sum * wt_mean + prior_games * league_avg[j]) /
          (wt_sum + prior_games)
        if (shrunk == 0 || is.na(shrunk) || league_avg[j] == 0) return(1.0)
        raw_factor <- league_avg[j] / shrunk
        min(max(raw_factor, cap[1]), cap[2])
      }, numeric(1))

      # Handle multiple stat_names mapping to same source_col
      for (fc in factor_col) {
        data.table::set(team_factors, j = fc, value = factors)
      }
    }

    profiles_list[[i]] <- team_factors
  }

  profiles <- data.table::rbindlist(profiles_list, fill = TRUE)

  # Rename team_conceding -> opponent for join clarity
  if ("team_conceding" %in% names(profiles)) {
    data.table::setnames(profiles, "team_conceding", "opponent")
  }

  profiles[]
}


#' Compute team defensive profiles (stats conceded per game)
#'
#' For each team and each rate stat, computes the decay-weighted average
#' of that stat conceded per game. Team profiles are shrunk toward the
#' league mean to handle small sample sizes.
#'
#' @param dt_played data.table of played matches (filtered to before ref_date).
#' @param ref_date Date. Reference date for decay computation.
#' @param lambda_decay Numeric. Decay rate per day.
#' @param rate_sources Named character vector: stat_name -> source_col.
#' @param cap Numeric vector of length 2: floor/ceiling for adj_factor.
#' @param prior_games Numeric. Pseudo-games at league average for shrinkage.
#' @return data.table with one row per team, containing `{stat}_adj_factor`.
#' @keywords internal
.compute_team_defensive_profiles <- function(dt_played, ref_date, lambda_decay,
                                              rate_sources, cap, prior_games = 5) {
  # Aggregate player stats to team-match level
  # Each team-match row's stats are what the opponent conceded
  stat_cols <- intersect(unname(rate_sources), names(dt_played))
  if (length(stat_cols) == 0) {
    cli::cli_abort("No rate stat source columns found in data")
  }

  # Sum stats per team per match
  team_match <- dt_played[, lapply(.SD, sum, na.rm = TRUE),
                           .SDcols = stat_cols,
                           by = .(match_id, team, opponent, match_date_rating)]

  # "Stats conceded" = stats produced by the opposition
  # For each match_id + opponent: sum of stats by the 'team' is what 'opponent' conceded
  # So we group by (match_id, opponent) and sum team stats to get allowed stats
  allowed <- team_match[, lapply(.SD, sum, na.rm = TRUE),
                         .SDcols = stat_cols,
                         by = .(match_id, team_conceding = opponent,
                                match_date_rating)]

  # Compute decay weights
  allowed[, days_since := as.numeric(ref_date - match_date_rating)]
  allowed[, decay_wt := exp(-lambda_decay * days_since)]

  # League average (weighted): guard against all-NA or zero weights
  total_wt <- sum(allowed$decay_wt, na.rm = TRUE)
  if (total_wt == 0) {
    cli::cli_warn("All decay weights are zero -- returning empty profiles")
    return(data.table::data.table(team = character(0)))
  }
  league_avg <- allowed[, lapply(.SD, function(x) {
    stats::weighted.mean(x, w = decay_wt, na.rm = TRUE)
  }), .SDcols = stat_cols]

  # Weighted average per team (conceding perspective)
  team_allowed <- allowed[, {
    w <- decay_wt
    n_games <- .N
    result <- lapply(stat_cols, function(col) {
      raw_wt_mean <- sum(w * get(col)) / sum(w)
      # Shrink toward league average
      shrunk <- (sum(w) * raw_wt_mean + prior_games * league_avg[[col]]) /
        (sum(w) + prior_games)
      shrunk
    })
    stats::setNames(result, stat_cols)
  }, by = .(team = team_conceding)]

  # Compute adjustment factors: league_avg / team_allowed
  # > 1 means team is stingy (boost players who faced them)
  # < 1 means team is generous (deflate players who faced them)
  for (col in stat_cols) {
    factor_col <- paste0(
      names(rate_sources)[rate_sources == col], "_adj_factor"
    )
    # May have multiple stat_names mapping to same source_col
    for (fc in factor_col) {
      team_allowed[, (fc) := {
        avg <- league_avg[[col]]
        val <- get(col)
        # Guard: if denominator or league avg is zero/NA, no adjustment
        raw_factor <- data.table::fifelse(
          val == 0 | is.na(val) | avg == 0 | is.na(avg), 1.0, avg / val
        )
        pmin(pmax(raw_factor, cap[1]), cap[2])
      }]
    }
  }

  # Keep only team + adj_factor columns
  factor_cols <- grep("_adj_factor$", names(team_allowed), value = TRUE)
  team_allowed[, c("team", factor_cols), with = FALSE]
}
