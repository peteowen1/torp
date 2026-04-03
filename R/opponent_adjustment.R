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


#' Adjust per-game player stats for opponent quality (for PSV)
#'
#' For each player-game and each rate stat, computes an opponent adjustment
#' factor based on rolling decay-weighted team defensive profiles.
#' Creates `{stat}_oadj` columns alongside the originals.
#'
#' @param player_stats data.table of player stats with per-game stat counts.
#'   Must have `home_team_name`, `away_team_name`, `team_status`, `match_id`,
#'   `utc_start_time` columns.
#' @param lambda_decay Decay rate per day for opponent profiles.
#' @param prior_games Pseudo-games at league average for shrinkage.
#' @param cap Floor/ceiling for adjustment factors.
#' @param stat_defs Output of [stat_rating_definitions()]. If NULL, uses default.
#' @return The input data with additional `{stat}_oadj` columns for each rate stat.
#' @export
adjust_stats_for_opponents <- function(player_stats,
                                        lambda_decay = OPP_ADJ_LAMBDA_DECAY,
                                        prior_games = 5,
                                        cap = OPP_ADJ_FACTOR_CAP,
                                        stat_defs = NULL) {
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

  # Compute team defensive profiles at the latest date in the data
  # (for per-game adjustment, we need profiles at each match date)
  ref_date <- max(dt$match_date_rating, na.rm = TRUE) + 1L

  cli::cli_inform("Computing team defensive profiles for per-game stat adjustment...")
  team_profiles <- .compute_team_defensive_profiles(
    dt, ref_date, lambda_decay, available_stats, cap
  )

  if (nrow(team_profiles) == 0) {
    cli::cli_warn("No defensive profiles computed")
    return(dt)
  }

  # Join opponent profiles to each player-game
  dt <- merge(dt, team_profiles, by.x = "opponent", by.y = "team", all.x = TRUE)

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
  factor_cols <- grep("_adj_factor$", names(dt), value = TRUE)
  if (length(factor_cols) > 0) dt[, (factor_cols) := NULL]

  # Clean up derived columns if we created them
  n_oadj <- sum(grepl("_oadj$", names(dt)))
  cli::cli_inform("Per-game stat adjustment: {n_oadj} _oadj columns added")

  dt[]
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
#' @return data.table with one row per team, containing `{stat}_adj_factor`.
#' @keywords internal
.compute_team_defensive_profiles <- function(dt_played, ref_date, lambda_decay,
                                              rate_sources, cap) {
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

  # Prior: add pseudo-games at league average rate
  prior_games <- 5
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
        raw_factor <- avg / get(col)
        # Cap the adjustment factor
        pmin(pmax(raw_factor, cap[1]), cap[2])
      }]
    }
  }

  # Keep only team + adj_factor columns
  factor_cols <- grep("_adj_factor$", names(team_allowed), value = TRUE)
  team_allowed[, c("team", factor_cols), with = FALSE]
}
