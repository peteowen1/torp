# Player Skill Rating (PSR)
# =========================
# Predict match margin from team-aggregated player skills via glmnet,
# then apportion coefficients back to individual players.
# PSR = "predicted margin contribution points" above league average.


#' Resolve path to PSR coefficient CSV
#'
#' Checks inst/extdata first, then falls back to data-raw/cache-stat-ratings/.
#' @param coef_file Filename (default "psr_coefficients.csv")
#' @return Absolute path to the CSV, or "" if not found
#' @keywords internal
.find_psr_coef_path <- function(coef_file = "psr_coefficients.csv") {
  path <- system.file("extdata", coef_file, package = "torp")
  if (path == "") {
    path <- file.path(
      find.package("torp", quiet = TRUE)[1] %||% ".",
      "data-raw", "cache-stat-ratings", coef_file
    )
  }
  if (!file.exists(path)) return("")
  path
}


# Stats excluded from PSV calculation: efficiency ratios (not rate stats),
# bounces (negative coefficient, not causal), and availability metrics
.PSV_EXCLUDE <- c("disposal_efficiency", "goal_accuracy", "contested_poss_rate",
                   "hitout_win_pct", "kick_efficiency", "bounces",
                   "cond_tog", "squad_selection")


#' Calculate Player Skill Ratings (PSR)
#'
#' Computes PSR for each player-round by applying glmnet coefficients to
#' individual player stat rating values. PSR represents each player's predicted
#' contribution to match margin based on their stat rating profile.
#'
#' @param skills A data.table/data.frame from \code{load_player_stat_ratings()},
#'   containing \code{player_id}, \code{player_name}, \code{season},
#'   \code{round}, \code{pos_group}, and \code{*_rating} columns.
#' @param coef_df A data.frame with columns \code{stat_name} and \code{beta},
#'   as produced by the PSR training script. If an \code{sd} column is present,
#'   each stat rating is divided by its SD before multiplying by beta (i.e. the
#'   coefficients are on the standardized scale).
#' @param center Logical. If TRUE (default), subtract the league mean so
#'   PSR = contribution above average player.
#'
#' @return A data.table with columns: \code{player_id}, \code{player_name},
#'   \code{season}, \code{round}, \code{pos_group}, \code{psr_raw}, \code{psr}.
#'
#' @export
calculate_psr <- function(skills, coef_df, center = TRUE) {
  dt <- data.table::as.data.table(skills)

  # Validate coef_df
  if (!all(c("stat_name", "beta") %in% names(coef_df))) {
    cli::cli_abort("{.arg coef_df} must have columns {.val stat_name} and {.val beta}")
  }

  # Filter to non-zero coefficients
  coef_df <- coef_df[coef_df$beta != 0, , drop = FALSE]

  if (nrow(coef_df) == 0) {
    cli::cli_warn("All coefficients are zero -- PSR will be zero for all players")
    dt[, c("psr_raw", "psr") := 0]
    id_cols <- intersect(c("player_id", "player_name", "season", "round", "pos_group"), names(dt))
    return(dt[, c(id_cols, "psr_raw", "psr"), with = FALSE])
  }

  # Map stat_name to rating column name (try _rating first, fall back to _skill)
  skill_cols <- paste0(coef_df$stat_name, "_rating")
  available <- skill_cols %in% names(dt)
  # Backward compat: if _rating columns not found, try _skill
  if (sum(available) == 0) {
    skill_cols <- paste0(coef_df$stat_name, "_skill")
    available <- skill_cols %in% names(dt)
  }

  if (sum(available) == 0) {
    cli::cli_abort("No matching stat rating columns found in data")
  }
  if (any(!available)) {
    missing <- coef_df$stat_name[!available]
    cli::cli_warn("Skill columns not found (skipped): {paste(missing, collapse = ', ')}")
  }

  coef_df <- coef_df[available, , drop = FALSE]
  skill_cols <- skill_cols[available]
  betas <- coef_df$beta

  # Compute PSR: sum of beta_i * (skill_i / sd_i) for each player-round
  mat <- as.matrix(dt[, skill_cols, with = FALSE])
  mat[is.na(mat)] <- 0

  # If coef_df has SD column, normalize skills before applying betas
  if ("sd" %in% names(coef_df)) {
    sds <- coef_df$sd
    sds[sds == 0 | is.na(sds)] <- 1
    mat <- sweep(mat, 2, sds, "/")
  }

  dt[, psr_raw := as.numeric(mat %*% betas)]

  # Center by pos_group (wt_80s-weighted mean subtraction)
  if (center && "pos_group" %in% names(dt) && "wt_80s" %in% names(dt)) {
    dt[!is.na(pos_group), psr := psr_raw - weighted.mean(psr_raw, wt_80s, na.rm = TRUE), by = pos_group]
    dt[is.na(pos_group), psr := psr_raw - weighted.mean(psr_raw, wt_80s, na.rm = TRUE)]
  } else if (center && "pos_group" %in% names(dt)) {
    dt[!is.na(pos_group), psr := psr_raw - mean(psr_raw, na.rm = TRUE), by = pos_group]
    dt[is.na(pos_group), psr := psr_raw - mean(psr_raw, na.rm = TRUE)]
  } else if (center) {
    dt[, psr := psr_raw - mean(psr_raw, na.rm = TRUE)]
  } else {
    dt[, psr := psr_raw]
  }

  id_cols <- intersect(
    c("player_id", "player_name", "season", "round", "pos_group",
      "team", "n_games", "wt_games", "wt_80s"),
    names(dt)
  )

  dt[, c(id_cols, "psr_raw", "psr"), with = FALSE]
}


#' Calculate PSR with Offensive/Defensive Decomposition
#'
#' Computes the margin-based PSR (the best single predictor of match outcomes),
#' then decomposes it into offensive (OSR) and defensive (DSR) components using
#' separately trained coefficient models. The decomposition uses an additive
#' shift so that \code{osr + dsr = psr} exactly.
#'
#' @inheritParams calculate_psr
#' @param osr_coef_df Coefficient data.frame for the offensive model (same
#'   format as \code{coef_df}: columns \code{stat_name}, \code{beta},
#'   optionally \code{sd}).
#' @param dsr_coef_df Coefficient data.frame for the defensive model.
#'
#' @return A data.table with columns: \code{player_id}, \code{player_name},
#'   \code{season}, \code{round}, \code{pos_group}, \code{psr_raw}, \code{psr},
#'   \code{osr}, \code{dsr}.
#'
#' @details
#' The margin-PSR is the gold-standard total rating. OSR and DSR come from
#' models trained on points-scored and points-conceded respectively. Since
#' these are trained independently, \code{raw_osr + raw_dsr != psr} in general.
#' We reconcile by distributing the residual evenly:
#'
#' \deqn{\delta = (psr - raw\_osr - raw\_dsr) / 2}
#' \deqn{osr = raw\_osr + \delta}
#' \deqn{dsr = raw\_dsr + \delta}
#'
#' @export
calculate_psr_components <- function(skills, coef_df, osr_coef_df, dsr_coef_df,
                                     center = TRUE) {
  # Margin PSR (the authoritative total)
  psr_result <- calculate_psr(skills, coef_df, center = center)

  # Raw offensive and defensive scores
  osr_result <- calculate_psr(skills, osr_coef_df, center = center)
  dsr_result <- calculate_psr(skills, dsr_coef_df, center = center)

  # Additive shift: distribute residual evenly so osr + dsr = psr
  raw_osr <- osr_result$psr
  raw_dsr <- dsr_result$psr
  delta <- (psr_result$psr - raw_osr - raw_dsr) / 2

  psr_result[, osr := raw_osr + delta]
  psr_result[, dsr := raw_dsr + delta]

  psr_result
}


# ============================================================================
# PSV: Per-Game Stat Value
# ============================================================================

#' Calculate Player Stat Value (PSV) from Per-Game Stats
#'
#' Applies the same glmnet coefficients used by PSR to raw single-game stats
#' to produce a per-game margin contribution score. While PSR uses Bayesian
#' smoothed career estimates (\code{_rating} columns), PSV uses actual
#' box-score stats from a single game.
#'
#' @param player_stats A data.table/data.frame of per-game player data with
#'   raw stat columns (e.g. \code{goals}, \code{kicks}, \code{disposals}) and
#'   a \code{tog} column (time-on-ground as a fraction 0-1).
#' @param coef_df A data.frame with columns \code{stat_name} and \code{beta}
#'   (same format as for \code{calculate_psr()}). If an \code{sd} column is
#'   present, raw rates are divided by SD before applying betas.
#' @param tog_adjust Logical. If TRUE (default), divide raw counts by TOG to
#'   get per-full-game rates (matching the scale the coefficients were trained
#'   on). If FALSE, use raw counts directly.
#' @param center Logical. If TRUE (default), subtract the per-round league mean
#'   so PSV represents contribution above the average player that round.
#'
#' @return A data.table with identifier columns plus \code{psv_raw} and
#'   \code{psv}.
#'
#' @export
calculate_psv <- function(player_stats, coef_df, tog_adjust = TRUE, center = TRUE) {
  dt <- data.table::as.data.table(player_stats)

  if (!all(c("stat_name", "beta") %in% names(coef_df))) {
    cli::cli_abort("{.arg coef_df} must have columns {.val stat_name} and {.val beta}")
  }

  coef_df <- coef_df[coef_df$beta != 0, , drop = FALSE]

  if (nrow(coef_df) == 0) {
    dt[, c("psv_raw", "psv") := 0]
    id_cols <- intersect(c("player_id", "player_name", "season", "round", "match_id", "team"), names(dt))
    return(dt[, c(id_cols, "psv_raw", "psv"), with = FALSE])
  }

  # Map stat_name to raw stat columns (direct column names, not _rating)
  stat_cols <- coef_df$stat_name

  # Exclude stats that don't belong in per-game PSV before checking availability
  # (avoids noisy warnings for stats that would be dropped anyway):
  #  - Efficiency %s (redundant — numerator + denominator already in as rate stats)
  #  - bounces (negative coefficient, not causal)
  #  - cond_tog, squad_selection (availability metrics, not performance)
  psv_exclude <- .PSV_EXCLUDE
  keep <- !stat_cols %in% psv_exclude
  coef_df <- coef_df[keep, , drop = FALSE]
  stat_cols <- stat_cols[keep]

  available <- stat_cols %in% names(dt)

  if (sum(available) == 0) {
    cli::cli_abort("No matching stat columns found in data for PSV calculation")
  }
  if (any(!available)) {
    missing <- stat_cols[!available]
    cli::cli_warn("Stat columns not found (skipped): {paste(missing, collapse = ', ')}")
  }

  coef_df <- coef_df[available, , drop = FALSE]
  stat_cols <- stat_cols[available]
  betas <- coef_df$beta

  # Use _oadj (opponent-adjusted) columns when available, fall back to raw
  oadj_cols <- paste0(stat_cols, "_oadj")
  has_oadj <- all(oadj_cols %in% names(dt))
  use_cols <- if (has_oadj) oadj_cols else stat_cols

  # Extract stat values
  mat <- as.matrix(dt[, use_cols, with = FALSE])
  mat[is.na(mat)] <- 0

  # TOG-adjust: divide counts by TOG to get per-full-game rates
  if (tog_adjust && "tog" %in% names(dt)) {
    tog_vec <- as.numeric(dt$tog)
    tog_vec[is.na(tog_vec) | tog_vec <= 0] <- 1
    mat <- mat / tog_vec
  }

  # Standardize using SDs from coefficient file (same scale as PSR training)
  if ("sd" %in% names(coef_df)) {
    sds <- coef_df$sd
    sds[sds == 0 | is.na(sds)] <- 1
    mat <- sweep(mat, 2, sds, "/")
  }

  dt[, psv_raw := as.numeric(mat %*% betas)]

  # Center by pos_group (TOG-weighted mean subtraction)
  if (center && "pos_group" %in% names(dt) && "tog" %in% names(dt)) {
    dt[, .tog_wt := pmax(as.numeric(tog), 0.1)]
    dt[!is.na(pos_group), psv := psv_raw - weighted.mean(psv_raw, .tog_wt, na.rm = TRUE), by = pos_group]
    dt[is.na(pos_group), psv := psv_raw - weighted.mean(psv_raw, .tog_wt, na.rm = TRUE)]
    dt[, .tog_wt := NULL]
  } else if (center && "pos_group" %in% names(dt)) {
    dt[!is.na(pos_group), psv := psv_raw - mean(psv_raw, na.rm = TRUE), by = pos_group]
    dt[is.na(pos_group), psv := psv_raw - mean(psv_raw, na.rm = TRUE)]
  } else if (center) {
    dt[, psv := psv_raw - mean(psv_raw, na.rm = TRUE)]
  } else {
    dt[, psv := psv_raw]
  }

  id_cols <- intersect(
    c("player_id", "player_name", "season", "round", "match_id",
      "team", "opponent", "tog"),
    names(dt)
  )

  dt[, c(id_cols, "psv_raw", "psv"), with = FALSE]
}


#' Calculate PSV with Offensive/Defensive Decomposition
#'
#' Applies offensive and defensive coefficient models to per-game stats,
#' producing \code{psv}, \code{osv}, and \code{dsv} columns where
#' \code{osv + dsv = psv}.
#'
#' @inheritParams calculate_psv
#' @param osr_coef_df Coefficient data.frame for the offensive model.
#' @param dsr_coef_df Coefficient data.frame for the defensive model.
#'
#' @return A data.table with identifier columns plus \code{psv_raw},
#'   \code{psv}, \code{osv}, \code{dsv}.
#'
#' @export
calculate_psv_components <- function(player_stats, coef_df, osr_coef_df,
                                      dsr_coef_df, tog_adjust = TRUE,
                                      center = TRUE) {
  psv_result <- calculate_psv(player_stats, coef_df, tog_adjust = tog_adjust,
                               center = center)
  osv_result <- calculate_psv(player_stats, osr_coef_df, tog_adjust = tog_adjust,
                               center = center)
  dsv_result <- calculate_psv(player_stats, dsr_coef_df, tog_adjust = tog_adjust,
                               center = center)

  # Additive shift so osv + dsv = psv
  raw_osv <- osv_result$psv
  raw_dsv <- dsv_result$psv
  delta <- (psv_result$psv - raw_osv - raw_dsv) / 2

  psv_result[, osv := raw_osv + delta]
  psv_result[, dsv := raw_dsv + delta]

  psv_result
}


#' Convenience wrapper to compute PSV from coefficient files
#'
#' Loads the margin, offensive, and defensive coefficient CSVs from
#' \code{inst/extdata} and calls \code{\link{calculate_psv_components}}.
#'
#' @inheritParams calculate_psv
#' @param psr_coef_path Path to the margin PSR coefficient CSV. If NULL,
#'   searches \code{inst/extdata/psr_coefficients.csv}.
#'
#' @return A data.table with \code{psv}, \code{osv}, \code{dsv} columns.
#'
#' @keywords internal
.compute_psv <- function(player_stats, psr_coef_path = NULL, tog_adjust = TRUE,
                          center = TRUE) {
  if (is.null(psr_coef_path)) {
    psr_coef_path <- .find_psr_coef_path()
  }

  if (!nzchar(psr_coef_path) || !file.exists(psr_coef_path)) {
    cli::cli_warn("PSR coefficient file not found: {psr_coef_path}")
    return(NULL)
  }

  coef_df <- utils::read.csv(psr_coef_path)

  coef_dir <- dirname(psr_coef_path)
  osr_path <- file.path(coef_dir, "osr_coefficients.csv")
  dsr_path <- file.path(coef_dir, "dsr_coefficients.csv")

  if (file.exists(osr_path) && file.exists(dsr_path)) {
    osr_coef_df <- utils::read.csv(osr_path)
    dsr_coef_df <- utils::read.csv(dsr_path)
    calculate_psv_components(player_stats, coef_df, osr_coef_df, dsr_coef_df,
                              tog_adjust = tog_adjust, center = center)
  } else {
    cli::cli_inform("OSR/DSR coefficient files not found -- computing PSV only (no osv/dsv decomposition)")
    calculate_psv(player_stats, coef_df, tog_adjust = tog_adjust, center = center)
  }
}


#' Explain a Player's Game Value (EPV + PSV Breakdown)
#'
#' Shows both the possession-value (EPV) and stat-value (PSV) decomposition
#' for a player's game performance. EPV comes from play-by-play chain data;
#' PSV comes from applying glmnet coefficients to box-score stats.
#'
#' @param player_id Player ID string (e.g. \code{"CD_I1001024"}).
#' @param match_id Match ID string (e.g. \code{"CD_M20260140102"}).
#' @param player_stats Optional pre-loaded player stats data.table. If NULL,
#'   loads from API via \code{load_player_stats()}.
#' @param season Season year (used if \code{player_stats} is NULL).
#' @param per80 Logical. If TRUE, show per-80-minute contributions.
#'   Default TRUE.
#'
#' @return Invisibly returns a list with \code{epv} (EPV summary data.table)
#'   and \code{psv} (PSV per-stat breakdown data.table).
#'
#' @export
explain_player_game <- function(player_id, match_id, player_stats = NULL,
                                 season = NULL, per80 = TRUE) {

  if (is.null(season)) season <- as.integer(substr(match_id, 5, 8))
  pid <- as.character(player_id)
  mid <- as.character(match_id)

  # --- Load player stats ---
  if (is.null(player_stats)) {
    player_stats <- data.table::as.data.table(load_player_stats(season))
  }
  ps <- data.table::as.data.table(player_stats)
  ps[, player_id := as.character(player_id)]
  ps[, match_id := as.character(match_id)]

  row <- ps[ps$player_id == pid & ps$match_id == mid]
  if (nrow(row) == 0) cli::cli_abort("No data found for player {player_id} in match {match_id}")
  if (nrow(row) > 1) row <- row[1]

  # TOG
  tog_val <- if ("tog" %in% names(row)) {
    as.numeric(row$tog)
  } else if ("time_on_ground_percentage" %in% names(row)) {
    pmax(as.numeric(row$time_on_ground_percentage) / 100, 0.1)
  } else 1
  if (is.na(tog_val) || tog_val <= 0) tog_val <- 1

  pname <- if ("player_name" %in% names(row)) row$player_name[1] else player_id

  # Derive team/opp if not present (player_stats uses home/away + team_status)
  if (!"team" %in% names(row) &&
      all(c("home_team_name", "away_team_name", "team_status") %in% names(row))) {
    row[, team := data.table::fifelse(team_status == "home", home_team_name, away_team_name)]
    row[, opp := data.table::fifelse(team_status == "home", away_team_name, home_team_name)]
  }
  team_nm <- if ("team" %in% names(row)) row$team[1] else ""
  opp_nm <- ""
  for (ocol in c("opp", "opponent")) {
    if (ocol %in% names(row)) { opp_nm <- row[[ocol]][1]; break }
  }

  # =========================================================================
  # SECTION 1: EPV (from player game ratings — chain-based possession value)
  # =========================================================================
  cli::cli_h2("{pname} | {team_nm} v {opp_nm} | TOG {round(tog_val * 100)}%")

  suffix <- if (per80) " (per 80 min)" else " (total)"
  epv_summary <- NULL
  pgr <- tryCatch(load_player_game_ratings(season), error = function(e) NULL)
  if (!is.null(pgr)) {
    pgr_dt <- data.table::as.data.table(pgr)
    pgr_dt[, player_id := as.character(player_id)]
    pgr_dt[, match_id := as.character(match_id)]
    epv_row <- pgr_dt[pgr_dt$player_id == pid & pgr_dt$match_id == mid]

    if (nrow(epv_row) > 0) {
      epv_row <- epv_row[1]

      # Pick total or p80 columns based on per80 flag
      if (per80) {
        comp_cols <- c(recv_epv = "recv_epv_p80", disp_epv = "disp_epv_p80",
                       spoil_epv = "spoil_epv_p80", hitout_epv = "hitout_epv_p80")
        epv_total_col <- "epv_p80"
      } else {
        comp_cols <- c(recv_epv = "recv_epv", disp_epv = "disp_epv",
                       spoil_epv = "spoil_epv", hitout_epv = "hitout_epv")
        epv_total_col <- "epv"
      }

      # Build EPV summary table
      epv_vals <- vapply(comp_cols, function(col) {
        if (col %in% names(epv_row)) round(as.numeric(epv_row[[col]]), 1) else NA_real_
      }, numeric(1))
      epv_total <- if (epv_total_col %in% names(epv_row)) {
        round(as.numeric(epv_row[[epv_total_col]]), 1)
      } else {
        round(sum(epv_vals, na.rm = TRUE), 1)
      }

      epv_summary <- data.table::data.table(
        component = c(names(comp_cols), "TOTAL"),
        epv = c(epv_vals, epv_total)
      )

      # Add position context: percentile among same-position players that round
      position_col <- intersect(c("position_group", "position"), names(epv_row))
      if (length(position_col) > 0) {
        pos <- epv_row[[position_col[1]]]
        pos_peers <- pgr_dt[pgr_dt$season == season &
                            pgr_dt[[position_col[1]]] == pos]
        if (nrow(pos_peers) > 1 && epv_total_col %in% names(pos_peers)) {
          pctile <- round(mean(pos_peers[[epv_total_col]] <= epv_total, na.rm = TRUE) * 100)
          epv_summary[component == "TOTAL", pctile := pctile]
        }
      }

      # Also grab PSV/OSV/DSV from the same row if available
      psv_from_pgr <- list()
      for (vcol in c("psv", "osv", "dsv", "torp_value")) {
        if (vcol %in% names(epv_row)) psv_from_pgr[[vcol]] <- round(as.numeric(epv_row[[vcol]]), 1)
      }

      cli::cli_h3("EPV{suffix} -- from play-by-play chains")
      print(epv_summary, row.names = FALSE)

      if (length(psv_from_pgr) > 0) {
        psv_line <- paste(names(psv_from_pgr), "=", psv_from_pgr, collapse = " | ")
        cat(sprintf("  PSV summary: %s\n", psv_line))
      }
    } else {
      cli::cli_inform("No EPV game ratings found for this match (not yet released?)")
    }
  }

  # =========================================================================
  # SECTION 2: PSV (stat-by-stat breakdown from glmnet coefficients)
  # =========================================================================
  cli::cli_h3("PSV{suffix} -- stat-by-stat breakdown")

  # Load coefficients
  psr_path <- .find_psr_coef_path()
  if (!nzchar(psr_path)) cli::cli_abort("PSR coefficient file not found")

  coef_df <- utils::read.csv(psr_path)
  coef_dir <- dirname(psr_path)
  osr_path <- file.path(coef_dir, "osr_coefficients.csv")
  dsr_path <- file.path(coef_dir, "dsr_coefficients.csv")
  has_components <- file.exists(osr_path) && file.exists(dsr_path)
  if (has_components) {
    osr_coefs <- utils::read.csv(osr_path)
    dsr_coefs <- utils::read.csv(dsr_path)
  }

  # PSV exclude list
  psv_exclude <- .PSV_EXCLUDE

  # Build breakdown for a single coefficient set
  .breakdown <- function(cdf, label) {
    cdf <- cdf[cdf$beta != 0 & !cdf$stat_name %in% psv_exclude, , drop = FALSE]
    available <- cdf$stat_name %in% names(row)
    cdf <- cdf[available, , drop = FALSE]

    raw_vals <- vapply(cdf$stat_name, function(s) {
      v <- as.numeric(row[[s]]); if (is.na(v)) 0 else v
    }, numeric(1))

    rate_vals <- raw_vals / tog_val
    sds <- if ("sd" %in% names(cdf)) { s <- cdf$sd; s[s == 0 | is.na(s)] <- 1; s } else rep(1, nrow(cdf))
    contributions <- (rate_vals / sds) * cdf$beta

    data.table::data.table(
      stat = cdf$stat_name,
      contribution = round(contributions, 2)
    ) |> data.table::setnames("contribution", label)
  }

  psv_result <- .breakdown(coef_df, "psv")
  if (has_components) {
    osv_dt <- .breakdown(osr_coefs, "osv")
    dsv_dt <- .breakdown(dsr_coefs, "dsv")
    psv_result <- merge(psv_result, osv_dt, by = "stat", all = TRUE)
    psv_result <- merge(psv_result, dsv_dt, by = "stat", all = TRUE)
  }

  # Add raw values and rates
  psv_result[, raw_value := vapply(stat, function(s) {
    v <- as.numeric(row[[s]]); if (is.na(v)) 0 else v
  }, numeric(1))]
  psv_result[, rate := round(raw_value / tog_val, 1)]

  # Reorder and sort
  front_cols <- c("stat", "raw_value", "rate", "psv")
  if (has_components) front_cols <- c(front_cols, "osv", "dsv")
  data.table::setcolorder(psv_result, intersect(front_cols, names(psv_result)))
  data.table::setorderv(psv_result, "psv", order = -1L)

  # Per-game totals (not per-80) — multiply by tog
  if (!per80) {
    for (col in intersect(c("psv", "osv", "dsv"), names(psv_result)))
      psv_result[, (col) := round(get(col) * tog_val, 2)]
  }

  total_psv <- sum(psv_result$psv, na.rm = TRUE)
  cat(sprintf("  Total PSV: %.1f\n", total_psv))

  # Filter out zero-contribution stats for cleaner output
  psv_result <- psv_result[psv != 0 | raw_value != 0]

  print(psv_result, nrows = nrow(psv_result))
  invisible(list(epv = epv_summary, psv = psv_result))
}


#' Show a Player's Biggest Plays from PBP
#'
#' Pulls play-by-play rows for a player and computes the EPV credit each play
#' generated (using the same formula as [create_player_game_data()]). Shows
#' the surrounding context (1 row before, 1 after) for each top play so you
#' can see how the passage unfolded.
#'
#' @param player Character player name (partial match OK) or player_id string.
#' @param season Season year(s) to search. Default is current season.
#' @param match_id Optional match ID to filter to a single game.
#' @param top_n Number of top plays to show per role. Default 5.
#' @param context Number of rows before/after each play to show. Default 1.
#' @param pbp_data Optional pre-loaded PBP data. If NULL, loads automatically.
#'
#' @return Invisibly returns a list with \code{as_receiver} and
#'   \code{as_disposer} data.tables of the biggest plays.
#'
#' @export
explain_player_plays <- function(player,
                                  season = get_afl_season(),
                                  match_id = NULL,
                                  top_n = 5,
                                  context = 1,
                                  pbp_data = NULL) {

  # Resolve player
  if (grepl("^CD_I", player)) {
    pid <- player
    pname <- player
  } else {
    resolved <- resolve_player(player)
    pid <- resolved$player_id
    pname <- resolved$player_name
  }

  # Load PBP
  if (is.null(pbp_data)) {
    pbp_data <- load_pbp(season)
  }
  dt <- data.table::as.data.table(pbp_data)
  data.table::setorder(dt, match_id, display_order)

  if (!is.null(match_id)) {
    dt <- dt[dt$match_id %in% match_id]
  }

  # Columns to show in context view
  ctx_cols <- c("display_order", "period", "player_name", "team",
                "description", "play_type", "phase_of_play",
                "x", "y", "pos_team", "delta_epv", "exp_pts",
                "lead_player", "lead_desc_tot")
  ctx_cols <- intersect(ctx_cols, names(dt))

  # Add a global row index for context lookups
  dt[, .row_idx := .I]

  # Load EPV params (same as create_player_game_data uses)
  p <- default_epv_params()

  # Helper: get context rows around a set of row indices
  .get_context <- function(idx, n = context) {
    offsets <- seq(-n, n)
    all_idx <- sort(unique(unlist(lapply(idx, function(i) i + offsets))))
    all_idx <- all_idx[all_idx >= 1L & all_idx <= nrow(dt)]
    out <- dt[all_idx, c(".row_idx", ctx_cols), with = FALSE]
    # Mark which rows are the target plays
    out[, is_play := .row_idx %in% idx]
    out
  }

  # --- Receiving plays (player is lead_player_id = next to touch the ball) ---
  recv_plays <- dt[dt$lead_player_id == pid]
  if (nrow(recv_plays) > 0) {
    recv_plays[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
    recv_plays[, recv_credit := data.table::fifelse(
      is_intercept_mark,
      ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) * p$recv_intercept_mark_scale,
      data.table::fifelse(
        pos_team == -1L,
        ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) * p$recv_scale,
        ((p$recv_pos_mult * delta_epv * pos_team) + p$recv_pos_offset) * p$recv_scale
      )
    )]
    recv_plays[, is_intercept_mark := NULL]
  }

  # --- Disposal plays (player is player_id = disposer) ---
  disp_plays <- dt[dt$player_id == pid]
  if (nrow(disp_plays) > 0) {
    disp_plays[, disp_credit := data.table::fifelse(
      pos_team == -1,
      delta_epv + p$disp_neg_offset,
      delta_epv + p$disp_pos_offset
    ) * p$disp_scale]
  }

  # Print
  cli::cli_h1("{pname} | Biggest Plays (season {paste(season, collapse=',')})")

  # --- Receiving output ---
  if (nrow(recv_plays) > 0) {
    cli::cli_h2("As Receiver (top {min(top_n, nrow(recv_plays))} of {nrow(recv_plays)} plays)")

    top_recv <- recv_plays[order(-abs(recv_credit))][seq_len(min(top_n, nrow(recv_plays)))]
    top_recv_idx <- top_recv$.row_idx

    # Print each top play with context
    for (i in seq_along(top_recv_idx)) {
      r <- top_recv[i]
      round_lbl <- if ("round_week" %in% names(r)) r$round_week else "?"
      opp_lbl <- if (all(c("home_away", "away_team_name", "home_team_name") %in% names(r))) {
        if (r$home_away == "Home") r$away_team_name else r$home_team_name
      } else "?"
      cli::cli_h3("Play {i}: recv_credit={round(r$recv_credit, 2)} | Rd {round_lbl} v {opp_lbl} | Q{r$period}")
      ctx <- .get_context(top_recv_idx[i])
      # Round numeric cols for display
      for (nc in intersect(c("delta_epv", "exp_pts", "x", "y"), names(ctx))) {
        ctx[, (nc) := round(get(nc), 1)]
      }
      # Mark the target row
      ctx[, marker := ifelse(is_play, ">>>", "")]
      ctx[, is_play := NULL]
      ctx[, .row_idx := NULL]
      print(ctx, row.names = FALSE)
      cat("\n")
    }

    cat(sprintf("  Total recv credit: %.1f across %d plays\n",
                sum(recv_plays$recv_credit, na.rm = TRUE), nrow(recv_plays)))

    # Per-game summary
    recv_by_game <- recv_plays[, .(
      plays = .N,
      total_credit = round(sum(recv_credit, na.rm = TRUE), 1),
      max_play = round(max(recv_credit, na.rm = TRUE), 1),
      min_play = round(min(recv_credit, na.rm = TRUE), 1)
    ), by = .(match_id, round_week)]
    data.table::setorderv(recv_by_game, "total_credit", order = -1L)
    cli::cli_h3("Per-game recv credit summary")
    print(recv_by_game, row.names = FALSE)
  } else {
    cli::cli_inform("No receiving plays found")
  }

  # --- Disposal output ---
  if (nrow(disp_plays) > 0) {
    cli::cli_h2("As Disposer (top {min(top_n, nrow(disp_plays))} of {nrow(disp_plays)} plays)")

    top_disp <- disp_plays[order(-abs(disp_credit))][seq_len(min(top_n, nrow(disp_plays)))]
    top_disp_idx <- top_disp$.row_idx

    for (i in seq_along(top_disp_idx)) {
      r <- top_disp[i]
      round_lbl <- if ("round_week" %in% names(r)) r$round_week else "?"
      opp_lbl <- if (all(c("home_away", "away_team_name", "home_team_name") %in% names(r))) {
        if (r$home_away == "Home") r$away_team_name else r$home_team_name
      } else "?"
      cli::cli_h3("Play {i}: disp_credit={round(r$disp_credit, 2)} | Rd {round_lbl} v {opp_lbl} | Q{r$period}")
      ctx <- .get_context(top_disp_idx[i])
      for (nc in intersect(c("delta_epv", "exp_pts", "x", "y"), names(ctx))) {
        ctx[, (nc) := round(get(nc), 1)]
      }
      ctx[, marker := ifelse(is_play, ">>>", "")]
      ctx[, is_play := NULL]
      ctx[, .row_idx := NULL]
      print(ctx, row.names = FALSE)
      cat("\n")
    }

    cat(sprintf("  Total disp credit: %.1f across %d plays\n",
                sum(disp_plays$disp_credit, na.rm = TRUE), nrow(disp_plays)))
  } else {
    cli::cli_inform("No disposal plays found")
  }

  # Clean up temp column
  dt[, .row_idx := NULL]

  invisible(list(as_receiver = recv_plays, as_disposer = disp_plays))
}


#' Explain a Player's EPR Calculation (Per-Game Trace)
#'
#' Traces the exact per-game inputs to the EPR calculation: shows each game's
#' \code{recv_epv_adj}, \code{disp_epv_adj}, etc. (position-adjusted per-80-min
#' rates), the decay weight for each game, and how Bayesian shrinkage produces
#' the final EPR components.
#'
#' @param player Character player name (partial match OK) or player_id string.
#' @param season_val Season year. Default is current season.
#' @param round_val Round number. Default is next round.
#' @param top_n Number of most recent games to show. Default 15.
#'
#' @return Invisibly returns a list with \code{game_trace} (per-game data.table)
#'   and \code{shrinkage} (named list of component calculations).
#'
#' @export
explain_epr <- function(player,
                         season_val = get_afl_season(type = "current"),
                         round_val = get_afl_week(type = "next"),
                         top_n = 15) {

  # Resolve player
  if (grepl("^CD_I", player)) {
    pid <- player
    pname <- player
  } else {
    resolved <- resolve_player(player)
    pid <- resolved$player_id
    pname <- resolved$player_name
  }

  # Load player game data (the raw _adj values that feed EPR)
  pgd <- data.table::as.data.table(load_player_game_data(TRUE))

  # Build match_ref and date_val the same way calculate_epr does
  fixtures <- load_fixtures(TRUE)
  gwk <- sprintf("%02d", round_val)
  match_ref <- paste0("CD_M", season_val, "014", gwk)

  date_val <- fixtures[fixtures$season == season_val & fixtures$round_number == round_val, ]
  date_val <- min(as.Date(date_val$utc_start_time), na.rm = TRUE)

  # Filter to this player's games before the reference round
  data.table::setkey(pgd, match_id)
  dt <- pgd[match_id <= match_ref & player_id == pid]
  if (nrow(dt) == 0) cli::cli_abort("No games found for {pname} before {season_val} round {round_val}")

  dt[, days_diff := as.numeric(as.Date(date_val) - as.Date(utc_start_time))]
  dt <- dt[days_diff >= 0]

  # Decay weights (same constants as calculate_epr_stats)
  dt[, `:=`(
    wt_recv   = exp(-days_diff / EPR_DECAY_RECV),
    wt_disp   = exp(-days_diff / EPR_DECAY_DISP),
    wt_spoil  = exp(-days_diff / EPR_DECAY_SPOIL),
    wt_hitout = exp(-days_diff / EPR_DECAY_HITOUT),
    tog = pmax(time_on_ground_percentage / 100, 0.1)
  )]

  # Show per-game trace
  cli::cli_h1("{pname} | EPR Calculation Trace")
  cli::cli_h2("Per-game inputs (position-adjusted per-80-min rates)")

  show_dt <- dt[order(-season, -round), .(
    season, round, team, opponent, tog,
    recv_adj = round(recv_epv_adj, 2),
    disp_adj = round(disp_epv_adj, 2),
    spoil_adj = round(spoil_epv_adj, 2),
    hitout_adj = round(hitout_epv_adj, 2),
    recv_total = round(recv_epv_adj * tog, 2),
    days_ago = days_diff,
    wt_recv = round(wt_recv, 3)
  )]

  cat(sprintf("  Showing %d of %d career games:\n", min(top_n, nrow(show_dt)), nrow(dt)))
  print(head(show_dt, top_n), row.names = FALSE)

  # Compute the aggregates (same as calculate_epr_stats — TOG-weighted)
  # _adj is per-80 rate; multiply by tog to get game total, weight by decay
  recv_sum   <- sum(dt$recv_epv_adj * dt$tog * dt$wt_recv, na.rm = TRUE)
  disp_sum   <- sum(dt$disp_epv_adj * dt$tog * dt$wt_disp, na.rm = TRUE)
  spoil_sum  <- sum(dt$spoil_epv_adj * dt$tog * dt$wt_spoil, na.rm = TRUE)
  hitout_sum <- sum(dt$hitout_epv_adj * dt$tog * dt$wt_hitout, na.rm = TRUE)

  # Denominator is weighted minutes (wt * tog), not weighted games
  wt_gms_recv   <- sum(dt$wt_recv * dt$tog, na.rm = TRUE)
  wt_gms_disp   <- sum(dt$wt_disp * dt$tog, na.rm = TRUE)
  wt_gms_spoil  <- sum(dt$wt_spoil * dt$tog, na.rm = TRUE)
  wt_gms_hitout <- sum(dt$wt_hitout * dt$tog, na.rm = TRUE)

  # Bayesian shrinkage (use actual per-component constants)
  loading <- EPR_LOADING_DEFAULT
  prior_gms <- c(recv = EPR_PRIOR_GAMES_RECV, disp = EPR_PRIOR_GAMES_DISP,
                  spoil = EPR_PRIOR_GAMES_SPOIL, hitout = EPR_PRIOR_GAMES_HITOUT)
  recv_epr   <- (loading * recv_sum   + prior_gms["recv"]   * EPR_PRIOR_RATE_RECV)   / (wt_gms_recv   + prior_gms["recv"])
  disp_epr   <- (loading * disp_sum   + prior_gms["disp"]   * EPR_PRIOR_RATE_DISP)   / (wt_gms_disp   + prior_gms["disp"])
  spoil_epr  <- (loading * spoil_sum  + prior_gms["spoil"]  * EPR_PRIOR_RATE_SPOIL)  / (wt_gms_spoil  + prior_gms["spoil"])
  hitout_epr <- (loading * hitout_sum + prior_gms["hitout"] * EPR_PRIOR_RATE_HITOUT) / (wt_gms_hitout + prior_gms["hitout"])

  cli::cli_h2("Shrinkage Calculation (pre-centering, TOG-weighted)")

  components <- data.table::data.table(
    component = c("recv", "disp", "spoil", "hitout"),
    weighted_sum = round(c(recv_sum, disp_sum, spoil_sum, hitout_sum), 2),
    wt_mins = round(c(wt_gms_recv, wt_gms_disp, wt_gms_spoil, wt_gms_hitout), 2),
    decay_days = c(EPR_DECAY_RECV, EPR_DECAY_DISP, EPR_DECAY_SPOIL, EPR_DECAY_HITOUT),
    prior_games = unname(prior_gms),
    prior_rate = c(EPR_PRIOR_RATE_RECV, EPR_PRIOR_RATE_DISP, EPR_PRIOR_RATE_SPOIL, EPR_PRIOR_RATE_HITOUT),
    epr_raw = round(c(recv_epr, disp_epr, spoil_epr, hitout_epr), 2)
  )
  print(components, row.names = FALSE)

  cat(sprintf("\n  Formula: EPR_i = (%.1f * sum_i + prior_i * rate_i) / (wt_mins_i + prior_i)\n", loading))
  cat(sprintf("  recv_epr = (%.1f * %.2f + %.1f * %.1f) / (%.2f + %.1f) = %.2f\n",
              loading, recv_sum, prior_gms["recv"], EPR_PRIOR_RATE_RECV, wt_gms_recv, prior_gms["recv"], recv_epr))
  cat(sprintf("  disp_epr = (%.1f * %.2f + %.1f * %.1f) / (%.2f + %.1f) = %.2f\n",
              loading, disp_sum, prior_gms["disp"], EPR_PRIOR_RATE_DISP, wt_gms_disp, prior_gms["disp"], disp_epr))

  # Weighted average recv rate (what recv_epr approximates before shrinkage)
  avg_recv <- if (wt_gms_recv > 0) recv_sum / wt_gms_recv else 0
  cat(sprintf("\n  Weighted avg recv rate: %.2f (shrinkage pulls toward %.1f)\n",
              avg_recv, EPR_PRIOR_RATE_RECV))
  cat(sprintf("  Shrinkage strength: %.0f%% data / %.0f%% prior (wt_mins=%.1f vs prior=%.1f)\n",
              wt_gms_recv / (wt_gms_recv + prior_gms["recv"]) * 100,
              prior_gms["recv"] / (wt_gms_recv + prior_gms["recv"]) * 100,
              wt_gms_recv, prior_gms["recv"]))

  cat(sprintf("\n  Note: sums use game totals (per80 * tog), denominator is weighted minutes\n"))
  cat(sprintf("  (wt * tog). Final EPR also has TOG-weighted centering (see calculate_epr()).\n"))

  invisible(list(
    game_trace = dt,
    shrinkage = list(
      components = components,
      recv_sum = recv_sum, disp_sum = disp_sum,
      spoil_sum = spoil_sum, hitout_sum = hitout_sum,
      wt_mins = c(recv = wt_gms_recv, disp = wt_gms_disp,
                   spoil = wt_gms_spoil, hitout = wt_gms_hitout),
      epr_raw = c(recv = recv_epr, disp = disp_epr,
                   spoil = spoil_epr, hitout = hitout_epr)
    )
  ))
}


#' Explain a Player's TORP Rating Decomposition
#'
#' Diagnostic function showing why a player has a given TORP, EPR, and PSR.
#' Breaks down EPR into recv/disp/spoil/hitout components with shrinkage
#' diagnostics, and PSR into per-stat contributions. Useful for understanding
#' divergences between EPR and PSR.
#'
#' @param player Character player name (partial match OK) or player_id string.
#' @param season_val Season year. Default is current season.
#' @param round_val Round number. Default is next round.
#' @param top_n Integer. Number of top PSR contributors to show. Default 15.
#'
#' @return Invisibly returns a list with \code{epr_breakdown}, \code{psr_breakdown},
#'   \code{game_log}, and \code{shrinkage_info} components.
#'
#' @export
explain_player_rating <- function(player,
                                   season_val = get_afl_season(type = "current"),
                                   round_val = get_afl_week(type = "next"),
                                   top_n = 15) {

  # Resolve player
  if (grepl("^CD_I", player)) {
    pid <- player
    pname <- player
  } else {
    resolved <- resolve_player(player)
    pid <- resolved$player_id
    pname <- resolved$player_name
  }

  # --- 1. TORP ratings (has EPR + PSR already blended) ---
  tr <- torp_ratings(season_val, round_val)
  player_row <- tr[tr$player_id == pid, ]
  if (nrow(player_row) == 0) {
    cli::cli_abort("Player {.val {pname}} ({pid}) not found in TORP ratings for {season_val} round {round_val}")
  }
  pr <- player_row[1, ]

  cli::cli_h1("{pname} | {pr$team} | TORP Rating Breakdown")
  cli::cli_h2("Overall: TORP {pr$torp} = 0.5 * EPR ({pr$epr}) + 0.5 * PSR ({round(pr$psr, 2)})")

  # --- 2. EPR decomposition ---
  cli::cli_h2("EPR Breakdown (recv + disp + spoil + hitout)")
  epr_cols <- c("recv_epr", "disp_epr", "spoil_epr", "hitout_epr")
  epr_vals <- vapply(epr_cols, function(col) {
    if (col %in% names(pr)) round(as.numeric(pr[[col]]), 2) else NA_real_
  }, numeric(1))
  names(epr_vals) <- gsub("_epr$", "", epr_cols)

  epr_dt <- data.table::data.table(
    component = names(epr_vals),
    value = epr_vals,
    pct_of_epr = round(epr_vals / sum(epr_vals, na.rm = TRUE) * 100, 1)
  )
  print(epr_dt, row.names = FALSE)

  # Shrinkage diagnostics
  shrink_cols <- c("gms", "wt_gms", "wt_tog", "wt_gms_recv")
  shrink_vals <- list()
  for (col in shrink_cols) {
    if (col %in% names(pr)) shrink_vals[[col]] <- round(as.numeric(pr[[col]]), 2)
  }

  cli::cli_h3("Shrinkage context")
  cat(sprintf("  Games: %s | Weighted games: %s | Weighted TOG: %s%%\n",
              shrink_vals$gms %||% "?",
              shrink_vals$wt_gms %||% shrink_vals$wt_gms_recv %||% "?",
              shrink_vals$wt_tog %||% "?"))
  cat(sprintf("  Prior games (shrinkage strength): 3.0 per component\n"))
  cat(sprintf("  Bayesian formula: EPR_i = (loading * sum_i + 3 * prior_rate) / (wt_gms_i + 3)\n"))

  # --- 3. PSR decomposition ---
  cli::cli_h2("PSR Breakdown (stat rating contributions to predicted margin)")

  psr_path <- .find_psr_coef_path()

  psr_breakdown <- NULL
  if (nzchar(psr_path)) {
    coef_df <- utils::read.csv(psr_path)
    coef_df <- coef_df[coef_df$beta != 0, , drop = FALSE]

    # Load player's stat ratings
    skills <- tryCatch(load_player_stat_ratings(season_val), error = function(e) NULL)
    if (!is.null(skills)) {
      skills_dt <- data.table::as.data.table(skills)
      player_skills <- skills_dt[skills_dt$player_id == pid]
      if (nrow(player_skills) > 0) {
        # Take latest round
        player_skills <- player_skills[order(-round)][1]

        skill_cols <- paste0(coef_df$stat_name, "_rating")
        available <- skill_cols %in% names(player_skills)
        if (sum(available) == 0) {
          skill_cols <- paste0(coef_df$stat_name, "_skill")
          available <- skill_cols %in% names(player_skills)
        }

        coef_sub <- coef_df[available, , drop = FALSE]
        skill_cols_avail <- skill_cols[available]

        raw_ratings <- vapply(skill_cols_avail, function(col) {
          v <- as.numeric(player_skills[[col]])
          if (is.na(v)) 0 else v
        }, numeric(1))

        # Apply SD normalisation if present
        if ("sd" %in% names(coef_sub)) {
          sds <- coef_sub$sd
          sds[sds == 0 | is.na(sds)] <- 1
          normalised <- raw_ratings / sds
        } else {
          normalised <- raw_ratings
        }

        contributions <- normalised * coef_sub$beta

        psr_breakdown <- data.table::data.table(
          stat = coef_sub$stat_name,
          rating = round(raw_ratings, 3),
          beta = round(coef_sub$beta, 4),
          contribution = round(contributions, 3)
        )
        data.table::setorderv(psr_breakdown, "contribution", order = -1L)

        cat(sprintf("\n  Top %d PSR contributors (total PSR raw: %.2f):\n", top_n,
                    sum(contributions)))
        print(head(psr_breakdown, top_n), row.names = FALSE)

        if (nrow(psr_breakdown) > top_n) {
          remaining <- sum(psr_breakdown$contribution[(top_n + 1):nrow(psr_breakdown)])
          cat(sprintf("  ... plus %d more stats contributing %.2f\n",
                      nrow(psr_breakdown) - top_n, remaining))
        }
      } else {
        cli::cli_warn("No stat ratings found for this player")
      }
    }
  } else {
    cli::cli_warn("PSR coefficient file not found")
  }

  # --- 4. Recent game log ---
  cli::cli_h2("Recent Game Log (EPV + PSV per game)")
  pgr <- tryCatch(
    load_player_game_ratings(season_val),
    error = function(e) NULL
  )
  game_log <- NULL
  if (!is.null(pgr)) {
    pgr_dt <- data.table::as.data.table(pgr)
    game_log <- pgr_dt[pgr_dt$player_id == pid]
    if (nrow(game_log) > 0) {
      show_cols <- intersect(
        c("season", "round", "team", "opp", "tog", "epv", "recv_epv",
          "disp_epv", "spoil_epv", "hitout_epv", "psv", "osv", "dsv",
          "torp_value"),
        names(game_log)
      )
      game_log <- game_log[order(-season, -round)]
      print(head(game_log[, show_cols, with = FALSE], 10), row.names = FALSE)
    }
  }

  # --- 5. Visual: percentile among all players ---
  cli::cli_h2("Percentile Ranks (vs all players)")
  pctile_metrics <- c("torp", "epr", "psr", "osr", "dsr",
                       "recv_epr", "disp_epr", "spoil_epr", "hitout_epr")
  pctile_metrics <- intersect(pctile_metrics, names(tr))
  pctiles <- vapply(pctile_metrics, function(col) {
    round(mean(tr[[col]] <= as.numeric(pr[[col]]), na.rm = TRUE) * 100)
  }, numeric(1))

  pctile_dt <- data.table::data.table(
    metric = names(pctiles),
    value = vapply(pctile_metrics, function(col) round(as.numeric(pr[[col]]), 2), numeric(1)),
    percentile = pctiles
  )
  print(pctile_dt, row.names = FALSE)

  # Plot if interactive
  if (interactive() && requireNamespace("graphics", quietly = TRUE)) {
    cols <- ifelse(pctile_dt$percentile >= 90, "#2166ac",
              ifelse(pctile_dt$percentile >= 70, "#67a9cf",
                ifelse(pctile_dt$percentile >= 30, "#d1e5f0",
                  ifelse(pctile_dt$percentile >= 10, "#fddbc7", "#b2182b"))))
    op <- graphics::par(mar = c(4, 8, 3, 1))
    on.exit(graphics::par(op), add = TRUE)
    bp <- graphics::barplot(
      pctile_dt$percentile,
      names.arg = pctile_dt$metric,
      horiz = TRUE, las = 1,
      col = cols, border = NA,
      xlim = c(0, 100),
      main = paste0(pname, " - Percentile Ranks"),
      xlab = "Percentile"
    )
    graphics::abline(v = 50, lty = 2, col = "grey40")
    graphics::text(pctile_dt$percentile + 3, bp, labels = paste0(pctile_dt$percentile, "%"),
                   cex = 0.8, adj = 0)
  }

  invisible(list(
    player_row = pr,
    epr_breakdown = epr_dt,
    psr_breakdown = psr_breakdown,
    game_log = game_log,
    percentiles = pctile_dt,
    shrinkage_info = shrink_vals
  ))
}


#' Load PSR Coefficient Files and Compute Components
#'
#' Convenience wrapper that loads the margin, offensive, and defensive
#' coefficient CSVs from \code{inst/extdata} and calls
#' \code{\link{calculate_psr_components}}.
#'
#' @inheritParams calculate_psr
#' @param psr_coef_path Path to the margin PSR coefficient CSV. If NULL,
#'   searches \code{inst/extdata/psr_coefficients.csv}.
#'
#' @return A data.table with \code{psr}, \code{osr}, \code{dsr} columns.
#' @keywords internal
.compute_psr_from_stat_ratings <- function(skills, psr_coef_path = NULL, center = TRUE) {
  # Resolve margin coefficient path

  if (is.null(psr_coef_path)) {
    psr_coef_path <- .find_psr_coef_path()
  }

  if (!nzchar(psr_coef_path) || !file.exists(psr_coef_path)) {
    cli::cli_warn("PSR coefficient file not found: {psr_coef_path}")
    return(NULL)
  }

  coef_df <- utils::read.csv(psr_coef_path)

  # Try to find osr/dsr coefficient files in the same directory
  coef_dir <- dirname(psr_coef_path)
  osr_path <- file.path(coef_dir, "osr_coefficients.csv")
  dsr_path <- file.path(coef_dir, "dsr_coefficients.csv")

  if (file.exists(osr_path) && file.exists(dsr_path)) {
    osr_coef_df <- utils::read.csv(osr_path)
    dsr_coef_df <- utils::read.csv(dsr_path)
    calculate_psr_components(skills, coef_df, osr_coef_df, dsr_coef_df, center = center)
  } else {
    cli::cli_inform("OSR/DSR coefficient files not found -- computing PSR only (no osr/dsr decomposition)")
    calculate_psr(skills, coef_df, center = center)
  }
}
