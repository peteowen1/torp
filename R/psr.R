# Player Skill Rating (PSR)
# =========================
# Predict match margin from team-aggregated player skills via glmnet,
# then apportion coefficients back to individual players.
# PSR = "predicted margin contribution points" above league average.


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

  # Center
  if (center) {
    league_mean <- mean(dt$psr_raw, na.rm = TRUE)
    dt[, psr := psr_raw - league_mean]
  } else {
    dt[, psr := psr_raw]
  }

  id_cols <- intersect(
    c("player_id", "player_name", "season", "round", "pos_group",
      "team", "n_games", "wt_games"),
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

  # Exclude stats that don't belong in per-game PSV:
  #  - Efficiency %s (redundant — numerator + denominator already in as rate stats)
  #  - bounces (negative coefficient, not causal)
  #  - cond_tog, squad_selection (availability metrics, not performance)
  psv_exclude <- c("disposal_efficiency", "goal_accuracy", "contested_poss_rate",
                    "hitout_win_pct", "kick_efficiency", "bounces",
                    "cond_tog", "squad_selection")
  keep <- !stat_cols %in% psv_exclude
  coef_df <- coef_df[keep, , drop = FALSE]
  stat_cols <- stat_cols[keep]
  betas <- coef_df$beta

  # Extract raw stat values
  mat <- as.matrix(dt[, stat_cols, with = FALSE])
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

  if (center) {
    # Center within each round (so PSV = contribution above average that round)
    group_cols <- intersect(c("season", "round"), names(dt))
    if (length(group_cols) > 0) {
      dt[, psv := psv_raw - mean(psv_raw, na.rm = TRUE), by = group_cols]
    } else {
      dt[, psv := psv_raw - mean(psv_raw, na.rm = TRUE)]
    }
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
    psr_coef_path <- system.file("extdata", "psr_coefficients.csv", package = "torp")
    if (psr_coef_path == "") {
      psr_coef_path <- file.path(
        find.package("torp", quiet = TRUE)[1] %||% ".",
        "data-raw", "cache-stat-ratings", "psr_coefficients.csv"
      )
    }
  }

  if (!file.exists(psr_coef_path)) {
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
    psr_coef_path <- system.file("extdata", "psr_coefficients.csv", package = "torp")
    if (psr_coef_path == "") {
      psr_coef_path <- file.path(
        find.package("torp", quiet = TRUE)[1] %||% ".",
        "data-raw", "cache-stat-ratings", "psr_coefficients.csv"
      )
    }
  }

  if (!file.exists(psr_coef_path)) {
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
