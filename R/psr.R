# Player Skill Rating (PSR)
# =========================
# Predict match margin from team-aggregated player skills via glmnet,
# then apportion coefficients back to individual players.
# PSR = "predicted margin contribution points" above league average.


#' Calculate Player Skill Ratings (PSR)
#'
#' Computes PSR for each player-round by applying glmnet coefficients to
#' individual player skill values. PSR represents each player's predicted
#' contribution to match margin based on their skill profile.
#'
#' @param skills A data.table/data.frame from \code{load_player_skills()},
#'   containing \code{player_id}, \code{player_name}, \code{season},
#'   \code{round}, \code{pos_group}, and \code{*_skill} columns.
#' @param coef_df A data.frame with columns \code{stat_name} and \code{beta},
#'   as produced by the PSR training script. If an \code{sd} column is present,
#'   each skill is divided by its SD before multiplying by beta (i.e. the
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

  # Map stat_name to skill column name
  skill_cols <- paste0(coef_df$stat_name, "_skill")
  available <- skill_cols %in% names(dt)

  if (sum(available) == 0) {
    cli::cli_abort("No matching skill columns found in data")
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


#' Load PSR Coefficient Files and Compute Components
#'
#' Convenience wrapper that loads the margin, offensive, and defensive
#' coefficient CSVs from \code{inst/extdata} (or a fallback path) and
#' calls \code{\link{calculate_psr_components}}.
#'
#' @inheritParams calculate_psr
#' @param psr_coef_path Path to the margin PSR coefficient CSV. If NULL,
#'   searches \code{inst/extdata/psr_v2_coefficients.csv}.
#'
#' @return A data.table with \code{psr}, \code{osr}, \code{dsr} columns,
#'   or the result of \code{calculate_psr()} (without osr/dsr) if the
#'   offensive/defensive coefficient files are not found.
#'
#' @keywords internal
.compute_psr_from_skills <- function(skills, psr_coef_path = NULL, center = TRUE) {
  # Resolve margin coefficient path

  if (is.null(psr_coef_path)) {
    psr_coef_path <- system.file("extdata", "psr_v2_coefficients.csv", package = "torp")
    if (psr_coef_path == "") {
      psr_coef_path <- file.path(
        find.package("torp", quiet = TRUE)[1] %||% ".",
        "data-raw", "cache-skills", "psr_v2_coefficients.csv"
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
  osr_path <- file.path(coef_dir, "osr_v2_coefficients.csv")
  dsr_path <- file.path(coef_dir, "dsr_v2_coefficients.csv")

  if (file.exists(osr_path) && file.exists(dsr_path)) {
    osr_coef_df <- utils::read.csv(osr_path)
    dsr_coef_df <- utils::read.csv(dsr_path)
    calculate_psr_components(skills, coef_df, osr_coef_df, dsr_coef_df, center = center)
  } else {
    cli::cli_inform("OSR/DSR coefficient files not found -- computing PSR only (no osr/dsr decomposition)")
    calculate_psr(skills, coef_df, center = center)
  }
}
