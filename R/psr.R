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
